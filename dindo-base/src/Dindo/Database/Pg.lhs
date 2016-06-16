




% src/Dindo/Database/Pg.lhs

\begin{code}
module Dindo.Database.Pg
    (
    ) where

      import Dindo.Import.Aeson
      import qualified Dindo.Import.ByteString as B

      import Dindo.Exception
      import Dindo.Database.Internal

      import Control.Monad.Status
      import Data.Pool
      import Data.Int(Int64)
      import Data.Time
      import qualified Database.PostgreSQL.Simple as PG
      import qualified Database.PostgreSQL.Simple.Types as PG
      import qualified Database.PostgreSQL.Simple.SqlQQ as PG
\end{code}

\begin{code}
      pgQuery = sql
\end{code}

\begin{code}
      type PgC = PG.Connection
      type PgCP = Pool PgC
      type PgCPA b = CPA PgC b
      type PgCI = PG.ConnectInfo
\end{code}

\begin{code}
      toPgCP :: PgCI -> Int -> NominalDiffTime
             -> Int ->  IO PgCP
      toPgCP x = createPool
        (connect x)
        close

\end{code}

\begin{code}
      instance FromJSON PgCI where
        parseJSON (Object v) = PgCI
          <$> v .: "host"
          <*> v .: "port"
          <*> v .: "user"
          <*> v .: "password"
          <*> v .: "dbname"
\end{code}

\begin{code}
      class PgSql a where
        getPgCP :: MonadIO m => HandlerT a m PgCP
        runPgCPA :: MonadIO m => PgCPA b -> HandlerT a m b
        runPgCPA = defRunPgCP

      defRunPgCP :: (PgSql a,MonadIO m)
                 => PgCPA b -> HandlerT a m b
      defRunPgCP (CPA fm) = do
        cp <- getPgCP
        liftIO $ withResource cp fm
\end{code}



可能返回的异常：
\begin{description}
  \item[FormatError] 查询字符串格式有误
  \item[QueryError] 返回为空 （或许应该使用 executePg）
  \item[ResultError] 返回值转换有误
  \item[SqlError] Pg 返回的错误。
\end{description}
最基本的查询，没有事务与异常
\begin{code}
      runPg :: (PgSql cfg,MonadIO m)
            => PgCPA b -> HandlerT cfg m b
      runPg = runPgCPA
\end{code}
带有异常捕获但是没有事务
\begin{code}
      tryRunPg :: (PgSql cfg,MonadIO m,Exception e)
               => PgCPA b -> HandlerT cfg m (Either e b)
      tryRunPg action = catchH
        (runPg action >>= (return.Right))
        (\e -> return $ Left e)
\end{code}
带有事务，不捕获异常（捕获之后再抛出）
\begin{code}
      runPgT :: (PgSql cfg,MonadIO m)
             => PgCPA b -> HandlerT cfg m b
      runPgT action' = catchH (runPg action) te
        where
          te :: (PgSql cfg,MonadIO m) => SomeException -> HandlerT cfg m b
          te e = do
            runPg rollbackPgT
            throw e
          action = do
            beginPgT
            rt <- action'
            commitPgT
            return rt
\end{code}
带事务，返回异常
\begin{code}
      tryRunPgT :: (PgSql cfg,Exception e,MonadIO m)
                => PgCPA b -> HandlerT cfg m (Either e b)
      tryRunPgT action' = catchH rv le
        where
          rv = fmap Right $ runPg action
          le e = do
            runPg rollbackPgT
            return $ Left e
          action = do
            beginPgT
            rt <- action'
            commitPgT
            return rt
\end{code}


事务
\begin{code}
      beginPgT :: PgCPA ()
      beginPgT = CPA PG.begin
      commitPgT :: PgCPA ()
      commitPgT = CPA PG.commit
      rollbackPgT :: PgCPA ()
      rollbackPgT = CPA PG.rollback
\end{code}

\begin{code}
      type PgQuery = PG.Query
      toQuery = PG.Query
\end{code}

查询


PostgreSQL 的查询

Query 函数
\begin{code}
      queryPg :: (PG.ToRow q,FromRow r)
              => PgQuery -> q -> PgCPA [r]
      queryPg q qq = CPA $ \c -> query c q qq
      queryPg_ :: PG.FromRow r
              => PgQuery -> PgCPA [r]
      queryPg_ q = CPA $ \c -> query_ c q
\end{code}


Execute 函数
\begin{code}
      executePg :: PG.ToRow q
                => PgQuery -> q -> PgCPA Int64
      executePg q qq = CPA $ \c -> execute c q qq
      executePg_ :: PgQuery -> PgCPA Int64
      executePg_ q = CPA $ \c -> execute_ c q
\end{code}
