




% src/Dindo/Database/Pg.lhs

\begin{code}
module Dindo.Database.Pg
    ( module PG
    , pgQuery
    , toPgCP
    , PgC(..),PgCP(..),PgCPA(..),PgCI(..)
    , PgSql(..)
    , runPg,runPgT,tryRunPg,tryRunPgT
    , beginPgT,commitPgT,rollbackPgT
    , toPgQuery
    , PgQuery(..)
    , queryPg,queryPg_
    , executePg,executePg_
    ) where

      import Dindo.Import.Aeson
      import Dindo.Import.Yesod
      import qualified Dindo.Import.ByteString as B

      import Dindo.Exception
      import Dindo.Database.Internal

      import Control.Monad.IO.Class
      import Data.Pool
      import Data.Int(Int64)
      import Data.Time
      import qualified Dindo.Import.Pg as PG
\end{code}

\begin{code}
      pgQuery = PG.sql
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
        (PG.connect x)
        PG.close

\end{code}

\begin{code}
      instance FromJSON PgCI where
        parseJSON (Object v) = PG.ConnectInfo
          <$> v .: "host"
          <*> v .: "port"
          <*> v .: "user"
          <*> v .: "password"
          <*> v .: "dbname"
\end{code}

\begin{code}
      class PgSql a where
        getPgCP :: HandlerT a IO PgCP
        runPgCPA :: PgCPA b -> HandlerT a IO b
        runPgCPA = defRunPgCP

      defRunPgCP :: PgSql a
                 => PgCPA b -> HandlerT a IO b
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
      runPg :: PgSql cfg
            => PgCPA b -> HandlerT cfg IO b
      runPg = runPgCPA
\end{code}
带有异常捕获但是没有事务
\begin{code}
      tryRunPg :: (PgSql cfg,Exception e)
               => PgCPA b -> HandlerT cfg IO (Either e b)
      tryRunPg action = catchH
        (runPg action >>= (return.Right))
        (\e -> return $ Left e)
\end{code}
带有事务，不捕获异常（捕获之后再抛出）
\begin{code}
      runPgT :: PgSql cfg
             => PgCPA b -> HandlerT cfg IO b
      runPgT action' = catchH (runPg action) te
        where
          te :: PgSql cfg => SomeException -> HandlerT cfg IO b
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
      tryRunPgT :: (PgSql cfg,Exception e)
                => PgCPA b -> HandlerT cfg IO (Either e b)
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
      toPgQuery = PG.Query
\end{code}

查询


PostgreSQL 的查询

Query 函数
\begin{code}
      queryPg :: (PG.ToRow q,PG.FromRow r)
              => PgQuery -> q -> PgCPA [r]
      queryPg q qq = CPA $ \c -> PG.query c q qq
      queryPg_ :: PG.FromRow r
              => PgQuery -> PgCPA [r]
      queryPg_ q = CPA $ \c -> PG.query_ c q
\end{code}


Execute 函数
\begin{code}
      executePg :: PG.ToRow q
                => PgQuery -> q -> PgCPA Int64
      executePg q qq = CPA $ \c -> PG.execute c q qq
      executePg_ :: PgQuery -> PgCPA Int64
      executePg_ q = CPA $ \c -> PG.execute_ c q
\end{code}
