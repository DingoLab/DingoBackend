




% src/Dindo/Database.lhs

\begin{code}
module Dindo.Database
    ( module X
    , module PG
    ) where

      import Dindo.Database.Pool as X
      import Dindo.Database.Dbable as X
      import Database.PostgreSQL.Simple as PG
      import Dindo.Exception
      import Dindo.RIO
      import Data.Int(Int64)
\end{code}

Sql 查询

事务
\begin{code}
      beginPgT :: PgCPA ()
      beginPgT = CPA $ \c -> PG.begin c
      commitPgT :: PgCPA ()
      commitPgT = CPA $ \c -> PG.commit c
      rollbackPgT :: PgCPA ()
      rollbackPgT = CPA $ \c -> PG.rollback c
\end{code}

PostgreSQL 的查询

Query 函数
\begin{code}
      queryPg :: (PG.ToRow q,FromRow r)
              => PG.Query -> q -> CPA [r]
      queryPg q qq = CPA $ \c -> query c q qq
      queryPg_ :: PG.FromRow r
              => PG.Query -> CPA [r]
      queryPg_ q = CPA $ \c -> query_ c q
\end{code}
可能返回的异常：
\begin{description}
  \item[FormatError] 查询字符串格式有误
  \item[QueryError] 返回为空 （或许应该使用 executePg）
  \item[ResultError] 返回值转换有误
  \item[SqlError] Pg 返回的错误。
\end{description}
Execute 函数
\begin{code}
      executePg :: PG.ToRow q
                => Query -> q -> RIO Int64
      executePg q qq = CPA $ \c -> execute c q qq
      executePg_ :: Query -> RIO Int64
      executePg_ q = CPA $ \c -> execute_ c q
\end{code}


最基本的查询，没有事务与异常
\begin{code}
      runPg :: PgSql cfg
            => PgCPA b -> RIO cfg b
      runPg action = (liftIO.runPgCP action) =<< getCP
\end{code}
带有异常捕获但是没有事务
\begin{code}
      tryRunPg :: (PgSql cfg,Exception e)
               => PgCPA b -> RIO cfg (Either e b)
      tryRunPg action = catchRIO
        (runPg action >>= (return.Right))
        (\e -> return $ Left e)
\end{code}
带有事务，不捕获异常（捕获之后再抛出）
\begin{code}
      runPgT :: PgSql cfg
             => PgCPA b -> RIO cfg b
      runPgT action' = catchRIO
        (runPg action)
        (\e -> runPg rollbackPgT >> throw e)
        where
          action = do
            beginPgT
            action'
            commitPgT
\end{code}
带事务，返回异常
\begin{code}
      tryRunPgT :: (PgSql cfg,Exception e)
                => PgCPA b -> RIO cfg (Either e b)
      tryRunPgT action' = catchRIO
        (runPg action >>= (return.Right))
        (\e -> runPg rollbackPgT >> (return.Left e))
        where
          action = do
            beginPgT
            action'
            commitPgT
\end{code}
