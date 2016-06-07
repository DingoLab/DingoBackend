



% src/Dindo/Database/Pool.lhs

\begin{code}
module Dindo.Database.Pool where
      import Data.Pool
      import Databaste.PostgreSQL.Simple as PG
      import Control.Monad.IO.Class
\end{code}

连接
\begin{code}
      type PgC = PG.Connection
      type PgCP = Pool PgC
      type PgCPA b = CPA PgC b
      newtype CPA c b = CPA
        { unCPA :: c -> IO b
        }
\end{code}

\begin{code}
      pgConnectionPool :: ByteString -> Int -> IO PgCP
      pgConnectionPool cstr limit = createPool
        (connectionPostgreSQL cstr)
        PG.close
        (div limit 2)
        60
        limit
\end{code}

\begin{code}
      getC :: CPA c c
      getC = CPA $ \c -> return c
      runCPA :: CPA c b -> Pool c  -> IO b
      runCPA (CPA f) p = withResource p f

      instance Functor (CPA cp) where
        fmap f ( i) = CPA $ \rd-> do
          x <- i rd
          return $ f x

      instance Applicative (CPA cp) where
        pure a = CPA $ \_ -> return a
        (CPA i) <*> (CPA j) = CPA $ \rd -> do
          f <- i rd
          x <- j rd
          return $ f x

      instance Monad (CPA cp) where
        return i = CPA $ \_ -> return i
        (CPA i) >>= f = CPA $ \rd -> do
          x <- i rd
          let (CPA y) = f x
          y rd

      instance MonadIO (CPA cp) where
        liftIO i = CPA $ \_ -> i
\end{code}

\begin{code}
      runPgCP :: PgCPA b ->  PgCP -> > IO b
      runPgCP = runCPA
\end{code}
