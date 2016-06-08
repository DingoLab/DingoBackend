




% src/Dindo/RIO.lhs

\begin{code}
module Dindo.RIO
    ( RIO(..)
    , RIOM(..)
    , RD(..)
    , runRIO
    , getQuerys
    , getHeaders
    , getRequest
    , getConfig
    , getLogger
    , liftIO
    ) where

      import Dindo.Import
      import Network.HTTP.Types
      import Control.Monad.IO.Class
      import System.Log.FastLogger
\end{code}


带有信息
\begin{code}
      data RD cfg = RD
        { rdRequest :: Request
        , rdConfig :: cfg
        , rdLogger :: LoggerSet
        }
\end{code}

带有 Request 等信息的 Monad （IO）
\begin{code}
      getRequest :: RIO cfg Request
      getRequest = RIO $ return.rdRequest
      getConfig  :: RIO cfg cfg
      getConfig = RIO $ return.rdConfig
      getHeaders :: RIO cfg [Header]
      getHeaders = (return.requestHeaders) =<< getRequest
      getQuerys :: RIO cfg Query
      getQuerys = (return.queryString) =<< getRequest

      getLogger :: RIO cfg LoggerSet
      getLogger = RIO $ return.rdLogger
\end{code}
\begin{code}
      runRIO rd (RIO i) = i rd

      newtype RIO cfg a = RIO
        { unRIO :: RD cfg -> IO a
        }

      type RIOM cfg a = RIO cfg (Maybe a)

      instance Functor (RIO cfg) where
        fmap f (RIO i) = RIO $ \rd-> do
          x <- i rd
          return $ f x

      instance Applicative (RIO cfg) where
        pure a = RIO $ \_ -> return a
        (RIO i) <*> (RIO j) = RIO $ \rd -> do
          f <- i rd
          x <- j rd
          return $ f x

      instance Monad (RIO cfg) where
        return i = RIO $ \_ -> return i
        (RIO i) >>= f = RIO $ \rd -> do
          x <- i rd
          let (RIO y) = f x
          y rd

      instance MonadIO (RIO cfg) where
        liftIO i = RIO $ \_ -> i
\end{code}
