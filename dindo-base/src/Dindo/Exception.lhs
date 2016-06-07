




% src/Dindo/Exception.lhs

\begin{code}
module Dindo.Exception
    ( BaseError(..)
    , baseError
    , invalidArgs
    , invalidAccept
    , invalidHeaders
    ) where

      import Control.Exception as Dindo.Exception
      import Data.Typeable
      import Dindo.RIO

      data BaseError = BaseError
        { typ :: String
        , context :: String
        } deriving (Show,Typeable)
      data ErrorError = ErrorError
        { etyp :: String
        , econtext :: SomeException
        }

      instance Exception BaseError where
        toException = SomeException

      baseError t = throw.BaseError t
      invalidArgs = baseError "invalid args"
      invalidHeaders = baseError "invalid headers"
      invalidAccept = baseError "invalid accept mime-type"
\end{code}

\begin{code}
      tryRIO :: Exception e => RIO a -> RIO (Either e a)
      tryRIO (RIO i) = RIO $ \rd -> try i
      catchRIO :: Exception e => RIO a -> (e -> RIO a) -> RIO a
      catchRIO (RIO i) f = RIO $ \rd -> catch i (\e runRIO rd (f e))
\end{code}
