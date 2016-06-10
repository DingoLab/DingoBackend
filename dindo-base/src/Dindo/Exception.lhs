




% src/Dindo/Exception.lhs

\begin{code}
module Dindo.Exception
    ( BaseError(..)
    , ErrorError(..)
    , ScError(..)
    , baseError
    , invalidArgs
    , invalidAccept
    , invalidHeaders
    , tryRIO
    , catchRIO
    , module X
    ) where

      import Control.Exception as X
      import Data.Typeable
      import Dindo.RIO
\end{code}

\begin{code}
      data BaseError = BaseError
        { typ :: String
        , context :: String
        } deriving (Show,Typeable)
      data ErrorError = ErrorError
        { etyp :: String
        , econtext :: SomeException
        } deriving (Show,Typeable)
      instance Exception ErrorError where
        toException = SomeException

      data ScError = ScError String
        deriving (Show,Typeable)

      instance Exception ScError where
        toException = SomeException

      instance Exception BaseError where
        toException = SomeException

      baseError t = throw.BaseError t
      invalidArgs = baseError "invalid args"
      invalidHeaders = baseError "invalid headers"
      invalidAccept = baseError "invalid accept mime-type"
\end{code}

\begin{code}
      tryRIO :: Exception e => RIO cfg a -> RIO cfg (Either e a)
      tryRIO (RIO i) = RIO $ \rd -> try $ i rd
      catchRIO :: Exception e => RIO cfg a -> (e -> RIO cfg a) -> RIO cfg a
      catchRIO (RIO i) f = RIO $ \rd -> catch (i rd) (\e -> runRIO rd (f e))
\end{code}
