




% src/Dindo/Config.lhs

\begin{code}
module Dindo.Config
    ( Launchble(..)
    ) where

      import Dindo.Import.ByteString
      import Dindo.Import.Aeson
\end{code}

\begin{code}
      class FromJSON a => Launchble a where
        getPort :: a -> Int
        getTimeOut :: a -> Int
        getServerName :: a -> ByteString
        getLogPath :: a -> String
        toRdCfg :: a -> IO cfg
\end{code}
