




% src/Dindo/Config.lhs

\begin{code}
module Dindo.Config
    ( Launchble(..)
    ) where

      import Dindo.Import.ByteString hiding (putStrLn)
      import Dindo.Import.Aeson
      import Network.Wai.Handler.Warp hiding (getPort)
      import System.Exit
      import System.Signal
      import Data.String(fromString)
\end{code}

\begin{code}
      class FromJSON a => Launchble a where
        type Rditem a
        getPort :: a -> Int
        getTimeOut :: a -> Int
        getServerName :: a -> ByteString
        getLogPath :: a -> String
        toRdCfg :: a -> IO (Rditem a)
        getListenType :: a -> String
        shutDown :: a ->  IO () -> IO ()
        shutDown _ = defShutDown
        setFromL :: a -> Settings -> Settings
        setFromL l = setInstallShutdownHandler (shutDown l).setPort (getPort l).setHost (fromString (getListenType l)) .setTimeout (getTimeOut l).setServerName (getServerName l)

      defShutDown f = do
        installHandler sigINT $ \ sig -> do
          if sig == sigINT
            then do
              f
              putStrLn "\nGoing to turn down"
              exitSuccess
            else putStrLn $ "catch" ++ show sig
\end{code}
