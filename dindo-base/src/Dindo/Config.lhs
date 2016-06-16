




% src/Dindo/Config.lhs

\begin{code}
module Dindo.Config
    (
    ) where

      import Dindo.Import.Wai hiding (getPort)
      import Dindo.Import.Aeson
      import Dindo.Import.ByteString (ByteString)
      import Data.String (fromString)
      import System.Exit
      import System.Signal
\end{code}


\begin{code}
      class FromJSON a => CfgF a where
        type CfgD a
        getPort :: a -> Port
        getLogPath :: a -> LogPath
        getTimeout :: a -> Int
        getServerName :: a -> ByteString
        toCfgD :: a -> IO (CfgD a)
        getListenType :: a -> String
        beginMicro :: a -> IO Bool
        heartBeat :: a -> IO ()
        endMicro :: a -> IO ()
        shutDown :: a -> IO () -> IO ()
        shutDown = defShutDown
        setSettings :: a -> Settings -> Settings
        setSettings = defSetSettings

      defShutDown l f = do
        installHandler sigINT $ \ sig -> do
          if sig == sigINT
            then do
              f
              endMicro l
              putStrLn "\nGoing to turn down"
              exitSuccess
            else putStrLn $ "catch" ++ show sig
      defSetSettings l = setInstallShutdownHandler (shutDown l)
        . setPort (getPort l)
        . setHost (fromString (getListenType l))
        . setTimeout (getTimeOut l)
        . setServerName (getServerName l)
\end{code}

\begin{code}
      data LogPath = LogStdout
                   | LogStderr
                   | LogFile FilePath
\end{code}
