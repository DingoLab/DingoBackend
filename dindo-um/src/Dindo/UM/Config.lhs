




% src/Dindo/UM/Config.lhs


\begin{code}
module Dindo.UM.Config
    ( UMConfig(..)
    , UMDbConfig(..)
    , UMRdConfig(..)
    ) where

      import Dindo.Config
      import Dindo.Import.Pool
      import Dindo.RIO
      import Dindo.Database
      import Dindo.Database.Pg
      import Dindo.Import.Aeson
\end{code}

\begin{code}
      data UMConfig = UMConfig
        { umcPort :: Int
        , umcTimeOut :: Int
        , umcLogPath :: String
        , umcDb :: UMDbConfig
        , umcListen :: String
        }
      data UMDbConfig = UMDbConfig
        { umdPort :: Int
        , umdHost :: String
        , umdUser :: String
        , umdPwd  :: String
        , umdDb   :: String
        , umdSP   :: Int
        , umdKT   :: Int
        , umdMax  :: Int
        }
\end{code}
\begin{code}
      data UMRdConfig = UMRdConfig
        { umrPgCP :: PgCP
        }
\end{code}

\begin{code}
      instance FromJSON UMConfig where
        parseJSON (Object v) = UMConfig
          <$> v .: "port"
          <*> v .: "timeout"
          <*> v .: "log-path"
          <*> v .: "database"
          <*> v .: "listen-type"
      instance FromJSON UMDbConfig where
        parseJSON (Object v) = UMDbConfig
          <$> v .: "port"
          <*> v .: "host"
          <*> v .: "user"
          <*> v .: "pwd"
          <*> v .: "db"
          <*> v .: "stripes"
          <*> v .: "keep-time"
          <*> v .: "max-connection"
\end{code}


\begin{code}
      instance PgSql UMRdConfig where
        getCP = RIO $ return.umrPgCP.rdConfig
\end{code}

\begin{code}
      instance Launchble UMConfig where
        type Rditem UMConfig = UMRdConfig
        getPort = umcPort
        getLogPath = umcLogPath
        getTimeOut = umcTimeOut
        getServerName _ = "dindo"
        getListenType = umcListen
        toRdCfg cfg = do
          let UMDbConfig{..} = umcDb cfg
          let ci = ConnectInfo umdHost (fromIntegral umdPort) umdUser umdPwd umdDb
          pool <- createPool (connect ci)
                             close
                             umdSP (fromIntegral umdKT) umdMax
          return $ UMRdConfig pool

\end{code}
