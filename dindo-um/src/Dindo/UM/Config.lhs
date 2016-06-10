




% src/Dindo/UM/Config.lhs


\begin{code}
module Dindo.UM.Config
    ( UMConfig(..)
    , UMDbConfig(..)
    , UMRdConfig(..)
    ) where

      import Dindo.Config
      import Dindo.RIO
      import Dindo.Database.Pg
      import Dindo.Import.Aeson
\end{code}

\begin{code}
      data UMConfig = UMConfig
        { umcPort :: Int
        , umcTimeOut :: Int
        , umcLogPath :: String
        , umcDb :: UMDbConfig
        }
      data UMDbConfig = UMDbConfig
        { umdPort :: Int
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
      instance FromJSON UMDbConfig where
        parseJSON (Object v) = UMDbConfig
          <$> v .: "port"
\end{code}


\begin{code}
      instance PgSql UMRdConfig where
        getCP = RIO $ return.umrPgCP.rdConfig
\end{code}

\begin{code}
      instance Launchble UMConfig where
        getPort = umcPort
        getLogPath = umcLogPath
        getTimeOut = umcTimeOut
        getServerName _ = "dindo"
        toRdCfg cfg = undefined
\end{code}
