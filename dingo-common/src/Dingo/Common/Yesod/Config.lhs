




% src/Dingo/Common/Yesod/Config.lhs

\begin{code}
\end{code}

\begin{code}
module Dingo.Common.Yesod.Config
    ( SvrConfig(..)
    , DbConfig(..)
    ) where
\end{code}

\begin{code}
      import Data.Yaml
      import Data.ByteString as B
\end{code}


通用 配置。
\begin{code}
      data SvrConfig = SvrConfig
        { svrPort :: Int
        , svrDb :: DbConfig
        }
      data DbConfig = DbConfig
        { dbAddr :: String
        , dbPort :: String
        , dbUser :: String
        , dbName :: String
        , dbConThd :: Int
        }
\end{code}

配置实现 ToJSON 与 FromJSON 类型类
\begin{code}
      instance ToJSON SvrConfig where
        toJSON SvrConfig{..} = object
          [ "port" .= svrPort
          , "datebase-bconfig" .= svrDb
          ]
      instance ToJSON DbConfig where
        toJSON DbConfig{..} = object
          [ "addr" .= dbAddr
          , "port" .= dbPort
          , "name" .= dbName
          , "con-limit" .= dbConThd
          ]
      instance FromJSON SvrConfig where
        parseJSON (Object v) = SvrConfig
          <$> v .: "port"
          <*> v .: "database-config"
      instance FromJSON DbConfig where
        parseJSON (Object v) =DbConfig
          <$> v .: "addr"
          <*> v .: "port"
          <*> v .: "name"
          <*> v .: "con-limit"
      dbConfig2Str :: DbConfig -> (B.ByteString,Int)
      dbConfig2Str DbConfig{..} = (str,dbConThd)
        where
          str = toStrict $
            fromString $    "host=\'" ++ dbAddr
                      ++ "\' port=\'" ++ dbPort
                      ++ "\' user=\'" ++ dbPsk
                      ++ "\' password=\'" ++ dbPsk
                      ++ "\' dbname=\'" ++ dbName
                      ++ "\'"
\end{code}
