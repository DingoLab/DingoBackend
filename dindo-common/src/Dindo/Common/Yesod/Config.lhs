




% src/Dindo/Common/Yesod/Config.lhs

\begin{code}
{-# LANGUAGE RecordWildCards
           , OverloadedStrings
           #-}
\end{code}

\begin{code}
module Dindo.Common.Yesod.Config
    ( SvrConfig(..)
    , DbConfig(..)
    , dbConfig2Str
    ) where
\end{code}

\begin{code}
      import Data.Yaml
      import Data.ByteString as B
      import Data.ByteString.Lazy
      import Data.String
\end{code}


通用 配置。
svrPort 后端端口
svrDb 数据库配置
dbAddr 数据库地址
dbPort 数据库端口
dbUser 数据库用户名
dbName 数据库名称
dbPsk  数据库用户与密码
dbConThd 链接数设置

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
        , dbPsk  :: String
        , dbConThd ::  Int
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
          , "user" .= dbUser
          , "name" .= dbName
          , "con-limit" .= dbConThd
          , "password" .= dbPsk
          ]
      instance FromJSON SvrConfig where
        parseJSON (Object v) = SvrConfig
          <$> v .: "port"
          <*> v .: "database-config"
      instance FromJSON DbConfig where
        parseJSON (Object v) =DbConfig
          <$> v .: "addr"
          <*> v .: "port"
          <*> v .: "user"
          <*> v .: "name"
          <*> v .: "password"
          <*> v .: "con-limit"
      dbConfig2Str :: DbConfig -> (B.ByteString,Int)
      dbConfig2Str DbConfig{..} = (str,dbConThd)
        where
          str = toStrict $
            fromString $    "host=\'" ++ dbAddr
                      ++ "\' port=\'" ++ dbPort
                      ++ "\' user=\'" ++ dbUser
                      ++ "\' password=\'" ++ dbPsk
                      ++ "\' dbname=\'" ++ dbName
                      ++ "\'"
\end{code}

JSON 与 Yaml 例程
\begin{json}
{ " 
}
\end{json}
\begin{yaml}
\end{yaml}
