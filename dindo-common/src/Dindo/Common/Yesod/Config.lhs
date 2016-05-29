




% src/Dindo/Common/Yesod/Config.lhs


\begin{code}
module Dindo.Common.Yesod.Config
    ( SvrConfig(..)
    , DbConfig(..)
    , ScError(..)
    ,scError
    , dbConfig2Str
    ) where
\end{code}

\begin{code}
      import Data.Yaml
      import Data.ByteString as B
      import Data.ByteString.Lazy
      import Data.String
      import Control.Exception
\end{code}


模块配置与数据库链接配置。
\begin{description}
  \item[svrPost] 后端侦听端口
  \item[svrDb] 后端的数据库配置（由下面的项组成）
  \item[dbAddr] 数据库的地址（ip／域名，不包含端口）
  \item[dbPort] 数据库侦听的端口
  \item[dbUser] 链接数据库的用户名
  \item[dbName] 链接的数据库
  \item[dbPsk] 链接的密码
  \item[ConThd] 连接数限制
\end{description}

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

将模块配置与数据库连接设置实现 ToJSON 与 FromJSON 类型类，以供数据转换为JSON 与 YAML。
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
        parseJSON _ = throw $ ScError "Invailed"
      instance FromJSON DbConfig where
        parseJSON (Object v) =DbConfig
          <$> v .: "addr"
          <*> v .: "port"
          <*> v .: "user"
          <*> v .: "name"
          <*> v .: "password"
          <*> v .: "con-limit"
        parseJSON _ = throw $ ScError "Invailed"
\end{code}
将 数据库配置转化成 链接字符串。
\begin{code}
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

设置读写异常
\begin{code}
      data ScError = ScError String
        deriving (Eq)
      scError = throw.ScError
      instance Show ScError where
        show (ScError e) = "parse server config file FAILED:\n\t" ++ e
      instance Exception ScError where
        displayException e = "parse server config file FAILED:\n\t"
\end{code}

JSON 与 Yaml 例程。
\begin{json}
{ "port":3000
, "database-config":
  { "addr":"127.0.0.1"
  , "port":"5432"
  , "user":"postgres"
  , "name":"postgres"
  , "password":"postgres"
  , "con-limit":10
  }
}
\end{json}
\begin{yaml}
port: 3000
database-config:
  addr: '127.0.0.1'
  port: '5432'
  user: postgres
  name: postgres
  password: postgres
\end{yaml}
这个需要在运行时传入。假设配置文件在 config.yml 中,启动 UsrManage 模块。
\begin{shell}
  # cat config.yml ｜ dindo-um
\end{shell}
