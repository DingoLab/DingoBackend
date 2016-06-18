




% src/Dindo/UM/Foundation.lhs

\begin{code}
module Dindo.UM.Foundation
    ( UM(..)
    , UMCfg
    , Handler(..)
    , Route(..)
    , resourcesUM
    , getSvrinfoR
    ) where

      import Dindo.Auth
      import Dindo.Base
      import Dindo.Config
      import Dindo.Rable
      import Dindo.Database.Pg

      import Paths_dindo_um(version)
      import Data.Version(showVersion,Version)

      import Dindo.Import
      import Dindo.Import.Aeson
      import Dindo.Import.Logger hiding (LogStdout,LogStderr,LogFile)
      import Dindo.Import.Yesod
      import qualified Dindo.Import.Text as T
\end{code}


\begin{code}
      data UM = UM
        { umPg :: PgCP
        , umLg :: Logger
        }
      mkYesodData "UM" [parseRoutes|
        /regist RegistR POST
        /identify IdentifyR POST
        /identified IdentifiedR POST
        /login LoginR POST
        /logout LogoutR POST
        /usrinfo UsrinfoR POST
        /usrhimg UsrhimgR POST
        /usrinfochange UsrinfochangeR POST
        /changpash ChangpashR POST
        /upeaddr UpeaddrR POST
        /geteaddr GeteaddR POST
      |]
\end{code}


实现Yesod类型类
\begin{code}
      instance Yesod UM where
        errorHandler = returnR
        isAuthorized SvrinfoR _ = return Authorized
        isAuthorized RegistR _ = noAuth
        isAuthorized LoginR _ = pskAuth
        isAuthorized _ _ = tokenAuth
        makeLogger = return.umLg
      mkSvrinfoR $ T.pack $ "dindo-um-" ++ showVersion version ++ "; dindo-base-" ++ $(dindo_base_version_quote)
\end{code}

用于 PostgreSQL 数据库
\begin{code}
      instance PgSql UM where
        getPgCP = getYesod >>= (return.umPg)
\end{code}

配置文件
\begin{code}
      data UMCfg = UMCfg
        { umPort :: Int
        , umLogPath :: LogPath
        , umTimeout :: Int
        , umListenType :: String
        , umDb :: PgCI
        , umDbSp :: Int
        , umDbCKpt :: NominalDiffTime
        , umDbCMax :: Int
        }

      instance FromJSON UMCfg where
        parseJSON (Object v) = UMCfg
          <$> v .: "port"
          <*> v .: "logpath"
          <*> v .: "timeout"
          <*> v .: "listen-type"
          <*> v .: "database"
          <*> v .: "db-sub-pools"
          <*> v .: "db-connection-kept"
          <*> v .: "db-conncetion-max"

      instance CfgF UMCfg where
        type CfgD UMCfg = UM
        getPort = umPort
        getLogPath = umLogPath
        getTimeout = umTimeout
        getListenType = umListenType
        getServerName _ = "dindo-um"
        toCfgD UMCfg{..} = do
          cp <- toPgCP umDb umDbSp umDbCKpt umDbCMax
          (getter,_) <- clockDateCacher
          lS <- case umLogPath of
            LogFile x -> newFileLoggerSet defaultBufSize x
            LogStdout -> newStdoutLoggerSet defaultBufSize
            LogStderr -> newStderrLoggerSet defaultBufSize
          return $! UM cp (Logger lS getter)
        beginMicro _ = putStrLn "begin microserver" >> return True
        endMicro _ = putStrLn "end microserver"
        heartBeat _ = putStrLn "假装在心跳"
\end{code}
