




% src/Dindo/UM/Foundation.lhs

\begin{code}
module Dindo.UM.Foundation where

      import Dindo.Common
      import Dindo.Import
      import Dindo.Import.Yesod
      import Dindo.Import.Database
      import Paths_dindo_usrmanage
      import Dindo.Import.Text as T
      import Data.Version
\end{code}

定义基本类型路由表
\begin{code}
      data UM = UM
        { connPool :: ConnectionPool
        , config   :: SvrConfig
        }
      mkYesodData "UM" [parseRoutes|
        /regist RegistR POST
        /identify IdentifyR POST
        /identified Identified POST
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
        isAuthorized ShomeR _ = return Authorized
        isAuthorized RegistR _ = noAuth
        isAuthorized LoginR _ = pskAuth
        isAuthorized _ _ = tokenAuth
      instance YesodPersist UM where
        type YesodPersistBackend UM = SqlBackend
        runDB a = getYesod >>= (runSqlPool a.connPool)
      mkShomeR $ pack $ "dindo-um-" ++ showVersion version ++ "; dindo-common-" ++ $(dindo_common_version_quasi)
\end{code}

微服务架构
\begin{code}
      instance APIble UM where
        apis _ = []
      instance Destorible UM where
        destoryHead _ = ""
        destoryAPI _ = ""
      instance Heartbeatable UM where
        heartbeat _ = return ()
      instance Registrable UM where
        regAddr _ = ""
        regPort = svrPort . config
        regSvrPort _ = 80
        regSvrAddr _ = ""
      instance Dindoble UM where
        fromPool = UM
\end{code}
