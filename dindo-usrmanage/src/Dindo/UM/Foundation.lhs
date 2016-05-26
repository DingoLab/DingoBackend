




% src/Dindo/UM/Foundation.lhs

\begin{code}
{-# LANGUAGE OverloadedStrings
           , TemplateHaskell
           , TypeFamilies
           , QuasiQuotes
           #-}
\end{code}

\begin{code}
module Dindo.UM.Foundation
    ( module Dindo.UM.Foundation
    , getSvrtimeR
    ) where
\end{code}

\begin{code}
      import Dindo.Common
      import Dindo.Import
      import Dindo.Import.Yesod
      import Dindo.Import.Database
      import Paths_dindo_usrmanage
      import Dindo.Import.Text as T
      import Data.Version
\end{code}

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
        isAuthorized SvrinfoR _ = return Authorized
        isAuthorized SvrtimeR _ = return Authorized
        isAuthorized RegistR _ = noAuth
        isAuthorized LoginR _ = pskAuth
        isAuthorized _ _ = tokenAuth
      instance YesodPersist UM where
        type YesodPersistBackend UM = SqlBackend
        runDB a = getYesod >>= (runSqlPool a.connPool)
      mkSvrinfoR $ pack $ "dindo-um-" ++ showVersion version ++ "; dindo-common-" ++ $(dindo_common_version_quasi)
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
