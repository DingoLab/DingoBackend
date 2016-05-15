




% src/Dindo/UM/Foundation.lhs

\begin{code}
{-# LANGUAGE OverloadedStrings
           , TemplateHaskell
           , TypeFamilies
           , QuasiQuotes
           #-}
\end{code}

\begin{code}
module Dindo.UM.Foundation where
\end{code}

\begin{code}
      import Dindo.Import
      import Dindo.Import.Yesod
      import Dindo.Import.Database
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
      |]
\end{code}

实现Yesod类型类
\begin{code}
      instance Yesod UM where
        errorHandler = returnR
        isAuthorized RegistR _ = noAuth
        isAuthorized LoginR _ = pskAuth
        isAuthorized _ _ = tokenAuth
      instance YesodPersist UM where
        type YesodPersistBackend UM = SqlBackend
        runDB a = getYesod >>= (runSqlPool a.connPool)

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
