




% src/Dindo/UM/Foundation.lhs

\begin{code}
{-# LANGUAGE OverloadedStrings
           , TemplateHaskell
           #-}
\end{code}

\begin{code}
module Dindo.UM.Foundation
    (
    ) where
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
      mkYesodData "UM" $(parseRoutesFile "dindo-config/um/route")
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
        runDB a = getYesod >>= (runWithPool a.connPool)
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
