




% src/Dindo/AT/Foundation.lhs

\begin{code}
module Dindo.AT.Foundation
    ( module Dindo.AT.Foundation
    , getSvrtimeR
    ) where

      import Dindo.Common
      import Dindo.Import
      import Dindo.Import.Yesod
      import Dindo.Import.Database
      import Dindo.Import.Text as T
      import Data.Version
      import Paths_dindo_agentake
\end{code}

定义基本类型路由表
\begin{code}
      data AT = AT
        { connPool :: ConnectionPool
        , config   :: SvrConfig
        }
      mkYesodData "AT" [parseRoutes|
        /upload-task  UtaskR  POST
        /change-task  CtaskR  POST
        /get-task     GtaskR  POST
        /bargain-task BtaskR  POST
        /info-task    ItaskR  POST
        /pay-task     PtaskR  POST
        /add-agent    AagentR POST
        /del-agent    DagentR POST
        /static-agent SagentR POST
      |]
\end{code}

实现 Yesod 类型类
\begin{code}
      instance Yesod AT where
        errorHandler = returnR
        isAuthorized ShomeR _ = return Authorized
        isAuthorized _ _      = tokenAuth
      instance YesodPersist AT where
        type YesodPersistBackend AT = SqlBackend
        runDB a = getYesod >>= (runSqlPool a.connPool)
      mkShomeR $ pack $ "dindo-at-" ++ showVersion version ++ "; dindo-common-" ++ $(dindo_common_version_quasi)
\end{code}

实现为服务架构
\begin{code}
      instance APIble AT where
        apis _ = []
      instance Destorible AT where
        destoryHead _ = ""
        destoryAPI _ = ""
      instance Heartbeatable AT where
        heartbeat _ = return ()
      instance Registrable AT where
        regAddr _ = ""
        regPort = svrPort.config
        regSvrPort _ = 80
        regSvrAddr _ = ""
      instance Dindoble AT where
        fromPool = AT
\end{code}
