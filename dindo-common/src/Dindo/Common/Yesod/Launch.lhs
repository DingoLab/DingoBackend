




% src/Dindo/Common/Yesod/Launch.lhs

\begin{code}
module Dindo.Common.Yesod.Launch
    ( Dindoble(..)
    ) where
\end{code}

\begin{code}
      import Dindo.MicroFramework.Register
      import Yesod
      import Dindo.Common.Yesod.Config
      import Database.Persist.Postgresql
      import Control.Monad.Logger
\end{code}

Dingo 后端的服务的“标准”
\begin{code}
      class Registrable a => Dindoble a where
        fromPool :: ConnectionPool -> SvrConfig -> a
        warpDindo :: SvrConfig -> (Int -> a -> IO()) -> IO ()
        warpDindo x warpF =
          runStdoutLoggingT $ withPostgresqlPool connStr cT $
            \pool -> liftIO $ do
              let site = fromPool pool x
              register site
              warpF port site
          where
            (connStr,cT) = dbConfig2Str.svrDb $ x
            port = svrPort x

\end{code}
