




% src/Dingo/Common/Yesod/Launch.lhs

\begin{code}
module Dingo.Common.Yesod.Launch
    ( Dindoble(..)
    ) where
\end{code}

\begin{code}
      import Dingo.MicroFramework.Register
      import Yesod
      import Dingo.Common.Yesod.Config
      import Database.Persist.Postgresql
      import Control.Monad.Logger
\end{code}

Dingo 后端的服务的“标准”
\begin{code}
      class Registrable a => Dindoble a where
        fromPool :: ConnectionPool -> a
        warpDindo :: SvrConfig -> (Int -> a -> IO()) -> IO ()
        warpDindo x warpF =
          runStdoutLoggingT $ withPostgresqlPool connStr cT $
            \pool -> liftIO $ do
              let site = fromPool pool
              register site
              warpF port site
          where
            (connStr,cT) = dbConfig2Str.svrDb $ x
            port = svrPort x

\end{code}
