




% src/Dingo/Common/Yesod/Launch.lhs

\begin{code}
module Dingo.Common.Yesod.Launch
    (
    ) where
\end{code}

\begin{code}
      import Dingo.MicroFramework.Register
      import Yesod
      import Dingo.Common.Yesod.Config
      import Database.Persist.PostgreSQL
\end{code}

Dingo 后端的服务的“标准”
\begin{code}
      class Registrable a => Dindoble a where
        fromPool :: ConnectionPool -> a
        warpDindo :: SvrConfig -> (Int -> a -> IO()) -> IO ()
        warpDindo x warpF =
          runStdoutLogingT $ withPostgresqlPool connStr cT $
            \pool -> do
              let site = fromPool pool
              warpF port site
          where
            (connStr,cT) = dbConfig2Str.svrDb $ x
            port = svrPort x

\end{code}
