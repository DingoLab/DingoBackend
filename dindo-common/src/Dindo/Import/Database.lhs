




% src/Dindo/Import/Database.lhs

\begin{code}
module Dindo.Import.Database
    ( module X
    , tryRunDB
    ) where
\end{code}

\begin{code}
      import Database.Persist as X
      import Database.Persist.Postgresql as X
      import Dindo.Database as X
      import Control.Exception
      import Yesod
\end{code}


\begin{code}
      tryRunDB :: ( Yesod site
                  , YesodPersist site
                  , YesodPersistBackend site ~ SqlBackend
                  )
                => YesodDB site a -> HandlerT site IO (Either SomeException a)
      tryRunDB f = do
        runInnerHandler <- handlerToIO
        liftIO $ try $ runInnerHandler $ runDB f
\end{code}
