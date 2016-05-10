




% src/Dindo/UM/Handler.lhs

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
\end{code}

\begin{code}
module Dindo.UM.Handler
    (
    ) where
\end{code}


\begin{code}
      import Dindo.Import
      import Dindo.Import.Yesod
      import Dindo.Import.Database
      import Dindo.UM.Foundation
      import Dindo.UM.Data
      import Dindo.Import.Digest
      import Dindo.Import.ByteString as B
      import Dindo.Import.Text as T
\end{code}

\begin{code}
      postRegistR :: Handler TypedContent
      postRegistR =
        getParam $ checkName $ insertAItem insertUItem
        where
          getParam f = do
            name' <- lookupPostParam "name"
            pash' <- lookupPostParam "pash"
            case (name',pash') of
              (Just name,Just pash) -> f (name,pash)
              _ -> returnR $ RtRegistFail "param: less and less"
          checkName (name,pash) f = do
            rt <- liftHandlerT $ runDB $ selectList [UsrName ==. name] []
            if empty
              then do
                x <- getCurrentTime
                let (time,p) = splitAt 10 $ show x
                let uid = 'U':time++showDigest $ sha1 $ decodeUtf8 $ T.concat [pash,name]
                f (pack uid,name,pash)
              else returnR $ RtRegistFail "name repeated"
          insertAItem (uid,name,pash) f = do
            liftHandlerT $ runDB $ insert $ Account uid pash
            f (uid,name)
          insertUItem (uid,name) f = do
            liftHandlerT $ runDB $ insert $ Usr uid 0 name "" "" "" "" "U"
            returnR $ RtRegist uid
\end{code}
