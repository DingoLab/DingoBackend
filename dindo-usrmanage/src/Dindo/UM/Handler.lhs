




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
        getParam insertAItem
        where
          getParam f = do
            name' <- lookupPostParam "name"
            pash' <- lookupPostParam "pash"
            tel'  <- lookupPostParam "tel"
            case (name',pash') of
              (Just name,Just pash) -> do
                x <- getCurrentTime
                let (time,p) = splitAt 10 $ show x
                let uid = 'U':time++showDigest $ sha1 $ decodeUtf8 $ T.concat [pash,name]
                f (pack uid,name,pash)
              _ -> returnR $ RtRegistFail "param: less and less"
          insertAItem (uid,name,pash,tel) f = do
            rt <- liftIO $ try $ liftHandlerT $ runDB $ insert $ Account uid pash tel name
            case rt of
              Left e -> returnR $ RtRegistFail $ pack $ show e
              Right _ -> returnR $ RtRegist uid

\end{code}
