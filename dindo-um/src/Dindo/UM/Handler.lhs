




% src/Dindo/UM/Handler.lhs

\begin{code}
module Dindo.UM.Handler
    (
    ) where

      import Dindo.Import.Rable
      import Dindo.RIO
      import Dindo.Base
      import Dindo.Import.Database
      import Dindo.Database.Pg
      import Dindo.Import
      import Dindo.Import.Text as T hiding(splitAt,map)
      import Dindo.Import.ByteString as B hiding(splitAt,map)
      import Dindo.Logger
      import Dindo.UM.Data
      import Dindo.Import.Digest
\end{code}

注册 的 API
\begin{code}
      postRegistR :: Handler
      postRegistR = do
        name <- fmap T.unpack $ lookupQuery "name"
        pash <- fmap T.unpack $ lookupQuery "pash"
        tel  <- fmap T.unpack $ lookupQuery "tel"
        now <- liftIO getCurrentTime
        let (time,p) = splitAt 10 $ show now
        let to = showDigest $ sha1 $ B.fromStrict $ encodeUtf8 $ T.concat $ map T.pack [pash,name]
        let uid =  'U':time ++ to
        rt <- tryRunPgT $
          executePg [pgQuery|
            INSERT INTO table_account(key_uid,key_name,key_tel,key_pash)
            VALUES(?,?,?,?)
            |] (uid,name,tel,pash)
        case rt of
          Left e -> returnR $ RtCommonFail $ showT (e ::SomeException)
          Right _ -> returnR $ RtRegist $ T.pack uid
\end{code}
