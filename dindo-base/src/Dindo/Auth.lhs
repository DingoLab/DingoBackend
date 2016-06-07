




% src/Dindo/Auth.lhs

\begin{code}
module Dindo.Auth
    (
    ) where

      import Dindo.RIO
      import Dindo.Rable
      import Dindo.Database
      import Dindo.Exception
      import Dindo.Base
      import Dindo.Import.ByteString as B
      import Dindo.Import.Text as T
      import Dindo.Import.Digest
      import Data.Time
\end{code}

一定要在已经认证身份的地方使用，否则没有可靠性
\begin{code}
      getUid :: RIO cfg TEXT
      getUid = lookupHeader "UID"
\end{code}

用户验证
runPash 0   -> uid
        1   -> name
        2   -> tel
\begin{code}
      runPash :: Int -> B.ByteString -> Text -> Text
      runPash i time pash = T.pack $ showDigest $ sha512 $ B.fromStrict $ B.concat [pre,encodeUtf8 pash,time]
        where
          pre = case i of
            0 -> "uid"
            1 -> "nnnn"
            2 -> "+86"
\end{code}

空验证
\begin{code}
      noAuth :: RIO cfg Response -> RIO cfg Response
      noAuth = id
\end{code}
Token 验证
\begin{code}
      tokenAuth :: PgSql cfg
                => RIO cfg Response -> RIO cfg Response
      tokenAuth f = do
        token <- lookupHeader "Tmp-Token"
        uid <- getUid
        rt' <- runPgT $ queryPg
          "SELECT key_timeup FROM table_tmptoken WHERE key_tmptoken=? AND key_uid=?"
          [token,uid]
        case rt' of
          (Only rt):_ -> do
            now <- liftIO getCurrentTime
            if diffUTCTime now rt >= 0
              then returnR $ Rt403 "Who are you!"
              else f
          _ -> returnR $ Rt403 "Who are you!"
\end{code}
密码验证
\begin{code}
      pskAuth :: PgSql cfg
              => RIO cfg Response -> RIO cfg Response
      pskAuth f = do
        pash <- lookupQuery "pash"
        uid <- lookupQuerym "uid"
        name <- lookupQuerym "name"
        tel <- lookupQuerym "tel"
        let (wh,q) = pickFF $ pickF [(uid,"key_uid"),(name,"key_name"),(tel,"key_tel")]
        rt <- runPgT $ queryPg
          ("SELECT key_pash FROM table_account WHERE "++ wh)
          q
          
\end{code}
\begin{code}
      pickF [] = ([],[])
      pickF ((Just x,y):s) = let (l,r) = pickF s in (x:l,y:r)
      pickF ((Nothing,_):s) = pickF s
      pickFF [] = ([],[])
      pickFF [(x,y)] = (y++"=?",x)
      pickFF ((x,y):s) = let (l,r) = pickFF s in (y++"=? AND "++l,x:r)
\end{code}
