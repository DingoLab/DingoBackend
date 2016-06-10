




% src/Dindo/Auth.lhs

\begin{code}
module Dindo.Auth
    ( getUid
    , runPash
    , noAuth
    , tokenAuth
    , pskAuth
    , pickF
    , pickFF
    ) where

      import Dindo.RIO
      import Dindo.Rable
      import Dindo.Database
      import Dindo.Database.Pg
      import Dindo.Exception
      import Dindo.Base
      import Dindo.Import.ByteString as B
      import Dindo.Import.Text as T
      import Dindo.Import.Digest
      import Data.Time
      import Network.Wai
\end{code}

一定要在已经认证身份的地方使用，否则没有可靠性
\begin{code}
      getUid :: RIO cfg Text
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
      tokenAuth fm = do
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
              else fm
          _ -> returnR $ Rt403 "Who are you!"
\end{code}
密码验证
\begin{code}
      pskAuth :: PgSql cfg
              => RIO cfg Response -> RIO cfg Response
      pskAuth fm = checkTime $ \time -> do
        pash <- lookupQuery "pash"
        uid <- lookupQuerym "uid"
        name <- lookupQuerym "name"
        tel <- lookupQuerym "tel"
        let (wh,q) = pickFF $ pickF [(uid,"key_uid"),(name,"key_name"),(tel,"key_tel")]
        rt <- runPgT $ queryPg
          (toQuery $ B.concat ["SELECT key_pash FROM table_account WHERE ",(encodeUtf8.T.pack)wh])
          q
        checkPash pash rt (runPash (k uid name tel) (showB time))
        where
          k uid name tel = do
            case (uid,name,tel) of
              (Just _,_,_) -> 0
              (Nothing,Just _,_) -> 1
              (Nothing,Nothing,Just _) -> 2
              _ -> -1000
          checkPash pash rt f = do
            case rt of
              (Only item):_ -> if f item == pash
                then fm
                else returnR $ Rt403 "Who are you!"
              _ -> returnR $ Rt403 "Who are you!"
          checkTime f = do
            time <- lookupHeader "Time-Stamp"
            now <- liftIO getCurrentTime
            let t = readT time
            let diff = diffUTCTime now t
            if abs diff <= 12
              then f t
              else returnR $ Rt403 "Who are you!"
\end{code}
\begin{code}
      pickF [] = []
      pickF ((Just x,y):s) = (x,y):pickF s
      pickF ((Nothing,_):s) = pickF s
      pickFF [] = ([],[])
      pickFF [(x,y)] = (y++"=?",[x])
      pickFF ((x,y):s) = let (l,r) = pickFF s in (y++"=? AND "++l,x:r)
\end{code}
