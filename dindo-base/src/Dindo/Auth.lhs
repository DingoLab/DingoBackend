




% src/Dindo/Auth.lhs

\begin{code}
module Dindo.Auth
    ( runPash
    , noAuth
    , tokenAuth
    , pskAuth
    ) where

      import Dindo.Import
      import Dindo.Import.Digest
      import Dindo.Import.Pg
      import Dindo.Import.Yesod
      import qualified Dindo.Import.ByteString as B
      import qualified Dindo.Import.Text as T

      import Dindo.Base
      import Dindo.Database
      import Dindo.Database.Pg
\end{code}


用于用户验证的
runPash 0 -> uid
        1 -> name
        2 -> tel
\begin{code}
      runPash :: Int -> B.ByteString -> T.Text -> T.Text
      runPash i time pash = T.pack $ showDigest $ sha512 $ B.fromStrict $ B.concat [pre,T.encodeUtf8 pash,time]
        where
          pre = case i of
            0 -> "uid"
            1 -> "nnnn"
            2 -> "+86"
      runPash _ _  x = id x
\end{code}
\begin{code}
      noAuth :: HandlerT site IO AuthResult
      noAuth = return Authorized
\end{code}

\begin{code}
      tokenAuth :: PgSql site
                => HandlerT site IO AuthResult
      tokenAuth = do
        token' <- lookupHeader "Tmp-Token"
        uid' <- lookupHeader "Uid"
        case (token',uid') of
          (Just token,Just uid) -> do
            rt' <- runPgT $ queryPg [pgQuery|
              SELECT key_timeup
              FROM table_tmptoken
              WHERE key_tmptoken=? AND key_uid=?
              ORDER BY key_timeup
              |] (token,uid)
            case rt' of
              (Only time):_ -> do
                now <- liftIO getCurrentTime
                if diffUTCTime now time >= 0
                  then return $ Unauthorized "Who are you!"
                  else return Authorized
              _ -> return $ Unauthorized "Who are you!"
          _ -> return $ Unauthorized "Who are you!"
\end{code}

\begin{code}
      pskAuth :: PgSql site
              => HandlerT site IO AuthResult
      pskAuth = checkTime $ \time -> do
        pash  <- lookupPostParam "pash"
        uid'  <- lookupPostParam "uid"
        name' <- lookupPostParam "name"
        tel'  <- lookupPostParam "tel"
        case (uid',name',tel') of
          (Just uid,name,tel) -> do
            let [n,t] = map (pickF "AND") $ zip (map ((T.concat.T.words)<$>) [name,tel]) ["key_name","key_tel"]
            rt <- runPgT $ queryPg (toPgQuery $ T.encodeUtf8 $ T.unwords
              [ "SELECT key_pash"
              , "FROM table_account"
              , "WHERE key_uid = ?"
              , fst n , fst t
              ]) (catMaybes [Just uid,snd n,snd t])
            checkPash pash rt (runPash 0 time)
          (Nothing,Just name,tel) -> do
            let t = pickF "AND" ((T.concat.T.words) <$> tel,"key_tel")
            rt <- runPgT $ queryPg (toPgQuery $ T.encodeUtf8 $ T.concat
              [ "SELECT key_pash"
              , "FROM table_account"
              , "WHERE key_name = ?"
              , fst t
              ]) $ catMaybes [Just name,snd t]
            checkPash pash rt (runPash 1 time)
          (Nothing,Nothing,Just tel) -> do
            rt <- runPgT $ queryPg [pgQuery|
              SELECT key_pash
              FROM table_account
              WHERE key_tel = ?
              |] $ Only tel
            checkPash pash rt (runPash 2 time)
          _ -> return $ Unauthorized "Who are you!"
        where
          checkPash (Just pash) rt f = do
            case rt of
              Only usrPash:_ -> if f usrPash == pash
                  then return Authorized
                  else return $ Unauthorized "Who are you!"
              _ -> return $ Unauthorized "Who are you!"
          checkTime f = do
            time' <- lookupHeader "TIME-STAMP"
            now <- liftIO getCurrentTime
            case time' of
              Just time -> do
                let t = read.T.unpack.T.decodeUtf8 $ time
                let diff = diffUTCTime now t
                if diff <= 12 && diff >= (-12)
                  then f time
                  else return $ Unauthorized "I bought a watch last year!"
              _ -> return $ Unauthorized "I bought a watch last year!"
\end{code}
