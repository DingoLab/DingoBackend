




% src/Dindo/Common/Auth.lhs

\begin{code}
module Dindo.Common.Auth
    ( runPash
    , tokenAuth
    , pskAuth
    , noAuth
    , fromEntity
    , pickF
    , pickU
    , getUid
    ) where
\end{code}

\begin{code}
      import Yesod
      import Database.Persist
      import Database.Persist.Sql
      import Dindo.Database
      import Data.Time
      import Data.Text.Encoding
      import Data.Maybe
      import qualified Data.ByteString as B
      import qualified Data.ByteString.Lazy as B hiding (concat,ByteString)
      import Data.Text (unpack,pack,Text)
      import Data.Digest.Pure.SHA
\end{code}


\begin{code}
      pickU [] = []
      pickU ((y,Just x):oth) = (y =. x):pickU oth
      pickU ((_,Nothing):oth) = pickU oth
      pickF [] = []
      pickF ((y,Just x):oth) = (y ==. x):pickF oth
      pickF ((_,Nothing):oth) = pickF oth
      getUid :: ( Yesod site
                , YesodPersist site
                , YesodPersistBackend site ~ SqlBackend
                )
             => HandlerT site IO Text
      getUid = do
        tt' <- lookupHeader "TMP-TOKEN"
        uid' <- lookupHeader "UID"
        let Just tt = fmap decodeUtf8 tt'
        let Just uid = fmap decodeUtf8 uid'
        rt':_ <- liftHandlerT $ runDB $ selectList [TmpTokenTt ==. tt,TmpTokenUid ==. uid] []
        let rt = fromEntity rt'
        return $ tmpTokenUid rt
\end{code}

用于用户验证的
runPash 0 -> uid
        1 -> name
        2 -> tel
\begin{code}
      runPash :: Int -> B.ByteString -> Text -> Text
      runPash i time pash = pack $ showDigest $ sha512 $ B.fromStrict $ B.concat [pre,encodeUtf8 pash,time]
        where
          pre = case i of
            0 -> "uid"
            1 -> "nnnn"
            2 -> "+86"
      runPash _ _  x = id x
      noAuth :: Yesod site => HandlerT site IO AuthResult
      noAuth = return Authorized

      tokenAuth :: ( Yesod site
                   , YesodPersist site
                   , YesodPersistBackend site ~ SqlBackend
                   )
                => HandlerT site IO AuthResult
      tokenAuth = do
        token' <- lookupHeader "TMP-TOKEN"
        case token' of
          Nothing -> return $ Unauthorized "Who are you!"
          Just token -> do
            rt' <- liftHandlerT $ runDB $ selectList [TmpTokenTt ==. decodeUtf8 token][Desc TmpTokenTime]
            case rt' of
              rt:_ -> do
                now <- liftIO getCurrentTime
                let time = tmpTokenTime.fromEntity $ rt
                if diffUTCTime now time >= 0
                  then return $ Unauthorized "Who are you!"
                  else return Authorized
              _ -> return $ Unauthorized "Who are you!"

      pskAuth :: ( Yesod site
                 , YesodPersist site
                 , YesodPersistBackend site ~ SqlBackend
                 )
              => HandlerT site IO AuthResult
      pskAuth = checkTime $ \time -> do
        pash  <- getPash
        uid'  <- lookupPostParam "uid"
        name' <- lookupPostParam "name"
        tel''  <- lookupPostParam "tel"
        let tel' = fmap (read.unpack) tel'' :: Maybe Int
        case (uid',name',tel') of
          (Nothing,Nothing,Nothing) -> return $ Unauthorized "Who are you!"
          (Just uid,name,tel) -> do
            rt <- liftHandlerT $ runDB $ selectList (
              [AccountUid ==. uid] ++ pickF [(AccountName,name)]++pickF [(AccountTel,tel)]) []
            checkPash pash rt (runPash 0 time)
          (Nothing,Just name,tel) -> do
            rt <- liftHandlerT $ runDB $ selectList (
              [AccountName ==. name] ++ pickF [(AccountTel,tel)]) []
            checkPash pash rt (runPash 1 time)
          (Nothing,Nothing,Just tel) -> do
            rt <- liftHandlerT $ runDB $ selectList
              [AccountTel ==. tel] []
            checkPash pash rt (runPash 2 time)
          _ -> return $ Unauthorized "Who are you!"
        where
          getPash = do
            pash' <- lookupPostParam "pash"
            return $ fromMaybe "" pash'
          checkPash pash rt f = do
            case rt of
              item:_ -> do
                let usrPash = f.accountPash.fromEntity $ item
                if usrPash == pash
                  then return Authorized
                  else return $ Unauthorized "Who are you!"
              _ -> return $ Unauthorized "Who are you!"
          checkTime f = do
            time' <- liftHandlerT $ lookupHeader "TIME-STAMP"
            now <- liftIO getCurrentTime
            case time' of
              Just time -> do
                let t = read.unpack.decodeUtf8 $ time
                let diff = diffUTCTime now t
                if diff <= 12 && diff >= (-12)
                  then f time
                  else return $ Unauthorized "I bought a watch last year!"
              _ -> return $ Unauthorized "I bought a watch last year!"

      fromEntity :: Entity a -> a
      fromEntity (Entity _ x) = x
\end{code}
