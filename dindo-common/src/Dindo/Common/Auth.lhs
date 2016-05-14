




% src/Dindo/Common/Auth.lhs

\begin{code}
{-# LANGUAGE TypeFamilies
           , OverloadedStrings
           #-}
\end{code}

\begin{code}
module Dindo.Common.Auth
    ( runPash
    , tokenAuth
    , pskAuth
    , noAuth
    , fromEntity
    , fromMaybe'
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
      import Data.Text (unpack)
\end{code}


用于用户验证的
\begin{code}
      runPash _= id
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
      pskAuth = do
        now <- liftIO getCurrentTime
        uid'  <- lookupPostParam "uid"
        name' <- lookupPostParam "name"
        tel'  <- lookupPostParam "tel"
        case (uid',name',tel') of
          (Nothing,Nothing,Nothing) -> return $ Unauthorized "Who are you!"
          (uid,name,tel) -> do
            pash <- getPash
            rt' <- liftHandlerT $ runDB $ selectList
              (  fromMaybe' AccountUid  uid
              ++ fromMaybe' AccountTel (fmap (read.unpack) tel)
              ++ fromMaybe' AccountName name
              ) []
            case rt' of
              rt:_ -> do
                let usrPash = runPash now.accountPash.fromEntity $ rt
                if usrPash == pash
                  then return Authorized
                  else return $ Unauthorized "Who are you!"
              _ -> return $ Unauthorized "Who are you!"
        where
          getPash = do
            pash' <- lookupPostParam "pash"
            return $ fromMaybe "" pash'
      fromEntity :: Entity a -> a
      fromEntity (Entity _ x) = x
      fromMaybe' _ Nothing = []
      fromMaybe' x (Just y) = [x ==. y]
\end{code}
