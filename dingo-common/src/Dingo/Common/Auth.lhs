




% src/Dingo/Common/Auth.lhs

\begin{code}
{-# LANGUAGE TypeFamilies
           , OverloadedStrings
           #-}
\end{code}

\begin{code}
module Dingo.Common.Auth
    ( runPash
    , tokenAuth
    , pskAuth
    ) where
\end{code}

\begin{code}
      import Yesod
      import Database.Persist
      import Database.Persist.Sql
      import Dingo.Database
      import Data.Time
      import Data.Text.Encoding
      import Data.Maybe
      import Data.Text (unpack)
\end{code}


用于用户验证的
\begin{code}
      runPash = id
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
        uid' <-lookupPostParam "uid"
        case uid' of
          Just uid -> do
            pash <- getPash
            rt' <- liftHandlerT $ runDB $ selectList [AccountUid ==. uid] []
            case rt' of
              rt:_ -> do
                let usrPash = runPash.accountPash.fromEntity $ rt
                if usrPash == pash
                  then return Authorized
                  else return $ Unauthorized "Who are you!"
              _ -> return $ Unauthorized "Who are you!"
          Nothing -> authByUn
        where
          getPash = do
            pash' <- lookupPostParam "pash"
            return $ maybe "" runPash pash'
          authByUn = do
            name' <- lookupPostParam "name"
            case name' of
              Just name -> do
                pash <- getPash
                rt' <- liftHandlerT $ runDB $ selectList [LoginUname ==. name] []
                case rt' of
                  rt:_ -> do
                    let usrPash = runPash.loginPash.fromEntity $ rt
                    if usrPash == pash
                      then return Authorized
                      else return $ Unauthorized "Who are you!"
                  _ -> return $ Unauthorized "Who are you!"
              Nothing -> authByTel
          authByTel = do
            tel' <- lookupPostParam "tel"
            case tel' of
              Just tel -> do
                pash <- getPash
                rt' <- liftHandlerT $ runDB $ selectList [LoginUtel ==. (read $ unpack $ tel)] []
                case rt' of
                  rt:_ -> do
                    let usrPash = runPash.loginPash.fromEntity $ rt
                    if usrPash == pash
                      then return Authorized
                      else return $ Unauthorized "Who are you!"
                  _ -> return $ Unauthorized "Who are you!"
              Nothing -> return $ Unauthorized "Who are you!"
      fromEntity :: Entity a -> a
      fromEntity (Entity _ x) = x
\end{code}
