




% src/Dingo/Common/Yesod.lhs


\begin{code}
module Dingo.Common.Yesod
    (
    ) where
\end{code}

\begin{code}
      import Yesod
      import Database.Persist
      import Dingo.Database
      import Data.Time

\end{code}


用于用户验证的
\begin{code}
      runPash = id
      noAuth :: Yesod site => HandlerT site IO AuthResult
      noAuth = return Authorized

      tokenAuth :: ( Yesod site
                   , YesodPersist site
                   , YesodPersistBackend site ~ DBBackend
                   )
                => HandlerT site IO AuthResult
      tokenAuth = do
        token' <- lookupHeader "TMP-TOKEN"
        case token' of
          Nothing -> return Unauthorized
          Just token -> do
            rt' <- liftHandlerT $ runDB $ selectList [TmpTokenTt ==. decodeUtf8 token]［Decs TmpTokenTime］
            case rt' of
              rt:_ -> do
                now <- liftIO getCurrentTime
                let time = tmpTokenTime.(\Entity x -> x) $ rt
                if diffUTCTime now time >= 0
                  then return Unauthorized
                  else return Authorized
              _ -> return Unauthorized

      pskAuth :: ( Yesod site
                 , YesodPersist site
                 , YesodPersistBackend site ~ DBBackend
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
                let usrPash = runPash.accountPash.(\Entity x -> x) $ rt
                if usrPash == pash
                  then return Authorized
                  else return Unauthorized
              _ -> return Unauthorized
          Nothing -> do
            authByUn
        where
          getPash = do
            pash' <- lookupPostParam "pash"
            return $ fromMaybe "" $ fmap runPash pash'
          authByUn = do
            name' <- lookupPostParam "name"
            case name' of
              Just name -> do
                pash <- getPash
                rt' <- liftHandlerT $ runDB $ selectList [LoginUname ==. name] []
                case rt' of
                  rt:_ -> do
                    let usrPash = runPash.loginPash.(\Entity x -> x) $ rt
                    if usrPash == pash
                      then return Authorized
                      else return Unauthorized
                  _ -> return Unauthorized
              Nothing -> authByTel
          authByTel = do
            tel' <- lookupPostParam "tel"
            case tel' of
              Just tel -> do
                pash <- getPash
                rt' <- liftHandlerT $ runDB $ selectList [LoginUtel ==. tel] []
                case rt' of
                  rt:_ -> do
                    let usrPash = runPash.loginPash.(\Entity x -> x) $ rt
                    if usrPash == pash
                      then return Authorized
                      else return Unauthorized
                  _ -> return Unauthorized
              Nothing -> return Unauthorized
\end{code}
