




% src/Dindo/UM/Handler.lhs

\begin{code}
module Dindo.UM.Handler
    ( postRegistR
    , postUsrinfoR
    , postLogoutR
    , postLoginR
    , postIdentified
    , postIdentifyR
    , postUsrinfochangeR
    , postChangpashR
    , postUsrhimgR
    , postUpeaddrR
    , postGeteaddR
    ) where
\end{code}


\begin{code}
      import Dindo.Import
      import Dindo.Import.Rable
      import Dindo.Import.Yesod
      import Dindo.Import.Database
      import Dindo.UM.Foundation
      import Dindo.UM.Data
      import Dindo.Import.Digest
      import Dindo.Import.ByteString as B hiding(unpack,pack,splitAt,take,map,null)
      import Dindo.Import.Text as T hiding(splitAt,take,map,null)
      import Dindo.Common.Auth(fromEntity,pickU,pickF)
      import Control.Exception(try,SomeException)
      import Control.Monad
\end{code}


注册的API
\begin{code}
      postRegistR :: Handler TypedContent
      postRegistR =
        getParam insertAItem
        where
          getParam f = do
            name' <- lookupPostParam "name"
            pash' <- lookupPostParam "pash"
            tel'  <- lookupPostParam "tel"
            case (name',pash',tel') of
              (Just name,Just pash,Just tel) -> do
                x <- liftIO getCurrentTime
                let (time,p) = splitAt 10 $ show x
                let to = showDigest $ sha1 $ fromStrictBS $ encodeUtf8 $ T.concat [pash,name]
                let uid = 'U':time ++ to
                f (pack uid,name,pash,read (unpack tel))
              _ -> returnR $ RtRegistFail "param: less and less"
          insertAItem (uid,name,pash,tel) = do
            rt <- liftHandlerT $ tryRunDB $
              insert $ Account uid pash tel name
            returnR $ case rt of
              Left e -> RtRegistFail $ pack $ show e
              Right _ -> RtRegist uid
\end{code}


用户认证的API
\begin{code}
      postIdentifyR :: Handler TypedContent
      postIdentifyR =
        checkParam $ addItem $ checkPic addPic
        where
          checkParam f = do
            email' <- lookupPostParam "email"
            rname' <- lookupPostParam "rname"
            prcid' <- lookupPostParam "prcid"
            addr' <- lookupPostParam "addr"
            case (email',rname',prcid',addr') of
              (Just email,Just rname,Just prcid,Just addr) ->
                f (email,rname,prcid,addr)
              _ -> returnR $ RtIdyFail "param: less and less"
          checkPic f ins = do
            pic' <- lookupFile "pic"
            case pic' of
              Just pic -> do
                rt <- sourceToList $ fileSource pic
                let bpic = B.concat rt
                f (bpic,ins)
              _ -> returnR $ RtIdyFail "param: picture needed"
          addItem f (email,rname,prcid,addr) =
            f $ \uid -> Usr uid email rname prcid addr "N"
          addPic (pic,usr) = do
            uid <- getUid
            now <- liftIO getCurrentTime
            let str = show now
            let (time,p) = splitAt 10 $ str
            let to = showDigest $ sha1 $ fromStrictBS $ encodeUtf8 $ T.concat [uid, pack str]
            let pid = pack $ 'A':time ++ to
            rt <- liftHandlerT $ tryRunDB $ do
              insert $ usr uid
              insert $ Apic pid uid pic $ Just 0
            returnR $ case rt of
              Left e -> RtIdyFail $ pack $ show e
              Right _ -> RtIdy
\end{code}

认证状态查询
\begin{code}
      postIdentified :: Handler TypedContent
      postIdentified = do
        uid <- getUid
        rt <- liftHandlerT $ runDB $ selectList [UsrUid ==. uid] []
        returnR $ case rt of
          (Entity _ item):_ -> if usrStatus item == "P"
            then RtIdfedPass
            else RtIdfedNo
          _ -> RtIdfedNo
\end{code}

用户登录
\begin{code}
      postLoginR :: Handler TypedContent
      postLoginR = do
        uid'  <- lookupPostParam "uid"
        name' <- lookupPostParam "name"
        tel'  <- lookupPostParam "tel"
        case (uid',name',tel') of
          (uid,name,tel) -> do
            pash <- getPash
            rt' <- liftHandlerT $ runDB $ selectList (pickF
              [ (AccountUid, uid)
              , (AccountName, name)
              ] ++ pickF
              [ (AccountTel,fmap (read.unpack) tel)
              ]) []
            case rt' of
              (Entity _ item):_ -> do
                let uid = accountUid item
                now <- liftIO getCurrentTime
                let lim = addUTCTime 3600 now
                let time = show lim
                let to = showDigest $ sha512 $ fromStrictBS $ encodeUtf8 $ T.concat [uid,pash,pack time]
                let tt = pack $ take 22 time ++ to
                liftHandlerT $ runDB $ insert $ TmpToken tt lim uid
                returnR $ RtCommonSuccT tt
        where
          getPash = do
            pash' <- lookupPostParam "pash"
            return $ fromMaybe "" pash'
\end{code}

用户登出
\begin{code}
      postLogoutR :: Handler TypedContent
      postLogoutR = do
        Just token <- lookupHeader "TMP-TOKEN"
        Just uid <- lookupHeader "USR-ID"
        rt <- liftHandlerT $ tryRunDB $ deleteWhere [TmpTokenTt ==. decodeUtf8 token,TmpTokenUid ==. decodeUtf8 uid]
        returnR $ case rt of
          Left e -> RtCommonFail $ pack $ show e
          Right _ -> RtCommonSucc
\end{code}

查询用户信息
\begin{code}
      postUsrinfoR :: Handler TypedContent
      postUsrinfoR = do
        tuid <- getUid
        uid' <- lookupPostParam "uid"
        let uid = fromMaybe tuid uid'
        rt' <- liftHandlerT $ runDB $ selectList [UsrUid ==. uid] []
        case rt' of
          Entity _ rt:_ -> do
            let email = usrEmail rt
            Entity _ item:_ <- liftHandlerT $ runDB $ selectList [AccountUid ==. uid] []
            returnR $ RtUInfo uid (accountName item) (pack $ show $ accountTel item) email
          _ -> returnR RtUInfoNSU
\end{code}

获得用户头像
\begin{code}
      postUsrhimgR :: Handler TypedContent
      postUsrhimgR = do
        tuid <- getUid
        uid' <- lookupPostParam "uid"
        let uid = fromMaybe tuid uid'
        rt' <- liftHandlerT $ runDB $ selectList [ApicUid ==. uid] []
        case rt' of
          Entity _ rt:_ -> returnR $ RtUImg $ apicBpic rt
          _ -> returnR $ RtUImgFail
\end{code}

用户信息变更
\begin{code}
      postUsrinfochangeR :: Handler TypedContent
      postUsrinfochangeR = check update
        where
          updatePic uid pic' = case pic' of
            Nothing -> return ()
            Just pic -> do
              rt <- sourceToList $ fileSource pic
              let bpic = B.concat rt
              updateWhere [ApicUid ==. uid,ApicTyp ==. Just 0] [ApicBpic =. bpic]
          update (a,b,pic) = do
            uid <- getUid
            rt <- liftHandlerT $ tryRunDB $ do
              when (not $ null a) $
                updateWhere [AccountUid ==. uid] a
              when (not $ null b) $
                updateWhere [UsrUid ==. uid] b
              updatePic uid pic
            returnR $ case rt of
              Left e -> RtCommonFail $ pack $ show e
              Right _ -> RtCommonSucc
          check f = do
            name <- liftHandlerT $ lookupPostParam "name"
            tel  <- liftHandlerT $ lookupPostParam "tel"
            email <- liftHandlerT $ lookupPostParam "email"
            rname <- liftHandlerT $ lookupPostParam "rname"
            prcid <- liftHandlerT $ lookupPostParam "prcid"
            addr <- liftHandlerT $ lookupPostParam "addr"
            pic <- liftHandlerT $ lookupFile "pic"
            let a = pickU [(AccountName,name)]
            let a' = pickU [(AccountTel,fmap (read.T.unpack) tel)]
            let b = pickU [(UsrEmail,email),(UsrRname,rname),(UsrPrcid,prcid),(UsrAddr,addr)]
            f (a++a',b,pic)
\end{code}
修改密码
\begin{code}
      postChangpashR :: Handler TypedContent
      postChangpashR = check changePash
        where
          changePash pash = do
            uid <- getUid
            rt <- liftHandlerT $ tryRunDB $ updateWhere [AccountUid ==. uid] [AccountPash =. pash]
            returnR $ case rt of
              Left e -> RtChPskFail $ pack $ show e
              Right _ -> RtChPsk
          check f = do
            pash' <- lookupPostParam "pash"
            case pash' of
              Nothing -> do
                returnR $ RtChPskFail "param: less and less"
              Just x -> f x
\end{code}

收获地址
\begin{code}
      postUpeaddrR :: Handler TypedContent
      postUpeaddrR = spl
        where
          changeItem aid a = do
            rt <- liftHandlerT $ tryRunDB $ updateWhere [AddrAid ==. aid] a
            returnR $ case rt of
              Left e -> RtEaddrFail $ pack $ show e
              Right _ -> RtEaddrChn
          checkChn f = do
            addr <- liftHandlerT $ lookupPostParam "addr"
            zipcode <- liftHandlerT $ lookupPostParam "zip"
            aid' <- liftHandlerT $ lookupPostParam "aid"
            case aid' of
              Just aid -> f aid $ pickU [(AddrAddr,addr),(AddrZip,zipcode)]
              Nothing -> returnR $ RtEaddrFail "param:change: less and less"
          delItem aid = do
            rt <- liftHandlerT $ tryRunDB $ deleteWhere [AddrAid ==. aid]
            returnR $ case rt of
              Left e -> RtEaddrFail $ pack $ show e
              Right _ -> RtEaddrDel
          checkDel f = do
            aid' <- liftHandlerT $ lookupPostParam "aid"
            case aid' of
              Just aid -> f aid
              Nothing -> returnR $ RtEaddrFail "param:del: less and less"
          addItem (addr,zipcode) = do
            uid <- getUid
            now <- liftIO getCurrentTime
            let aid' = showDigest $ sha256 $ fromStrictBS $ encodeUtf8 addr
            let aid = pack $ "A"++show now++aid'
            rt <- liftHandlerT $ tryRunDB $ insert $ Addr aid uid zipcode addr
            returnR $ case rt of
              Left e -> RtEaddrFail $ pack $ show e
              Right _ -> RtEaddrAdd aid
          checkAdd f = do
            addr' <- liftHandlerT $ lookupPostParam "addr"
            zip' <- liftHandlerT $ lookupPostParam "zip"
            case (addr',zip') of
              (Just addr,Just zipcode) -> f (addr,zipcode)
              _ -> returnR $ RtEaddrFail "param:add: less and less"
          spl = do
            opt <- liftHandlerT $ lookupHeader "OPT"
            case opt of
              Just "ADD" -> checkAdd addItem
              Just "DEL" -> checkChn changeItem
              Just "CHANGE" -> checkDel delItem
              _ -> returnR $ RtEaddrFail "header:opt: less and less"
\end{code}

获取收货地址
\begin{code}
      postGeteaddR :: Handler TypedContent
      postGeteaddR = spl
        where
          getByUid uid = do
            rt <- liftHandlerT $ runDB $ selectList [AddrUid ==. uid] []
            returnR $ RtGEadd $ map fromEntity rt
          getByAid aid = do
            uid <- getUid
            rt <- liftHandlerT $ runDB $ selectList [AddrAid ==. aid,AddrUid ==. uid] []
            returnR $ RtGEadd $ map fromEntity rt
          spl = do
            uid' <- liftHandlerT $ lookupPostParam "uid"
            aid' <- liftHandlerT $ lookupPostParam "aid"
            case (uid',aid') of
              (Just uid, _) -> getByUid uid
              (Nothing, Just aid) -> getByAid aid
              _ -> returnR $ RtGEaddFail "param: less and less"
\end{code}
