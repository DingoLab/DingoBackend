




% src/Dindo/AT/Handler.lhs

\begin{code}
module Dindo.AT.Handler
    ( postUtaskR
    , postCtaskR
    , postGtaskR
    , postBtaskR
    , postItaskR
    , postPtaskR
    , postAagentR
    , postDagentR
    , postSagentR
    ) where

      import Dindo.Import
      import Dindo.Import.Rable
      import Dindo.Import.Yesod
      import Dindo.Import.Database
      import Dindo.AT.Foundation
      import Dindo.AT.Data
      import Dindo.Import.Digest
      import Dindo.Import.ByteString as B hiding(unpack,pack,splitAt,take,map,null)
      import Dindo.Import.Text as T hiding(splitAt,take,map,null)
      import Dindo.Common.Auth(fromEntity,pickU,pickF)
      import Control.Exception(try,SomeException)
      import Control.Monad
      import Data.Char
\end{code}

任务发布的 API
\begin{code}
      postUtaskR :: Handler TypedContent
      postUtaskR = getParam newTask
        where
          getParam f = do
            ew' <- lookupPostParam "ew"
            ns' <- lookupPostParam "ns"
            r'  <- lookupPostParam "r"
            wei' <- lookupPostParam "wei"
            size' <- lookupPostParam "size"
            note  <- lookupPostParam "note"
            intr  <- lookupPostParam "intr"
            cost' <- lookupPostParam "cost"
            linked <- lookupPostParam "linked"
            uid <- getUid
            case (ew',ns',r',wei',size',cost') of
              (Just ew,Just ns,Just r,Just wei,Just size,Just cost) ->
                f $ \tid -> ( Task tid (Just uid) linked
                            , Taskinfo tid (readT ew) (readT ns) (readT r) (readT wei) (toSize size) note (readT cost) intr
                            )
              _ -> returnR.RtUtaskFail $ "dindo:at:invailed param"
          toSize s = let (a,b,c) = readT s in [a,b,c]
          newTask f = do
            uid <- getUid
            tid <- mkTid uid
            let (t,ti) = f uid
                tc     = Taskcost tid [] []
            rt <- liftHandlerT $ tryRunDB $ do
              insert t
              insert ti
              insert tc
              return ()
            case rt of
              Left e -> returnR.RtUtaskFail . pack.show $ e
              Right _ -> returnR.RtUtaskSucc $ tid
          mkTid uid = do
            now <- liftIO $ fmap (pack.show) getCurrentTime
            let hash = pack.showDigest.sha1.fromStrictBS.encodeUtf8. T.concat $ [uid,now]
            return.T.concat $ ["T",now,hash]
\end{code}
任务的删改
\begin{code}
      postCtaskR :: Handler TypedContent
      postCtaskR = undefined
        where
          optCheck = do
            ct' <- lookupPostParam "ct"
            let ct = fmap T.toUpper ct'
            case ct of
              Just "DEL" -> delCheck delHandler
              Just "FIX" -> fixCheck fixHandler
              _ -> returnR $ RtCommonFail "dindo:at:invailed param"
          fixCheck f = do
            tid'<- lookupPostParam "tid"
            ew  <- lookupPostParam "ew"
            ns  <- lookupPostParam "ns"
            r   <- lookupPostParam "r"
            wei <- lookupPostParam "wei"
            size <- lookupPostParam "size"
            note <- lookupPostParam "note"
            cost <- lookupPostParam "cost"
            intr <- lookupPostParam "intr"
            linked <- lookupPostParam "linked"
            case tid' of
              Just tid -> f tid (pickU [(TaskCb,fmap Just linked)]) $ pickU
                [ (TaskinfoEw,fmap readT ew)
                , (TaskinfoNs,fmap readT ns)
                , (TaskinfoR ,fmap readT r)
                , (TaskinfoWei,fmap readT wei)
                ] ++ pickU [ (TaskinfoSize,fmap toSize size)]
                  ++ pickU [ (TaskinfoNote,fmap Just note)
                           , (TaskinfoDes ,fmap Just intr)
                           ]
                  ++ pickU [ (TaskinfoCost,fmap readT cost)]
              _ -> returnR $ RtCommonFail "dindo:at:invailed param"
          toSize x = let (a,b,c) = readT x in [a,b,c]
          fixHandler tid cb up = do
            rt <- liftHandlerT $ tryRunDB $ do
              updateWhere [TaskinfoTid ==. tid] up
              updateWhere [TaskTid ==. tid] cb
            case rt of
              Left e -> returnR.RtCommonFail .pack.show $ e
              Right () -> returnR RtCommonSucc
          delCheck f = do
            tid' <- lookupPostParam "tid"
            case tid' of
              Just tid -> f tid
              _ -> returnR $ RtCommonFail "dindo:at:invailed param"
          delHandler :: Text -> Handler TypedContent
          delHandler tid = do
            rt <- liftHandlerT $ tryRunDB $ do
              deleteWhere [TaskcostTid ==. tid]
              deleteWhere [TaskinfoTid ==. tid]
              deleteWhere [TaskTid ==. tid]
            case rt of
              Left e -> returnR.RtCommonFail .pack.show $ e
              Right _ -> returnR RtCommonSucc
\end{code}
\begin{code}
      postGtaskR :: Handler TypedContent
      postGtaskR = undefined
\end{code}
\begin{code}
      postBtaskR :: Handler TypedContent
      postBtaskR = undefined
\end{code}
\begin{code}
      postItaskR :: Handler TypedContent
      postItaskR = undefined
\end{code}
\begin{code}
      postPtaskR :: Handler TypedContent
      postPtaskR = undefined
\end{code}
\begin{code}
      postAagentR :: Handler TypedContent
      postAagentR = undefined
\end{code}
\begin{code}
      postDagentR :: Handler TypedContent
      postDagentR = undefined
\end{code}
\begin{code}
      postSagentR :: Handler TypedContent
      postSagentR = undefined
\end{code}
