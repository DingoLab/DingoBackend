




% src/Dindo/Rable.lhs

\begin{code}
module Dindo.Rable
    ( RtType(..),RtWhere(..),RtStatus(..)
    , Varable(..),Rable(..)
    , RtSvrinfo(..)
    ) where

      import Dindo.Import.Aeson as A
      import Dindo.Import.Wai (Status)
      import Dindo.Import.XML as X
      import Dindo.Import.Wai
      import Dindo.Import.Yaml as Y
      import Yesod.Core
      import qualified Dindo.Import.ByteString as B
      import qualified Dindo.Import.Text as T

      import Control.Monad
      import GHC.Exts(fromList)
\end{code}


\begin{code}
      data RtType = RtJson | RtYaml | RtXml | RtText
        deriving (Eq,Show)
      data RtWhere = RtBody | RtOther T.Text
        deriving (Eq,Show)
\end{code}


\begin{code}
      class Show a => Varable a where
        toValue :: a -> Value
        toNodes :: a -> [Node]
        toContents :: RtType -> a -> B.ByteString
        toContents = defToContent
      defToContent :: Varable a => RtType -> a -> B.ByteString
      defToContent RtJson = defToContentJson
      defToContent RtYaml = defToContentYaml
      defToContent RtXml = defToContentXml
      defToContent RtText = B.showButf8
      defToContentJson :: Varable a => a -> B.ByteString
      defToContentJson = B.toStrict. A.encode . toValue
      defToContentYaml :: Varable a => a -> B.ByteString
      defToContentYaml = Y.encode . toValue
      defToContentXml :: Varable a => a -> B.ByteString
      defToContentXml x = B.toStrict $ renderLBS def $ Document p root []
        where
          root = Element "data" (fromList []) $ toNodes x
          p = Prologue [] Nothing []
\end{code}


\begin{code}
      data RtStatus = RtSucc (Maybe Status)
                    | RtFail (Maybe Status)
      statusHead :: RtStatus -> T.Text
      statusHead (RtSucc _) = "Success"
      statusHead (RtFail _) = "Failed"
\end{code}

\begin{code}
      class Varable a => Rable a where
        toWhere :: a -> RtWhere
        toStatus :: a -> RtStatus
        returnR :: a -> HandlerT site IO TypedContent
        returnR = defReturnR
      defReturnR :: Rable a
                 => a -> HandlerT site IO TypedContent
      defReturnR x = do
        addHeader "Status" $ status x
        if toWhere x == RtBody
          then addHeader "Context-Where" "Body"
          else addHeader "Context-Where" $ (\(RtOther a)-> a) $ toWhere x
        addContent >>= sC (toStatus x)
        where
          sC (RtSucc (Just x)) = sendResponseStatus x
          sC (RtSucc Nothing) = return
          sC (RtFail (Just x)) = sendResponseStatus x
          sC (RtFail Nothing) = return
          status = statusHead.toStatus
          addContent = case toWhere x of
            RtBody -> selectRep $ do
              provideRepType "application/json" $ return $ T.decodeUtf8 $ toContents RtJson x
              provideRepType "application/yaml" $ return $ T.decodeUtf8 $ toContents RtYaml x
              provideRepType "application/xml"  $ return $ T.decodeUtf8 $ toContents RtXml  x
            RtOther y -> do
              addHeader y $ T.pack $ show x
              selectRep $  provideRep $ return (""::T.Text)
\end{code}


将 Yesod 中的 ErrorResponse 实现 Varable 与 Rable
\begin{code}
      instance Varable ErrorResponse where
        toValue NotFound = A.String "NotFound"
        toValue (InternalError x) = object ["internal-error" .= x]
        toValue (PermissionDenied x) = object ["permission-denied" .= x]
        toValue (InvalidArgs x) = object ["invalid-args" .= x]
        toValue NotAuthenticated = A.String "NotAuthenticated"
        toValue (BadMethod x) = object ["bad-method" .= show x]
        toNodes NotFound = [xml|NotFound|]
        toNodes (InternalError x) = [xml|<InternalError>#{x}|]
        toNodes (PermissionDenied x) = [xml|<PermissionDenied>:#{x}|]
        toNodes (InvalidArgs x) = [xml|<InvalidArgs>#{x'}|]
          where
            x' = T.unlines x
        toNodes NotAuthenticated = [xml|NotAuthenticated|]
        toNodes (BadMethod x) = [xml|<BadMethod>#{T.pack $ show x}|]

      instance Rable ErrorResponse where
        toWhere _ = RtBody
        toStatus _ = RtFail Nothing
\end{code}


通用成功与失败标志
\begin{code}
      data RtCommon = RtCommonSucc
                    | RtCommonSuccT T.Text
                    | RtCommonFail T.Text
        deriving (Eq,Show)
      instance Varable RtCommon where
        toValue RtCommonSucc = Null
        toValue (RtCommonSuccT t) = String t
        toValue (RtCommonFail x) = String x
        toNodes RtCommonSucc = [xml|null|]
        toNodes (RtCommonSuccT x) = [xml|#{x}|]
        toNodes (RtCommonFail x) = [xml|<error>#{x}|]
      instance Rable RtCommon where
        toWhere RtCommonSucc = RtBody
        toWhere (RtCommonFail _) = RtBody
        toWhere (RtCommonSuccT _) = RtBody
        toStatus RtCommonSucc = RtSucc Nothing
        toStatus (RtCommonSuccT _) = RtSucc Nothing
        toStatus (RtCommonFail _) = RtFail $ Just status400
\end{code}

服务器状态
\begin{code}
      data RtSvrinfo = RtSvrinfo T.Text T.Text
        deriving (Show,Eq)
      instance Varable RtSvrinfo where
        toValue (RtSvrinfo t i) = object ["server-time" .= t,"server-info" .= i]
        toNodes (RtSvrinfo t i) = [xml|<server-time>#{t}<server-info>#{i}|]
      instance Rable RtSvrinfo where
        toWhere _ = RtBody
        toStatus _ = RtSucc Nothing
\end{code}
