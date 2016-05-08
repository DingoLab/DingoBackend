




% src/Dingo/Common/Rable.lhs

返回的类型的通用类型类

\begin{code}
{-# LANGUAGE OverloadedStrings
           , TemplateHaskell
           , QuasiQuoter
           #-}
\end{code}

\begin{code}
module Dingo.Common.Rable
    ( RtType(..)
    , RtWhere(..)
    , Varable(..)
    , defToContent
    , defToContentXml
    , defToContentYaml
    , defToContentJson
    , Rable(..)
    , defReturnR
    , RtStatus(..)
    , statusHead
    , RtFailCode(..)
    ) where
\end{code}


\begin{code}
      import Data.Aeson as A
      import Data.Yaml as Y
      import Text.XML as X
      import Text.Hamlet.XML
      import Data.ByteString.Lazy.Internal as B
      import Data.ByteString.Lazy (fromStrict)
      import Data.Text as T
      import GHC.Exts(fromList)
      import Control.Monad
      import Yesod.Core
\end{code}

JSON,Yaml,XML
\begin{code}
      data RtType = RtJson | RtYaml | RtXml
      data RtWhere = RtBody | RtOther Text
\end{code}

\begin{code}
      class Varable a where
        toValue :: a -> Value
        toNodes :: a -> [Node]
        toContent :: RtType -> a -> B.ByteString
        toContent = defToContent
      defToContent :: Varable a => RtType -> a -> B.ByteString
      defToContent RtJson = defToContentJson
      defToContent RtYaml = defToContentYaml
      defToContent RtXml = defToContentXml
      defToContentJson = A.encode . toValue
      defToContentYaml = fromStrict . Y.encode . toValue
      defToContentXml  x = renderLBS def $ Document p root []
        where
          root = Element "data" (fromList []) x
          p = Prologue [] Nothing []
\end{code}

\begin{code}
      class Varable a => Rable a where
        toWhere :: a -> RtWhere
        toStatus :: a -> RtStatus
        returnR :: a -> m  TypedContent
        returnR = defReturnR
      defReturnR x = do
        addHeader "STATUS" $ status x
        if toWhere x == RtBody
          then addHeader "CONTEXT-WHERE" "BODY"
          else addHeader "CONTEXT-WHERE" $ (\(RtOther a)-> a) x
        addContent
        where
          status = statusHead.toStatus
          addContent = case toWhere x of
            RtBody -> selectRep do
              provideRepType "application/json" $ return $ decodeUtf8 $ toContent RtJson x
              provideRepType "application/yaml" $ return $ decodeUtf8 $ toContent RtYaml x
              provideRepType "application/xml"  $ return $ decodeUtf8 $ toContent RtXml  x
            RtOther x -> do
              addHeader x decodeUtf8 $ toContent t x
              selectRep $ provideRep $ return ()
\end{code}

\begin{code}
      data RtStatus = RtSucc | RtFail
      statusHead :: RtStatus -> Text
      statusHead RtSucc = "SUCCESS"
      statusHead RtFail = "FAILED"
\end{code}


将 Yesod 中的 ErrorResponse 实现 Varable 与 Rable
\begin{code}
      instance Varable ErrorResponse where
        toValue NotFound = A.String "NotFound"
        toValue (InternalError x) = object ["internal-error" .= x]
        toValue (PermissionDenied x) = object ["permission-denied" .= x]
        toValue (InvalidArgs x) = object ["invalid-args" .= x]
        toValue NotAuthenticated = A.String "NotAuthenticated"
        toValue (BadMethod x) = object ["bad-method" .= x]
        toNodes NotFound = [xml|NotFound|]
        toNodes (InternalError x) = [xml|<InternalError>#{x}|]
        toNodes (PermissionDenied x) = [xml|<PermissionDenied#{x}|]
        toNodes (InvalidArgs x) = [xml|<InvalidArgs>#{x}|]
        toNodes NotAuthenticated = [xml|NotAuthenticated|]
        toNodes (BadMethod x) = [xml|<BadMethod>#{x}|]

      instance Rable ErrorResponse where
        toWhere _ = RtOther "CONTEXT"
        toStatus _ = RtFail
\end{code}
