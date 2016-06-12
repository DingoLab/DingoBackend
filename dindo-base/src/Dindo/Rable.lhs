




% src/Dindo/Rable.lhs

返回的类型的通用类型类

\begin{code}
module Dindo.Rable
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
    , statusH
    , statusC
    , RtCommon(..)
    , Rt403(..)
    , Rt500(..)
    , RtSvrInfo(..)
    ) where

      import Dindo.RIO
      import Dindo.Base
      import Dindo.Exception
      import Dindo.Import
      import Dindo.Import.Aeson as A
      import Dindo.Import.Yaml as Y
      import Text.XML as X
      import Text.Hamlet.XML
      import Data.ByteString.Lazy as BL
      import Data.ByteString.Internal as BI
      import Data.Text as T
      import Data.Text.Encoding
      import GHC.Exts(fromList)
      import Control.Monad
      import Control.Exception(SomeException)
\end{code}


JSON,Yaml,XML
\begin{code}
      data RtType = RtJson | RtYaml | RtXml | RtOth BI.ByteString
        deriving (Eq,Show)
      data RtWhere = RtBody | RtOther BI.ByteString
        deriving (Eq)
      instance Show RtWhere where
        show RtBody = "Body"
        show (RtOther o) =  T.unpack $ decodeUtf8 o
\end{code}

MIME 转换
\begin{code}
      toMIME :: RtType -> BI.ByteString
      toMIME RtJson = "application/json"
      toMIME RtYaml = "application/yaml"
      toMIME RtXml  = "application/xml"
      toMIME (RtOth x) = x
\end{code}

\begin{code}
      class Show a => Varable a where
        toValue :: a -> Value
        toNodes :: a -> [Node]
        toContents :: RtType -> a -> BL.ByteString
        toContents t = fromStrict.defToContent t
      defToContent :: Varable a => RtType -> a -> BI.ByteString
      defToContent RtJson = defToContentJson
      defToContent RtYaml = defToContentYaml
      defToContent RtXml = defToContentXml
      defToContentJson :: Varable a => a -> BI.ByteString
      defToContentJson = toStrict. A.encode . toValue
      defToContentYaml :: Varable a => a -> BI.ByteString
      defToContentYaml = Y.encode . toValue
      defToContentXml :: Varable a => a -> BI.ByteString
      defToContentXml  x = toStrict $ renderLBS def $ Document p root []
        where
          root = Element "data" (fromList []) $ toNodes x
          p = Prologue [] Nothing []
\end{code}

\begin{code}
      class Varable a => Rable a where
        toWhere :: a -> RtWhere
        toStatus :: a -> RtStatus
        returnR ::  a -> RIO cfg Response
        returnR = defReturnR
      defReturnR :: Rable a
                 => a -> RIO cfg Response
      defReturnR x = do
        let whereRt = twhere x
        cTRt <- toCT
        return $ responseLBS (sC x)
          [ ("Status",sH x)
          , ("Context-Where",whereRt)
          , ("Content-Type",toMIME cTRt)
          ] $ toContents cTRt $ x
        where
          sH = statusH.toStatus
          sC = statusC.toStatus
          twhere x = if toWhere x == RtBody
            then "Body"
            else (\(RtOther a)-> a) $ toWhere x
          toCT = do
            wh <- lookupHeaderm "Accept"
            return $ case wh of
              Just "application/xml" -> RtXml
              Just "application/yaml" -> RtYaml
              Just "application/some" -> RtOth undefined
              _ -> RtJson
\end{code}

\begin{code}
      data RtStatus = RtSucc | RtFail Status
      statusH :: RtStatus -> BI.ByteString
      statusH RtSucc = "Success"
      statusH (RtFail _) = "Failed"
      statusC :: RtStatus -> Status
      statusC RtSucc = status200
      statusC (RtFail x) = x
\end{code}


通用成功与失败标志
\begin{code}
      data RtCommon = RtCommonSucc
                    | RtCommonSuccT Text
                    | RtCommonFail Text
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
        toStatus RtCommonSucc = RtSucc
        toStatus (RtCommonSuccT _) = RtSucc
        toStatus (RtCommonFail _) = RtFail status400
\end{code}

拒绝认证（403）
\begin{code}
      data Rt403 = Rt403 Text
        deriving (Eq,Show)
      instance Varable Rt403 where
        toValue (Rt403 t) = object ["auth-msg".=t]
        toNodes (Rt403 t) = [xml|<auth-msg>#{t}|]
      instance Rable Rt403 where
        toWhere _ = RtBody
        toStatus _ = RtFail status403
\end{code}

500
\begin{code}
      data Rt500 = Rt500 SomeException
        deriving (Show)
      instance Varable Rt500 where
        toValue (Rt500 e) = object ["ierror".= show e]
        toNodes (Rt500 e) = [xml|<ierror>#{T.pack(show e)}|]
      instance Rable Rt500 where
         toStatus _ = RtFail status500
         toWhere _ = RtBody
\end{code}
返回服务器信息
\begin{code}
      data RtSvrInfo = RtSvrInfo Text Text Text Text Bool
        deriving (Show,Eq)
      instance Varable RtSvrInfo where
        toValue (RtSvrInfo t s v d _) = object [ "server-time".=t
                                             , "server-info".=s
                                             , "server-version".=v
                                             , "dindo-base-version".=d
                                             ]
        toNodes (RtSvrInfo t s v d _) = [xml|
          <server-time>#{t}
          <server-info>#{s}
          <server-version>#{v}
          <dindo-base-version>#{d}
          |]
      instance Rable RtSvrInfo where
        returnR (x@(RtSvrInfo _ _ _ _ b)) =
          spReturnR statusCode RtBody RtSucc x
          where
            statusCode = if b
              then status404
              else Status 200 "GetSvrInfo"
\end{code}

\begin{code}
      spReturnR sC sW sS x = do
        cTRt <- toCT
        return $ responseLBS sC
          [ ("Status",statusH sS)
          , ("Context-Where",whereH sW)
          , ("Content-Type",toMIME cTRt)
          ] $ toContents cTRt $ x
        where
          toCT = do
            wh <- lookupHeaderm "Accept"
            return $ case wh of
              Just "application/xml" -> RtXml
              Just "application/yaml" -> RtYaml
              Just "application/some" -> RtOth undefined
              _ -> RtJson
          whereH (RtOther x) = x
          whereH RtBody = "Body"
\end{code}
