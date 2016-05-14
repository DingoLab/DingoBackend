




% src/Dindo/UM/Data.lhs

\begin{code}
{-# LANGUAGE OverloadedStrings
           , QuasiQuotes
           , RecordWildCards
           #-}
\end{code}

\begin{code}
module Dindo.UM.Data
    ( RtRegist(..)
    , RtIdy(..)
    , RtIdfed(..)
    , RtCommonSucc(..)
    , RtUImg(..)
    , RtUInfo(..)
    ) where
\end{code}

\begin{code}
      import Dindo.Import.Rable
      import Dindo.Import.Aeson as A
      import Dindo.Import.Yaml as Y
      import Dindo.Import.Text as T
      import Dindo.Import.ByteString as B
      import Dindo.Import.Yesod
\end{code}

用户注册返回数据
\begin{code}
      data RtRegist = RtRegist
          { uid :: Text
          }
        | RtRegistFail
          { regReason :: Text
          }
        deriving (Eq)
      instance Show RtRegist where
        show (RtRegist x) = T.unpack x
        show (RtRegistFail x) = T.unpack x
      instance Varable RtRegist where
        toValue (RtRegist x) = object ["uid" .= x]
        toValue (RtRegistFail x) = object ["error" .= x]
        toNodes (RtRegist x) = [xml|<uid>#{x}|]
        toNodes (RtRegistFail x) =[xml|<error>#{x}|]
      instance Rable RtRegist where
        toWhere (RtRegist _) = RtBody
        toWhere (RtRegistFail _) = RtBody
        toStatus (RtRegist _) = RtSucc
        toStatus (RtRegistFail _) = RtFail
\end{code}

用户认证信息的返回数据
\begin{code}
      data RtIdy = RtIdy
        | RtIdyFail
          { idyReason :: Text
          }
        deriving (Eq)
      instance Show RtIdy where
        show (RtIdyFail x) = T.unpack x
      instance Varable RtIdy where
        toValue RtIdy = Null
        toValue (RtIdyFail x) = object ["error" .= x]
        toNodes RtIdy  = [xml|null|]
        toNodes (RtIdyFail x) = [xml|<error>#{x}|]
      instance Rable RtIdy where
        toWhere (RtIdyFail _) = RtBody
        toWhere RtIdy = RtBody
        toStatus RtIdy = RtSucc
        toStatus (RtIdyFail _) = RtFail
\end{code}

用户查询认证状态信息
\begin{code}
      data RtIdfed = RtIdfedPass | RtIdfedNo
        deriving (Eq,Show)
      instance Varable RtIdfed where
        toValue RtIdfedPass = object ["status" .= ("pass"::Text)]
        toValue RtIdfedNo   = object ["status" .= ("no"::Text)]
        toNodes RtIdfedPass = [xml|<status>pass|]
        toNodes RtIdfedNo   = [xml|<status>no|]
      instance Rable RtIdfed where
        toWhere RtIdfedPass = RtBody
        toWhere RtIdfedNo = RtBody
        toStatus RtIdfedPass = RtSucc
        toStatus RtIdfedNo = RtSucc
\end{code}

通用成功标志
\begin{code}
      data RtCommonSucc = RtCommonSucc
        deriving (Eq,Show)
      instance Varable RtCommonSucc where
        toValue RtCommonSucc = Null
        toNodes RtCommonSucc = [xml|null|]
      instance Rable RtCommonSucc where
        toWhere RtCommonSucc = RtBody
        toStatus RtCommonSucc = RtSucc
\end{code}

用户信息查询返回结果
\begin{code}
      data RtUInfo = RtUInfo
          { rtuiUid :: Text
          , rtuiName :: Text
          , rtuiTel :: Text
          , rtuiEmail :: Text
          }
        | RtUInfoNSU
      instance Show RtUInfo where
        show RtUInfoNSU = "no such a user"
      instance Varable RtUInfo where
        toValue RtUInfo{..} = object
          [ "uid" .= rtuiUid
          , "name" .= rtuiName
          , "tel" .= rtuiTel
          , "email" .= rtuiEmail
          ]
        toNodes RtUInfo{..} = [xml|
        <uid> #{rtuiUid}
        <name> #{rtuiName}
        <tel> #{rtuiTel}
        <email> #{rtuiEmail}
        |]
      instance Rable RtUInfo where
        toWhere RtUInfo{..} = RtBody
        toWhere RtUInfoNSU = RtOther "CONTEXT"
        toStatus RtUInfo{..} = RtSucc
        toStatus RtUInfoNSU = RtFail
\end{code}

获取用户头像返回内容
\begin{code}
      data RtUImg = RtUImg ByteString
                  | RtUImgFail
        deriving (Eq)
      instance Show RtUImg
      instance Varable RtUImg
      instance Rable RtUImg where
        returnR (RtUImg img) =
          selectRep $ provideRepType "image/png" $ return img
        returnR RtUImgFail = do
          addHeader "CONTEXT-WHERE" "CONTEXT"
          addHeader "CONTEXT" "Failed on get image"
          selectRep $ provideRep $ return (""::Text)

\end{code}
