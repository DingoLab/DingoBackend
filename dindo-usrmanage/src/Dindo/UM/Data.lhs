




% src/Dindo/UM/Data.lhs

\begin{code}
module Dindo.UM.Data
    ( RtRegist(..)
    , RtIdy(..)
    , RtIdfed(..)
    , RtCommon(..)
    , RtUImg(..)
    , RtUInfo(..)
    , RtChPsk(..)
    , RtEaddr(..)
    , RtGEadd(..)
    ) where
\end{code}

\begin{code}
      import Dindo.Import.Rable
      import Dindo.Import.Aeson as A
      import Dindo.Import.Yaml as Y
      import Dindo.Import.Text as T
      import Dindo.Import.ByteString as B
      import Dindo.Import.Yesod
      import Dindo.Import.Database
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

通用成功与失败标志
\begin{code}
      data RtCommon = RtCommonSucc
                    | RtCommonSuccT Text
                    | RtCommonFail Text
        deriving (Eq,Show)
      instance Varable RtCommon where
        toValue RtCommonSucc = Null
        toValue (RtCommonSuccT t) = object ["tmp-token" .= t]
        toValue (RtCommonFail x) = String x
        toNodes RtCommonSucc = [xml|null|]
        toNodes (RtCommonSuccT x) = [xml|<tmp-token>#{x}|]
        toNodes (RtCommonFail x) = [xml|<error>#{x}|]
      instance Rable RtCommon where
        toWhere RtCommonSucc = RtBody
        toWhere (RtCommonFail _) = RtBody
        toWhere (RtCommonSuccT _) = RtBody
        toStatus RtCommonSucc = RtSucc
        toStatus (RtCommonSuccT _) = RtSucc
        toStatus (RtCommonFail _) = RtFail
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

更改密码的 返回值
\begin{code}
      data RtChPsk = RtChPsk
                   | RtChPskFail Text
        deriving (Eq)
      instance Show RtChPsk where
        show (RtChPskFail x) = T.unpack x
      instance Varable RtChPsk where
        toValue RtChPsk = Null
        toValue (RtChPskFail x) = object ["error" .= x]
        toNodes RtChPsk = [xml|null|]
        toNodes (RtChPskFail x) = [xml|<error>#{x}|]
      instance Rable RtChPsk where
        toWhere RtChPsk = RtBody
        toWhere (RtChPskFail _) = RtBody
        toStatus RtChPsk = RtSucc
        toStatus (RtChPskFail _) = RtFail
\end{code}

收货地址的增删 的返回值
\begin{code}
      data RtEaddr = RtEaddrAdd Text
                   | RtEaddrChn
                   | RtEaddrDel
                   | RtEaddrFail Text
        deriving (Eq,Show)
      instance Varable RtEaddr where
        toValue (RtEaddrAdd x) = object ["aid" .= x]
        toValue RtEaddrChn = Null
        toValue RtEaddrDel = Null
        toValue (RtEaddrFail x) = object ["error" .= x]
        toNodes (RtEaddrAdd x) = [xml|<aid>#{x}|]
        toNodes RtEaddrChn = [xml|null|]
        toNodes RtEaddrDel = [xml|null|]
        toNodes (RtEaddrFail x) = [xml|<error>#{x}|]
      instance Rable RtEaddr where
        toWhere (RtEaddrAdd _) = RtBody
        toWhere RtEaddrChn = RtBody
        toWhere RtEaddrDel = RtBody
        toWhere (RtEaddrFail _) = RtBody
        toStatus (RtEaddrAdd _) = RtSucc
        toStatus RtEaddrChn = RtSucc
        toStatus RtEaddrDel = RtSucc
        toStatus (RtEaddrFail _) = RtFail
\end{code}

获取地址
\begin{code}
      data RtGEadd = RtGEadd [Addr]
                   | RtGEaddFail Text
        deriving (Eq,Show)
      instance Varable RtGEadd where
        toValue (RtGEadd x) = toJSON x
        toValue (RtGEaddFail x) = object ["error" .= x]
        toNodes (RtGEadd xs) = [xml|
          $forall x <- xs
            <aid>#{addrAid x}
            <addr>#{addrAddr x}
            <zip>#{addrZip x}
          |]
        toNodes (RtGEaddFail x) = [xml|<error>#{x}|]
      instance Rable RtGEadd where
        toWhere (RtGEadd _ ) = RtBody
        toWhere (RtGEaddFail _) = RtBody
        toStatus (RtGEadd _) = RtSucc
        toStatus (RtGEaddFail _) =RtFail
\end{code}
