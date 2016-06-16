




% src/Dindo/UM/Data.lhs

\begin{code}
module Dindo.UM.Data
    (
    ) where

      import Dindo.Rable
      import Dindo.Import.Aeson
      import qualified Dindo.Import.Text as T
      import qualified Dindo.Import.ByteString as B
      import Dindo.Import.Yesod
      -- import Dindo.Database
\end{code}



用户注册返回数据
\begin{code}
      data RtRegist = RtRegist Text
                    | RtRegistFail Text
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
        toWhere _ = RtBody
        toStatus (RtRegist _) = RtSucc Nothing
        toStatus (RtRegistFail _) = RtFail Nothing
\end{code}

返回数据样例：
\begin{json}[caption=注册成功的json格式返回样例]
  ｛ "uid" : "U201604054a8974a8974a8974a8974a8974a8974a8974a897"
   }
\end{json}
\begin{xml}[caption=注册成功的xml格式样例]
<uid>U201604054a8974a8974a8974a8974a8974a8974a8974a897</uid>
\end{xml}


用户认证信息的返回数据
\begin{code}
      data RtIdy = RtIdy
                 | RtIdyFail Text
        deriving (Eq)
      instance Show RtIdy where
        show (RtIdyFail x) = T.unpack x
      instance Varable RtIdy where
        toValue RtIdy = Null
        toValue (RtIdyFail x) = object ["error" .= x]
        toNodes RtIdy  = [xml|null|]
        toNodes (RtIdyFail x) = [xml|<error>#{x}|]
      instance Rable RtIdy where
        toWhere _ = RtBody
        toStatus RtIdy = RtSucc Nothing
        toStatus (RtIdyFail _) = RtFail Nothing
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
        toWhere _ = RtBody
        toStatus RtIdfedPass = RtSucc Nothing
        toStatus RtIdfedNo = RtSucc Nothing
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
        toWhere RtUInfoNSU = RtOther "Context"
        toStatus RtUInfo{..} = RtSucc Nothing
        toStatus RtUInfoNSU = RtFail Nothing
\end{code}



登录
\begin{code}
      data RtLogin = RtLoginSucc Text Text
                   | RtLoginFail Text
        deriving (Show,Eq)
      instance Varable RtLogin where
        toValue (RtLoginSucc u t) = object ["uid".=u,"tmp-token".=t]
        toValue (RtLoginFail e) = object ["error" .= e]
        toNodes (RtLoginFail e) = [xml|<error>#{e}|]
        toNodes (RtLoginSucc u t) =[xml|
          <uid>#{u}
          <tmp-token>#{t}
          |]
      instance Rable RtLogin where
        toWhere _ = RtBody
        toStatus (RtLoginSucc _ _) = RtSucc Nothing
        toStatus (RtLoginFail _) = RtFail Nothing
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
        toWhere _ = RtBody
        toStatus RtChPsk = RtSucc Nothing
        toStatus (RtChPskFail _) = RtFail Nothing
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
        toValue (RtEaddrFail x) = object ["error" .= x]
        toValue _ = Null
        toNodes (RtEaddrAdd x) = [xml|<aid>#{x}|]
        toNodes (RtEaddrFail x) = [xml|<error>#{x}|]
        toNodes _ = [xml|null|]
      instance Rable RtEaddr where
        toWhere _ = RtBody
        toStatus (RtEaddrFail _) = RtFail Nothing
        toStatus _ = RtSucc Nothing
\end{code}


获取地址
\begin{code}
      data RtGEadd = RtGEadd [(Text,Text,Text)]
                   | RtGEaddFail Text
        deriving (Eq,Show)
      instance Varable RtGEadd where
        toValue (RtGEadd x) = toJSON $
          map (\(a,b,c)->object ["aid".=a,"addr".=b,"zip".=c]) x
        toValue (RtGEaddFail x) = object ["error" .= x]
        toNodes (RtGEadd xs = [xml|
          $forall (a,b,c) <- xs
            <aid>#{a}
            <addr>#{b}
            <zip>#{c}
          |]
        toNodes (RtGEaddFail x) = [xml|<error>#{x}|]
      instance Rable RtGEadd where
        toWhere _ = RtBody
        toStatus (RtGEadd _) = RtSucc Nothing
        toStatus (RtGEaddFail _) = RtFail Nothing
\end{code}
