




% src/Dindo/UM/Data.lhs

\begin{code}
{-# LANGUAGE OverloadedStrings
           , QuasiQuotes
           #-}
\end{code}

\begin{code}
module Dindo.UM.Data
    (
    ) where
\end{code}

\begin{code}
      import Dindo.Import.Rable
      import Dindo.Import.Aeson as A
      import Dindo.Import.Yaml as Y
      import Dindo.Import.Text as T
\end{code}

用户注册返回数据
\begin{code}
      data RtRegist = RtRegist
          { uid :: Text
          }
        | RtRegistFail
          { reason :: Text
          }
        deriving (Eq)
      instance Show RtRegist where
        show (RtRegist x) = unpack x
        show (RtRegistFail x) = unpack x
      instance Varable RtRegist where
        toValue (RtRegist x) = object ["uid" .= x]
        toValue (RtRegistFail x) = object ["error" .= x]
        toNodes (RtRegist x) = [xml|<uid>#{x}|]
        toNodes (RtRegistFail x) =[xml|<error>#{x}|]
      instance Rable RtRegist where
        toWhere (RtRegist _) = RtBody
        toWhere (RtRegistFail _) = RtOther "CONTEXT"
        toStatus (RtRegist _) = RtSucc
        toStatus (RtRegistFail _) = RtFail
\end{code}
