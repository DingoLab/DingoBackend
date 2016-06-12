




% src/Dindo/UM/Data.lhs

\begin{code}
module Dindo.UM.Data
    ( Handler
    , RtRegist(..)
    ) where

      import Dindo.Import.Rable
      import Dindo.Import.Text as T
      import Dindo.Import.Aeson
      import Dindo.Base
      import Dindo.UM.Config
\end{code}

\begin{code}
      type Handler = HandlerT UMRdConfig
\end{code}

\begin{code}
      data RtRegist = RtRegist Text
        deriving (Eq,Show)
      instance Varable RtRegist where
        toValue (RtRegist uid) = object ["uid" .= uid]
        toNodes (RtRegist uid) = [xml|<uid>#{uid}|]
      instance Rable RtRegist where
        toWhere _ = RtBody
        toStatus _ = RtSucc
\end{code}
