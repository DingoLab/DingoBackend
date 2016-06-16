




% src/Dindo/Base.lhs

\begin{code}
module Dindo.Base
    ( dindo_base_version
    , dindo_base_version_quote
    ) where

      import Data.Version
      import Dindo.Import.TH
      import Paths_dindo_base

      import qualified Dindo.Import.ByteString as B
\end{code}


\begin{code}
      dindo_base_version :: String
      dindo_base_version = showVersion version
      dindo_base_version_quote :: Q Exp
      dindo_base_version_quote = stringE dindo_base_version
\end{code}


\begin{code}
      getUid :: HandlerT site IO Text
      getUid = lookupHeader "Uid" >>= (return.decodeUtf8.fromMaybe (error "no uid head"))
\end{code}

\begin{code}
      getTmpToken :: HandlerT site IO Text
      getTmpToken = lookupHeader "Tmp-Token" >>= (return.decodeUtf8.fromMaybe (error "no tmp-token head"))
\end{code}

\begin{code}
      pickF :: Text -> (Maybe a,Text) -> (Text,Maybe a)
      pickF _ (Nothing,str) = ("",Nothing)
      pickF x (Just x,str) = (B.unwords [x,str,"= ?"],Just x)
\end{code}
