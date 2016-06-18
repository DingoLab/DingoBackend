




% src/Dindo/Base.lhs

\begin{code}
module Dindo.Base
    ( dindo_base_version
    , dindo_base_version_quote
    , getUid
    , getTmpToken
    , pickF
    ) where

      import Data.Version
      import Paths_dindo_base

      import Dindo.Import
      import Dindo.Import.TH
      import Dindo.Import.Yesod
      import qualified Dindo.Import.ByteString as B
      import qualified Dindo.Import.Text as T
\end{code}


\begin{code}
      dindo_base_version :: String
      dindo_base_version = showVersion version
      dindo_base_version_quote :: Q Exp
      dindo_base_version_quote = stringE dindo_base_version
\end{code}


\begin{code}
      getUid :: HandlerT site IO T.Text
      getUid = lookupHeader "Uid" >>= (return.T.decodeUtf8.fromMaybe (error "no uid head"))
\end{code}

\begin{code}
      getTmpToken :: HandlerT site IO T.Text
      getTmpToken = lookupHeader "Tmp-Token" >>= (return.T.decodeUtf8.fromMaybe (error "no tmp-token head"))
\end{code}

\begin{code}
      pickF :: T.Text -> (Maybe a,T.Text) -> (T.Text,Maybe a)
      pickF _ (Nothing,str) = ("",Nothing)
      pickF y (Just x,str) = (T.unwords [y,str,"= ?"],Just x)
\end{code}
