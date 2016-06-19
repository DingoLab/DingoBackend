




% src/Dindo/Import/Yesod.lhs

\begin{code}
module Dindo.Import.Yesod
    ( module X
    , mkYesodData
    , mkSvrinfoR
    ) where

      import Yesod.Core as X hiding (mkYesodData)
      import qualified Yesod.Core (mkYesodData)
      import Yesod.Core.Handler as X

      import Dindo.Import
      import Dindo.Import.Aeson
      import Dindo.Import.ByteString(toStrict)
      import Dindo.Import.TH
      import qualified Dindo.Import.Text as T

      import Dindo.Rable
\end{code}

\begin{code}
      mkYesodData a b = Yesod.Core.mkYesodData a b'
        where
          b' = b ++ [parseRoutes|/ SvrinfoR GET|]
\end{code}

\begin{code}
      mkSvrinfoR :: T.Text -> Q [Dec]
      mkSvrinfoR info = [d|
        getSvrinfoR :: Yesod site => HandlerT site IO TypedContent
        getSvrinfoR = infoR info'
        |]
        where
          info' = T.unpack info

      infoR :: Yesod site
            => T.Text
            -> HandlerT site IO TypedContent
      infoR info = do
        addD' <- lookupGetParam "add"
        let addD = fromRational $ toRational $ fromMaybe 0 $ fmap (read.T.unpack) addD'
        now <- (T.showT.addUTCTime addD) <$> liftIO getCurrentTime
        returnR $ RtSvrinfo now info
\end{code}
