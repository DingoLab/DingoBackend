




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
\end{code}

\begin{code}
      mkYesodData a b = Yesod.Core.mkYesodData a b'
        where
          b' = b ++ [parseRoutes|/ SvrinfoR GET|]
\end{code}

\begin{code}
      mkSvrinfoR :: T.Text -> Q [Dec]
      mkSvrinfoR info = [d|
        getSvrinfoR :: Yesod site => HandlerT site IO T.Text
        getSvrinfoR = infoR info'
        |]
        where
          info' = T.unpack info

      infoR :: Yesod site
            => T.Text
            -> HandlerT site IO T.Text
      infoR info = do
        addD' <- lookupGetParam "add"
        let addD = fromRational $ toRational $ fromMaybe 0 $ fmap (read.T.unpack) addD'
        now <- (show.addUTCTime addD) <$> liftIO getCurrentTime
        return $ T.decodeUtf8 $ toStrict $ encode $ object
          [ "server-time" .= now
          , "server-info" .= info
          ]
\end{code}
