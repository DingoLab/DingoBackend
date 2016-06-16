




% src/Dindo/Import/Yesod.lhs

\begin{code}
module Dindo.Import.Yesod
    ( module X
    , mkYesodData
    , mkSvrinfoR
    ) where

      import Yesod.Core as X hiding (mkYesodData)
      import qualified Yesod.Core (mkYesodData)
      import Yesod.Handler as X

      import Dindo.Import.TH
\end{code}

\begin{code}
      mkYesodData a b = Yesod.Core.mkYesodData a b'
        where
          b' = b ++ [parseRoutes|/ SvrinfoR GET|]
\end{code}

\begin{code}
      mkSvrinfoR :: Text -> Q [Dec]
      mkSvrinfoR info = [d|
        getSvrinfoR :: Yesod site => HandlerT site IO Text
        getSvrinfoR = infoR info
        |]

      infoR :: Yesod site
            => Text
            -> HandlerT site IO Text
      infoR info = do
        addD' <- lookupGetParam "add"
        let addD = fromRational $ toRational $ fromMaybe 0 $ fmap (read.unpack) addD'
        now <- fmap (show.addUTCTime addD) $ liftIO getCurrentTime
        return $ TE.decodeUtf8 $ toStrict $ encode $ object
          [ "server-time" .= now
          , "server-info" .= info
          ]
\end{code}
