




% src/Dindo/Import/Yesod.lhs

% 导出 Yesod


\begin{code}
module Dindo.Import.Yesod
    ( module X
    , mkYesodData
    , mkShomeR
    ) where
\end{code}

\begin{code}
      import Yesod as X hiding (mkYesodData)
      import qualified Yesod (mkYesodData)
      import Dindo.Common.Rable as X
      import Dindo.Common.Auth as X
      import Dindo.Common.Yesod.Launch as X
      import Dindo.Common.Yesod.Config as X
      import Dindo.Import.TH
      import Data.Maybe
      import Data.Time
      import Data.Text
      import qualified Data.Text.Encoding as TE
\end{code}

\begin{code}
      mkYesodData a b = Yesod.mkYesodData a b'
        where
          b' = b ++ [parseRoutes|/ ShomeR GET|]
      mkShomeR :: Text -> Q [Dec]
      mkShomeR info = [d|
        getShomeR :: Yesod site => HandlerT site IO Text
        getShomeR = do
          addD' <- lookupGetParam "add"
          let addD = fromRational $ toRational $ fromMaybe 0 $ fmap (read.unpack) addD'
          now <- fmap (show.addUTCTime addD) $ liftIO getCurrentTime
          let rtinfo = object
            [ "server-time" .= now
            , "server-info" .= info
            ]
          return $ ET.decodeUtf8 $ toStrict $ encode rtinfo
        |]
\end{code}
