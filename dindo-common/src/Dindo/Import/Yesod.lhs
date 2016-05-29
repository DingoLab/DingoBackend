




% src/Dindo/Import/Yesod.lhs

% 导出 Yesod


\begin{code}
module Dindo.Import.Yesod
    ( module X
    , mkYesodData
    , mkGetSvrInfoAuthor
    , getSvrtimeR
    , mkSvrinfoR
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
\end{code}

\begin{code}
      mkYesodData a b = Yesod.mkYesodData a b'
        where
          b' = b ++ [parseRoutes|
            /svrtime SvrtimeR GET
            /svrinfo SvrinfoR GET
            |]
      mkGetSvrInfoAuthor :: Q [Dec]
      mkGetSvrInfoAuthor = return $
        [FunD (mkName "isAuthorized") [Clause [VarP (mkName "GettimeR"),WildP] (NormalB (AppE (VarE (mkName "return")) (ConE (mkName "Authorized")))) []]
        ,FunD (mkName "isAuthorized") [Clause [VarP (mkName "GetinfoR"),WildP] (NormalB (AppE (VarE (mkName "return")) (ConE (mkName "Authorized")))) []]
        ]
      getSvrtimeR :: Yesod site => HandlerT site IO Text
      getSvrtimeR = do
        addD' <- lookupGetParam "add"
        let addD = fromRational $ toRational $ fromMaybe 0 $ fmap (read.unpack) addD'
        now <- liftIO getCurrentTime
        return $ pack $ show $ addUTCTime addD now
      mkSvrinfoR :: Text -> Q [Dec]
      mkSvrinfoR info = [d|
        getSvrinfoR :: Yesod site => HandlerT site IO Text
        getSvrinfoR = return info
        |]
\end{code}
