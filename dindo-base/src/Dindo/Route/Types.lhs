




% src/Dindo/Route/Types.lhs

\begin{code}
module Dindo.Route.Types where

      import Data.Yaml
      import Dindo.Base(dindo_base_version_quote)
      import Network.HTTP.Types
      import Network.Wai
      import Language.Haskell.TH
      import Language.Haskell.TH.Quote
      import Dindo.RIO
      import Data.Time
      import Dindo.Rable
      import Dindo.Exception
      import Network.Wai
      import Network.HTTP.Types
      import Dindo.Import.Text (showT,pack)
      import Dindo.Logger(LoggerSet,apLogger)
\end{code}

\begin{code}
      data RouInfo = RouInfo
          { rouInfo :: String
          , metInfo :: String
          , autInfo :: String
          , funInfo :: String
          }
        | RouInfoWild String String String
      fromList :: [String] -> Maybe RouInfo
      fromList [a,b,c,d] = Just $ RouInfo a b c d
      fromList _  = Nothing
      mapList :: [String] -> [RouInfo]
      mapList [] = [RouInfo "/" "GET" "noAuth" "getSvrInfoR",RouInfoWild "GET" "noAuth" "getSvrInfoRE"]
      mapList (x:xs) = case (fromList $ words x) of
        Just y -> y:mapList xs
        Nothing -> mapList xs
      dindo :: QuasiQuoter
      dindo = QuasiQuoter undefined undefined undefined mkDindo
\end{code}

\begin{code}
      mkSvrInfoR :: String -> String -> Bool -> RIO cfg Response
      mkSvrInfoR s v b = do
        time <- liftIO getCurrentTime
        returnR $ RtSvrInfo (showT time) (pack s) (pack v) $(dindo_base_version_quote) b
\end{code}

\begin{code}
      checkMethod :: Method -> cfg -> LoggerSet -> Request -> (RIO cfg Response -> RIO cfg Response) -> RIO cfg Response -> IO Response
      checkMethod met cfg logset req auth fm =
        runRIO (RD req cfg logset) $ apLogger $ if met == requestMethod req
          then catchRIO fdo $ \e -> returnR $ Rt500 e
          else returnR $ Rt403 "I do not know you!"
        where
          fdo = auth $ fm
      checkGET = checkMethod "GET"
      checkPOST = checkMethod "POST"
\end{code}

\begin{code}
      mkDindo :: String -> Q [Dec]
      mkDindo str = return
        [ SigD (mkName "getSvrInfoR") (AppT (AppT (ConT (mkName "RIO")) (VarT (mkName "cfg"))) (ConT (mkName "Response")))
        , FunD (mkName "getSvrInfoR") [Clause [] (NormalB (AppE (AppE (AppE (VarE (mkName "mkSvrInfoR")) (VarE (mkName "dindom_name"))) (VarE (mkName "dindom_version"))) (ConE (mkName "False")) ))[]]
        , SigD (mkName "getSvrInfoRE") (AppT (AppT (ConT (mkName "RIO")) (VarT (mkName "cfg"))) (ConT (mkName "Response")))
        , FunD (mkName "getSvrInfoRE") [Clause [] (NormalB (AppE (AppE (AppE (VarE (mkName "mkSvrInfoR")) (VarE (mkName "dindom_name"))) (VarE (mkName "dindom_version"))) (ConE (mkName "True")) ))[]]
        , SigD (mkName "stdDindo") (AppT (AppT ArrowT (VarT (mkName "cfg"))) (AppT (AppT ArrowT (ConT (mkName "LoggerSet"))) (ConT (mkName "Application"))))
        , FunD (mkName "stdDindo") [Clause [VarP (mkName "c"),VarP (mkName "l")]
            (NormalB (LamE [VarP (mkName "request"),VarP (mkName "respond")]
              (CaseE (AppE (VarE (mkName "rawPathInfo")) (VarE (mkName "request")))  (map mkCase rinfos))))
            []
          ]
        ]
        where
          rinfos = mapList.lines $ str
          mkCase (RouInfoWild metInfo autInfo funInfo) = Match WildP
            (NormalB (InfixE (Just (VarE (mkName "respond"))) (VarE (mkName "=<<"))
              (Just
              (AppE
                (AppE
                  (AppE
                    (AppE
                      (AppE (VarE (mkName $ "check" ++ metInfo))
                            (VarE (mkName "c")))
                      (VarE (mkName "l")))
                    (VarE (mkName "request")))
                  (VarE (mkName autInfo)))
                (VarE (mkName funInfo))))))
            []
          mkCase RouInfo{..} = Match (LitP $ StringL rouInfo)
            (NormalB (InfixE (Just (VarE (mkName "respond"))) (VarE (mkName "=<<"))
              (Just
              (AppE
                (AppE
                  (AppE
                    (AppE
                      (AppE (VarE (mkName $ "check" ++ metInfo))
                            (VarE (mkName "c")))
                      (VarE (mkName "l")))
                    (VarE (mkName "request")))
                  (VarE (mkName autInfo)))
                (VarE (mkName funInfo))))))
            []
\end{code}
