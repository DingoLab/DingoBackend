




% src/Dindo/Route/Types.lhs

\begin{code}
module Dindo.Route.Types where

      import Data.Yaml
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
      import Dindo.Import.Text (showT)
      import Dindo.Logger(LoggerSet,apLogger)
\end{code}

\begin{code}
      data RouInfo = RouInfo
        { rouInfo :: String
        , metInfo :: String
        , autInfo :: String
        , funInfo :: String
        }
      fromList :: [String] -> Maybe RouInfo
      fromList [a,b,c,d] = Just $ RouInfo a b c d
      fromList _  = Nothing
      mapList :: [String] -> [RouInfo]
      mapList [] = [RouInfo "/svrtime" "GET" "noAuth" "getSvrTimeR"]
      mapList (x:xs) = case (fromList $ words x) of
        Just y -> y:mapList xs
        Nothing -> mapList xs
      dindo :: QuasiQuoter
      dindo = QuasiQuoter undefined undefined undefined mkDindo
\end{code}

\begin{code}
      getSvrTimeR :: RIO cfg Response
      getSvrTimeR = do
        time <- liftIO getCurrentTime
        returnR $ RtCommonSuccT $ showT time
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
        [ SigD (mkName "stdDindo") (AppT (AppT ArrowT (VarT (mkName "cfg"))) (AppT (AppT ArrowT (ConT (mkName "LoggerSet"))) (ConT (mkName "Application"))))
        , FunD (mkName "stdDindo") [Clause [VarP (mkName "c"),VarP (mkName "l")]
            (NormalB (LamE [VarP (mkName "request"),VarP (mkName "respond")]
              (CaseE (AppE (VarE (mkName "rawPathInfo")) (VarE (mkName "request")))  (map mkCase rinfos))))
            []
          ]
        ]
        where
          rinfos = mapList.lines $ str
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
