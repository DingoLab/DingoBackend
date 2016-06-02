




% src/Main.lhs

\begin{code}
module Main
    ( main
    ) where
\end{code}

\begin{code}
      import qualified GHC.IO.Encoding as E
      import System.IO
      import Dindo.Std
      import System.Console.CmdArgs
      import Dindo.Import.Aeson as A
      import Dindo.Import.Yaml as Y
      import Dindo.Import.Yesod
      import Data.Maybe
      import qualified Dindo.Import.ByteString as B
      import qualified Dindo.Import.Text as T
      import Dindo.Common.Yesod.Launch
      import Dindo.Common.Yesod.Config
      import Paths_dindo_launch
      import Data.Version
      import Dindo.Common(dindo_common_version_quasi)
      import Dindo.Import.Database(dindo_database_version_quasi)
      import Control.Exception(try,SomeException,ErrorCall(..),throw,evaluate)
      import Data.Char
      import System.Exit
      import Control.Concurrent
      import System.Signal
\end{code}

启动方式是通过 标准输入流输入，输入的格式是 JSON 或者 是 YAML， “--form=”
这个选项是控制输入或输出的是的，是JSON或者是YAML。
\begin{code}
      data Launch = Launch {form ::String}
        deriving (Show,Data,Typeable)
      launch = Launch{ form="auto" &= typ "AUTO|YAML|JSON" &= help "格式"
                     }
        &= summary ( "dindo-common-"
                  ++ $(dindo_common_version_quasi)
                  ++ "; dindo-database-"
                  ++ $(dindo_database_version_quasi)
                  ++ "; " ++ $(dindo_module_name) ++ "-"
                  ++ $(dindo_module_version)
                  ++ "; dindo-launch-"
                  ++ showVersion version)
\end{code}

\begin{code}
      main :: IO ()
      main = do
#ifndef WithoutUTF8
        E.setLocaleEncoding E.utf8
        hSetEncoding stdout utf8
#endif
        tid <- myThreadId
        installHandler sigINT $ \ sig -> do
          if sig == sigINT
            then do
              putStrLn "going to turn down"
              killThread tid
              exitSuccess
            else putStrLn $ "catch" ++ show sig
        cfg' <- cmdArgs launch >>= cfg
        warpDindo cfg' itemWarp
        where
          itemWarp :: Int -> $(std) -> IO()
          itemWarp = warp
      cfg :: Launch -> IO SvrConfig
      cfg l = getContents >>=
          (decode'.T.encodeUtf8.T.pack)
        where
          tryList :: [a -> SvrConfig] -> [ScError] -> a -> IO SvrConfig
          tryList [] es a = scError.concatWith "\n\t".map getError $ es
          tryList (x:xs) es a = do
            rt <- try.evaluate $ x a :: IO (Either ScError SvrConfig)
            case rt of
              Left e -> tryList xs (e:es) a
              Right sc -> return sc
          getError (ScError a) = a
          concatWith a xs = foldr sig "all failed" xs
            where
              sig x os = x ++ a ++ os
          decJ = fromMaybe (throw $ ScError "Invailed JSON").A.decode.B.fromStrictBS
          decY = fromMaybe (throw $ ScError "Invailed YAML").Y.decode
          decA = tryList [decY,decJ] []
          decode' = let ll = form l in
            case map toLower ll of
              "auto" -> decA
              "json" -> evaluate.decJ
              "yaml" -> evaluate.decY
              _ -> error "error form"

\end{code}
