




% src/Main.lhs

\begin{code}
module Main
    ( main
    ) where

      import Dindo.Import
      import Dindo.Import.Aeson as A
      import Dindo.Import.Wai
      import Dindo.Import.Yaml as Y
      import Dindo.Import.Yesod
      import qualified Dindo.Import.ByteString as B
      import qualified Dindo.Import.Text as T

      import Dindo.Base
      import Dindo.Config
      import Dindo.Launch
      import Dindo.Std

      import Control.Concurrent
      import Control.Exception(try,SomeException,ErrorCall(..),throw,evaluate)
      import Data.Char
      import System.Exit
      import System.IO
      import qualified GHC.IO.Encoding as E


      import Paths_dindo_launch(version)
      import Data.Version(showVersion)
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
