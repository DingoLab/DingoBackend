




% Dindo

\begin{code}
module Main
    ( main
    ) where

      import Dindo.Std
      import Dindo.Config
      import Dindo.Exception(ScError(..))
      import Dindo.Import.Wai hiding(getPort)
      import System.Console.CmdArgs
      import qualified GHC.IO.Encoding as E
      import System.IO
      import Dindo.Import.Log
      import Dindo.Import.Aeson as A
      import Dindo.Import.Yaml as Y
      import Dindo.Import.Text as T hiding (map,foldr)
      import Dindo.Import.ByteString as B hiding (map,foldr,getContents)
      import Paths_dindo_launch
      import Data.Version
      import Control.Exception(try,SomeException,ErrorCall(..),throw,evaluate)
      import Data.Char as C
      import Data.Maybe
\end{code}



启动方式是通过 标准输入流输入，输入的格式是 JSON 或者 是 YAML， “--form=”
这个选项是控制输入或输出的是的，是JSON或者是YAML。
\begin{code}
      data Launch = Launch {form ::String}
        deriving (Show,Data,Typeable)
      launch = Launch{form="auto" &= typ "AUTO|YAML|JSON" &= help "格式"}
        &= summary ( {-"dindo-base-"
                  ++ $(dindo_common_version_quasi)
                  ++ "; " ++ $(dindo_module_name) ++ "-"
                  ++ $(dindo_module_version)
                  ++ "; -} "dindo-launch-"
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
        ls <- case getLogPath cfg' of
          "stdout" -> newStdoutLoggerSet defaultBufSize
          "stderr" -> newStderrLoggerSet defaultBufSize
          x -> newFileLoggerSet defaultBufSize x
        rdCfg <- toRdCfg cfg'
        run (getPort cfg') $ stdDindo rdCfg ls
      cfg :: Launch -> IO $(stdT)
      cfg l = getContents >>= (decode'.T.encodeUtf8.T.pack)
        where
          tryList ::  [a -> $(stdT)] -> [ScError] -> a -> IO $(stdT)
          tryList [] es a = throw.ScError .concatWith "\n\t".map getError $ es
          tryList (x:xs) es a = do
            rt <- try.evaluate $ x a :: IO (Either ScError $(stdT))
            case rt of
              Left e -> tryList xs (e:es) a
              Right sc -> return sc
          getError (ScError a) = a
          concatWith a xs = foldr sig "all failed" xs
            where
              sig x os = x ++ a ++ os
          decJ = fromMaybe (throw $ ScError "Invailed JSON").A.decode.B.fromStrict
          decY = fromMaybe (throw $ ScError "Invailed YAML").Y.decode
          decA = tryList [decY,decJ] []
          decode' = let Launch ll = l in
            case map C.toLower ll of
              "auto" -> decA
              "json" -> evaluate.decJ
              "yaml" -> evaluate.decY
              _ -> error "error form"
\end{code}
