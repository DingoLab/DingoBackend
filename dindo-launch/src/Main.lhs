




% src/Main.lhs

\begin{code}
module Main
    ( main
    ) where

      import Dindo.Import
      import Dindo.Import.Aeson as A
      import Dindo.Import.Yaml as Y
      import Dindo.Import.Yesod
      import qualified Dindo.Import.ByteString as B
      import qualified Dindo.Import.Text as T
      import qualified Dindo.Import.Wai as W

      import Dindo.Base
      import Dindo.Config
      import Dindo.Std

      import Control.Concurrent
      import Control.Exception(try,SomeException(..),ErrorCall(..),throw,evaluate)
      import Data.Char
      import Data.String
      import System.Console.CmdArgs
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
                  ++ $(dindo_base_version_quote)
                  ++ "; " ++ $(dindom_name_quote) ++ "-"
                  ++ $(dindom_version_quote)
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
        cfg' <- cmdArgs launch >>= config
        let settings = setSet cfg' W.defaultSettings
        toCfgD cfg' >>= toWaiApp >>= (W.runSettings settings)
        where
          setSet cfg' = W.setPort (getPort cfg')
                      . W.setHost (fromString $ getListenType cfg')
                      . W.setTimeout (getTimeout cfg')
                      . W.setServerName (getServerName cfg')
                      . W.setInstallShutdownHandler (shutDown cfg')
      config :: Launch -> IO $(cfg)
      config l = getContents >>=
          (decode'.T.encodeUtf8.T.pack)
        where
          tryList :: [a -> $(cfg)] -> [SomeException] -> a -> IO $(cfg)
          tryList [] es a = error.concatWith "\n\t".map getError $ es
          tryList (x:xs) es a = do
            rt <- try.evaluate $ x a :: IO (Either SomeException $(cfg))
            case rt of
              Left e -> tryList xs (e:es) a
              Right sc -> return sc
          getError (SomeException a) = show a
          concatWith a xs = foldr sig "all failed" xs
            where
              sig x os = x ++ a ++ os
          decJ = fromMaybe (error "Invailed JSON").A.decode.B.fromStrict
          decY = fromMaybe (error "Invailed YAML").Y.decode
          decA = tryList [decY,decJ] []
          decode' = let ll = form l in
            case map toLower ll of
              "auto" -> decA
              "json" -> evaluate.decJ
              "yaml" -> evaluate.decY
              _ -> error "error form"
\end{code}
