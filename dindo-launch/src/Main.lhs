




% src/Main.lhs

\begin{code}
{-# LANGUAGE TemplateHaskell
           , DeriveDataTypeable
           #-}
\end{code}

\begin{code}
module Main
    ( main
    ) where
\end{code}

\begin{code}
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
\end{code}

\begin{code}
      data Launch = Launch {form ::String}
        deriving (Show,Data,Typeable)
      launch = Launch{form="json" &= typ "YAML|JSON" &= help "格式"}
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
        cfg' <- cmdArgs launch >>= cfg
        warpDindo cfg' itemWarp
        where
          itemWarp :: Int -> $(std) -> IO()
          itemWarp = warp
      cfg :: Launch -> IO SvrConfig
      cfg l = getContents >>= (return.fromMaybe (error "Invailed config json").decode'.T.encodeUtf8.T.pack)
        where
          decode' = case l of
            Launch "json" -> A.decode.B.fromStrictBS
            Launch "yaml" -> Y.decode
            _ -> error "error form"

\end{code}
