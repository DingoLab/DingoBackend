




% src/pash/Main.lhs
产生密钥 的工具

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
      import System.Environment
      import Dindo.Import
      import Dindo.Common.Auth
      import Dindo.Import.Digest
      import qualified Dindo.Import.Text as T
      import qualified Dindo.Import.ByteString as B
      import Dindo.Common(dindo_common_version_quasi)
      import Data.Version
      import System.Console.CmdArgs
      import Paths_dindo_tools
\end{code}

\begin{code}
      main :: IO ()
      main = do
        Pash key t <- cmdArgs pash
        now <- getCurrentTime
        pash <- getPash t key now
        a' <- getContents
        let a = concat.lines $ a'
        case t of
          100 -> putStr $ a ++ " -d \"pash="++pash++"\""
          _ -> putStr $ a ++ " -d \"pash="++pash++"\" -H \"TIME-STAMP:"++show now++"\""
        return ()
        where
          getPash typ key now = case typ of
            100 -> return $ showDigest $ sha256 $ B.fromStrictBS $ T.encodeUtf8 $ T.pack key
            x -> do
              let k = T.pack $ showDigest $ sha256 $ B.fromStrictBS $ T.encodeUtf8 $ T.pack key
              let time = T.encodeUtf8.T.pack.show $ now
              return $ T.unpack $ runPash x time k
\end{code}

\begin{code}
      data Pash = Pash {pKey :: String,pType :: Int}
        deriving (Show,Data,Typeable)
      pash =  Pash
        { pKey = def &= args &= typ "PASSWORD"
        , pType = def &= args &= typ "Identify type"
        } &= summary ( "dindo-common:-"
                    ++ $(dindo_common_version_quasi)
                    ++ "; dindo-tools-"
                    ++ showVersion version
                    )
\end{code}
