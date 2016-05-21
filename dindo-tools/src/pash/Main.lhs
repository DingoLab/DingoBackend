




% src/pash/Main.lhs
产生密钥 的工具

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
\end{code}

\begin{code}
      main :: IO ()
      main = do
        (key:typ':_) <- getArgs
        now <- getCurrentTime
        let typ = read typ'
        pash <- getPash typ key now
        a' <- getContents
        let a = concat.lines $ a'
        case typ of
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
