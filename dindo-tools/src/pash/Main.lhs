




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
        Pash key t at <- cmdArgs pash
        now' <- getCurrentTime
        let now = addUTCTime (fromIntegral at) now
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
\paragraph{dindo-pash 使用说明}
\label{tools:pash:help}
一共有两个参数：一个是密码，另一个是散列方式，也就是认证方式。
\begin{description}
  \item[100] 注册时
  \item[0] 使用 uid 登录时
  \item[1] 使用 name 登录时
  \item[2] 使用 tel 登录时
\end{description}
有一个 flag 开关是关于时间矫正的，矫正单位是秒。
\begin{code}
      data Pash = Pash {pKey :: String,pType :: Int,aTime :: Int}
        deriving (Show,Data,Typeable)
      pash =  Pash
        { pKey = def &= args &= typ "PASSWORD" &= help "密码"
        , pType = def &= args &= typ "Identify type" &= help "认证方式"
        , aTime = 0 &= typ "UTCDiffTime" &= help "时间矫正"
        } &= summary ( "dindo-common:-"
                    ++ $(dindo_common_version_quasi)
                    ++ "; dindo-tools-"
                    ++ showVersion version
                    )
\end{code}
