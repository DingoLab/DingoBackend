




% src/Main.lhs

\begin{code}
module Main
    ( main
    ) where
\end{code}

\begin{code}
      import Dindo.Std
      import Dindo.Common.Yesod.Launch
      import Dindo.Common.Yesod.Config
\end{code}

\begin{code}
      main :: IO ()
      main = do
        cfg' <- cfg
        warpDindo cfg' itemWarp
        where
          itemWarp :: Int -> Std -> IO()
          itemWarp = warp
          cfg :: SvgConfig
          cfg = getContent >>= (return.encode)
\end{code}
