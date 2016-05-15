




% src/Dindo/Std.lhs
\begin{code}
{-# LANGUAGE TemplateHaskell #-}
\end{code}

\begin{code}
module Dindo.Std
    ( module X
    , std
    ) where

      import Dindo.UM as X -- need change
      import Dindo.Import.TH
      std = [t|UM|]
\end{code}
