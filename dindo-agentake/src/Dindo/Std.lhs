




% src/Dindo/Std.lhs

\begin{code}
module Dindo.Std
    ( module X
    , std
    , dindo_module_name
    , dindo_module_version
    ) where

      import Dindo.AT as X
      import Dindo.Import.TH
\end{code}

\begin{code}
      dindo_module_name :: Q Exp
      dindo_module_name = stringE "dindo-agentake"
      dindo_module_version :: Q Exp
      dindo_module_version = dindo_agentake_version_quasi
\end{code}
