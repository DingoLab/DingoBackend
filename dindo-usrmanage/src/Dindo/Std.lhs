




% src/Dindo/Std.lhs

\begin{code}
module Dindo.Std
    ( module X
    , std
    , dindo_module_name
    , dindo_module_version
    ) where

      import Dindo.UM as X -- need change
      import Dindo.Import.TH
\end{code}

\begin{code}
      dindo_module_name = stringE "dindo-usrmanage"
      dindo_module_version = dindo_usrmanage_version_quasi
      std = [t|UM|]
\end{code}
