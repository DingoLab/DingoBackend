




% src/Dindo/Std.lhs

\begin{code}
module Dindo.Std
    (
    ) where

      import Dindo.UM as X

      dindom_version = dindo_um_version
      dindom_version_quote = dindo_um_version_quote
      dindom_name = "dindo-um"
      dindom_name_quote = stringE dindom_name

      std = [t|UM|]
      cfg = [t|UMCfg]
\end{code}
