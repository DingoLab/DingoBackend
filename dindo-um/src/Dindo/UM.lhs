




% src/Dindo/UM.lhs

\begin{code}
module Dindo.UM
    (
    ) where

      import Dindo.UM.Foundation as X
      import Dindo.UM.Handler as X

      import Dindo.Import.TH
      import Paths_dindo_um
      import Data.Version
\end{code}

\begin{code}
      dindo_um_version = showVersion version
      dindo_um_version_quote = stringE version
\end{code}
