




% src/Dindo/UM.lhs

\begin{code}
module Dindo.UM
    ( dindo_um_version
    , dindo_um_version_quote
    , stringE
    , UM(..)
    , UMCfg
    ) where

      import Dindo.UM.Foundation as X
      import Dindo.UM.Handler as X

      import Dindo.Import.TH
      import Dindo.Import.Yesod
      import Paths_dindo_um
      import Data.Version
\end{code}

\begin{code}
      dindo_um_version = showVersion version
      dindo_um_version_quote = stringE dindo_um_version
\end{code}

\begin{code}
      mkYesodDispatch "UM" resourcesUM
\end{code}
