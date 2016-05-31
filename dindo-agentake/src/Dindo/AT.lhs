




% src/Dindo/AT.lhs

\begin{code}
module Dindo.AT
    ( module X
    , dindo_agentake_version
    , dindo_agentake_version_quasi
    ) where
      import Dindo.AT.Foundation as X
      import Dindo.AT.Handler as X
      import Dindo.Import.Yesod
      import Dindo.Import.TH
      import Data.Version
      import Paths_dindo_agentake
\end{code}

\begin{code}
      dindo_agentake_version :: Version
      dindo_agentake_version = version
      dindo_agentake_version_quasi :: Q Exp
      dindo_agentake_version_quasi = stringE $ showVersion version
      mkYesodDispatch "AT" resourcesAT
\end{code}
