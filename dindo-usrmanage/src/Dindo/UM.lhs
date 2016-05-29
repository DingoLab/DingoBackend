




% src/Dindo/UM.lhs

\begin{code}
module Dindo.UM
    ( module X
    , dindo_usrmanage_version
    , dindo_usrmanage_version_quasi
    ) where
      import Dindo.UM.Foundation as X
      import Dindo.UM.Handler as X
      import Dindo.Import.Yesod
      import Dindo.Import.TH
      import Data.Version
      import Paths_dindo_usrmanage

      dindo_usrmanage_version = version
      dindo_usrmanage_version_quasi = stringE $ showVersion version
      mkYesodDispatch "UM" resourcesUM
\end{code}
