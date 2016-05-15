




% src/Dindo/Common.lhs

\begin{code}
module Dindo.Common
    ( dindo_common_version
    , dindo_common_version_quasi
    ) where

      import Data.Version
      import Paths_dindo_common
      import Language.Haskell.TH
      import Language.Haskell.TH.Syntax

      dindo_common_version = version
      dindo_common_version_quasi = stringE $ showVersion version
\end{code}
