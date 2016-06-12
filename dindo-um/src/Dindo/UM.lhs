




% src/Dindo/UM.lhs

\begin{code}
module Dindo.UM
    ( stdDindo
    , stdT
    , dindom_name
    , dindom_name_quote
    , dindom_version
    , dindom_version_quote
    ) where

      import Dindo.Route
      import Dindo.Auth
      import Dindo.Import.Wai
      import Dindo.RIO
      import Dindo.UM.Handler
      import Dindo.UM.Config
      import Paths_dindo_um
      import Data.Version
      import Dindo.Import.TH
\end{code}

\begin{code}
      [dindo| |]
\end{code}

\begin{code}
      dindo_um_version = showVersion version
      dindo_um_version_quote = stringE dindo_um_version
\end{code}

\begin{code}
      stdT = [t|UMConfig|]
      dindom_name = "dindo-um"
      dindom_name_quote = stringE "dindo-um"
      dindom_version = dindo_um_version
      dindom_version_quote = dindo_um_version_quote
\end{code}
