




% src/Dindo/UM.lhs

\begin{code}
{-# LANGUAGE TemplateHaskell
           , OverloadedStrings
           #-}
\end{code}

\begin{code}
module Dindo.UM
    ( module X
    ) where
      import Dindo.UM.Foundation as X
      import Dindo.UM.Handler as X
      import Dindo.Import.Yesod
      import Dindo.Import.TH
      import Data.Version
      import Paths_dindo_usrmanage

      dindo_usrmanaga_version = version
      dindo_usrmanaga_version_quasi = stringE $ show version
      mkYesodDispatch "UM" resourcesUM
\end{code}
