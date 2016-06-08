




% src/Dindo/Launch.lhs

\begin{code}
module Dindo.Launch
    ( Dindoable(..)
    ) where

      import Dindo.MicroFramework.Register
      import Dindo.Import.Aeson
      import Dindo.Logger
      import Network.Wai
\end{code}

\begin{code}
      class Registrable a => Dindoable a where
        appItem :: a ->  Application
        createItem :: FromJSON b => b -> a
\end{code}
