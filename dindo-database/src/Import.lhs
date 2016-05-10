



% Src/Import.lhs

%%%
%%% Dingo's Backend
%%% Import
%%%

\begin{code}
module Import
( module X
, persistFileWithC
) where
\end{code}

\begin{code}
import Language.Haskell.TH as X
import Data.Aeson as X
import Database.Persist as X
import Data.Text.Encoding as X
import Database.Persist.TH as X
import Database.Persist.Quasi as X
import Data.Time as X
\end{code}

\begin{code}
persistFileWithC :: PersistSettings
                 -> FilePath
                 -> Q Exp
persistFileWithC s = persistFileWith s.("dindo-config/"++)                         
\end{code}
