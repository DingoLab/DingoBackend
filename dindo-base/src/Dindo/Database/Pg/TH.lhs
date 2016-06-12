




% src/Dindo/Database/Pg/TH.lhs

\begin{code}
module Dindo.Database.Pg.TH
    ( pgQuery
    ) where

      import Language.Haskell.TH
      import Language.Haskell.TH.Quote
      import Language.Haskell.TH.Syntax
\end{code}

\begin{code}
      toPgQuery :: String -> Q Exp
      toPgQuery str' = [e|toQuery str|]
        where
          str = concat $ lines $ str'

      pgQuery = QuasiQuoter toPgQuery undefined undefined undefined
\end{code}
