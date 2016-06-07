




% src/Dindo/Database/Dbable.lhs

\begin{code}
 module Dindo.Database.Dbable where
       import Dindo.Database.Pool
\end{code}

\begin{code}
      class PgSql a where
        getCP :: RIO a PgCP
\end{code}
