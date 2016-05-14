




% src/Dindo/Import/ByteString.lhs

% 导出 Data.ByteString等

\begin{code}
module Dindo.Import.ByteString
    ( module X
    , fromStrictBS
    ) where
\end{code}

\begin{code}
      import Data.ByteString as X
      import Data.ByteString.Lazy
      fromStrictBS = fromStrict
\end{code}
