




% src/Dindo/Import/Text.lhs

%  导出 Data.Text 等模块

\begin{code}
module Dindo.Import.Text
    ( module X
    , showT
    , readT
    ) where

      import Data.Text as X
      import Data.Text.Encoding as X
\end{code}

\begin{code}
      showT :: Show a => a -> Text
      showT = pack.show
\end{code}

\begin{code}
      readT :: Read a => Text -> a
      readT = read.unpack
\end{code}
