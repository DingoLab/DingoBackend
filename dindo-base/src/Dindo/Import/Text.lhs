




% src/Dindo/Import/Text.lhs

\begin{code}
module Dindo.Import.Text
    ( module X
    , fromStrict,toStrict
    ) where

      import Data.Text as X
      import Data.Text.Lazy (fromStrict,toStrict)
      import Data.Text.Encoding as X
\end{code}

\begin{code}
      showT :: Show a => a -> Text
      showT = pack.show
      readT :: Read a => Text -> a
      readT = read.unpack
\end{code}
