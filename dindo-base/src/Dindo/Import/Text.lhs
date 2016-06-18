




% src/Dindo/Import/Text.lhs

\begin{code}
module Dindo.Import.Text
    ( module X
    , encodeUtf8,decodeUtf8
    , showT,readT
    ) where

      import Data.Text as X
      import Data.Text.Encoding
\end{code}

\begin{code}
      showT :: Show a => a -> Text
      showT = pack.show
      readT :: Read a => Text -> a
      readT = read.unpack
\end{code}
