




% src/Dindo/Import/ByteString.lhs
Import and REexport Data.ByteString

\begin{code}
module Dindo.Import.ByteString
    ( module X
    , fromStrict
    , toStrict
    , showB,readB
    ) where

      import Data.ByteString as X
      import Data.ByteString.Lazy (fromStrict,toStrict)
\end{code}

\begin{code}
      import Data.Text as T
      import Data.Text.Encoding as ET

      showB :: Show a => a -> ByteString
      showB = encodeUtf8.T.pack.show

      readB :: Read a => ByteString -> a
      readB = read.T.unpack.decodeUtf8
\end{code}
