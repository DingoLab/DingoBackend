




% src/Dindo/Import/ByteString.lhs

\begin{code}
module Dindo.Import.ByteString.lhs
    ( module X
    , fromStrict
    , toStrict
    , showButf8,readButf8
    ) where

      import Data.ByteString as X
      import Data.ByteString.Lazy
      import qualified Data.Text as T
      import qualified Data.Text.Encoding as TE
\end{code}

\begin{code}
      showButf8 :: Show a => a -> ByteString
      showButf8 = encodeUtf8.T.pack.show
      readButf8 :: Read a => ByteString -> a
      readButf8 = read.T.unpack.decodeUtf8
\end{code}
