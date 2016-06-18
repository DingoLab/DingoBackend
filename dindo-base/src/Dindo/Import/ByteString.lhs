




% src/Dindo/Import/ByteString.lhs

\begin{code}
module Dindo.Import.ByteString
    ( module X
    , fromStrict,toStrict
    , showButf8,readButf8
    ) where

      import Data.ByteString as X
      import Data.ByteString.Lazy as L
      import qualified Data.Text as T
      import qualified Data.Text.Encoding as TE
\end{code}

\begin{code}
      showButf8 :: Show a => a -> X.ByteString
      showButf8 = TE.encodeUtf8.T.pack.show
      readButf8 :: Read a => X.ByteString -> a
      readButf8 = read.T.unpack.TE.decodeUtf8
\end{code}
