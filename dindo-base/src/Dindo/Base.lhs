




% src/Dindo/Base.lhs

\begin{code}
module Dindo.Base
    ( lookupHeader
    , lookupHeaderm
    , lookupQueryf
    , lookupQuery
    , lookupQuerym
    , HandlerT
    , dindo_base_version
    , dindo_base_version_quote
    ) where

      import Dindo.Import
      import Dindo.RIO
      import Dindo.Exception
      import Data.Text as T
      import qualified Data.Text.Encoding as TE
      import qualified Data.CaseInsensitive as CI
      import Dindo.Import.ByteString as B
      import Network.Wai(Response)
      import Paths_dindo_base
      import Data.Version
      import Language.Haskell.TH
\end{code}

\begin{code}
      dindo_base_version = showVersion version
      dindo_base_version_quote = stringE dindo_base_version
\end{code}

\begin{code}
      type HandlerT cfg = RIO cfg Response
\end{code}

查找头
\begin{code}
      lookupHeader :: CI.CI ByteString -> RIO cfg Text
      lookupHeader x =
        (return.fromMaybe te.fmap TE.decodeUtf8.lookup x) =<< getHeaders
        where
        te = invalidHeaders $ T.unpack $ TE.decodeUtf8 $ CI.original x
      lookupHeaderm :: CI.CI ByteString -> RIOM cfg Text
      lookupHeaderm x =
        (return.fmap TE.decodeUtf8.lookup x) =<< getHeaders
\end{code}

查找 Query（flag）
\begin{code}
      lookupQueryf :: ByteString -> RIO cfg Bool
      lookupQueryf x = do
        q <- getQuerys
        case lookup x q of
          Just _ -> return True
          _ -> return False
\end{code}

查找 Query
\begin{code}
      lookupQuery :: ByteString -> RIO cfg Text
      lookupQuery x =
        (return.fromMaybe tee.fromMaybe te.fmap (fmap TE.decodeUtf8).lookup x) =<<  getQuerys
        where
          te  = invalidArgs $ T.unpack $ TE.decodeUtf8 x
          tee = invalidArgs $ T.unpack $ TE.decodeUtf8 x
      lookupQuerym :: ByteString -> RIOM cfg Text
      lookupQuerym x =
        (return.fromMaybe Nothing .fmap (fmap TE.decodeUtf8).lookup x) =<< getQuerys
\end{code}

添加头
\begin{code}
      addCHeaders :: [Header] -> [Header]
      addCHeaders = (++)
        [ ("Server","DINDO")
        ]
\end{code}
