




% src/Dindo/Exception.lhs

\begin{code}
module Dindo.Exception
    ( module X
    , catchH
    , handleH
    , tryH
    ) where

      import Dindo.Import.Yesod

      import Control.Exception as X  hiding (Handler)
\end{code}

\begin{code}
      catchH :: Exception e
             => HandlerT site IO a
             -> (e -> HandlerT site IO a)
             -> HandlerT site IO a
      catchH fm h = do
        hio <- handlerToIO
        liftIO $ catch (hio fm) (\x -> hio $ h x)
\end{code}

\begin{code}
      handleH :: Exception e
              => (e -> HandlerT site IO a)
              -> HandlerT site IO a
              -> HandlerT site IO a
      handleH = flip catchH
\end{code}

\begin{code}
      tryH :: Exception e
           => HandlerT site IO a
           -> HandlerT site IO (Either e a)
      tryH fm = do
        hio <- handlerToIO
        liftIO $ try $ hio fm
\end{code}
