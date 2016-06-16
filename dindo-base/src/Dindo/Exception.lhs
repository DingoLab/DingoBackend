




% src/Dindo/Exception.lhs

\begin{code}
module Dindo.Exception
    ( module X
    , catchH
    , catchesH
    , handleH
    , tryH
    ) where

      import Dindo.Import.Yesod

      import Control.Exception as X  hiding (Handler)
\end{code}

\begin{code}
      catchH :: ( Exception e
                , MonadIO m
                )
             => HandlerT site m a
             -> (e -> HandlerT site m a)
             -> HandlerT site m a
      catchH fm h = do
        hio <- handlerToIO
        liftIO $ catch (hio fm) (hio h)
\end{code}

\begin{code}
      catchesH :: ( Exception e
                  , MonadIO m
                  )
               => HandlerT site m a
               -> [e -> HandlerT site m a]
               -> HandlerT site m a
      catchesH fm hs = do
        hio <- handlerToIO
        liftIO $ catches (hio fm) (map hio hs)
\end{code}

\begin{code}
      handleH :: ( Exception e
                 , MonadIO m
                 )
              => (e -> HandlerT site m a)
              -> HandlerT site m a
              -> HandlerT site m a
      handleH = flip catchH
\end{code}

\begin{code}
      tryH :: ( Exception e
              , MonadIO m
              )
           => HandlerT site m a
           => HandlerT site m (Either e a)
      tryH fm = do
        hio <- handlerToIO
        liftIO $ try $ hio fm
\end{code}
