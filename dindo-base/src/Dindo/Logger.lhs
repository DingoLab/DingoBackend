




% src/Dindo/Logger.lhs

\begin{code}
module Dindo.Logger
    ( defaultBufSize
    , BufSize
    ) where

      import System.Log.FastLogger
      import Data.Monoid
      import Dindo.RIO
      import Network.Wai.Internal
      import Data.Time
      import Data.Maybe
      import qualified Dindo.Import.Text as T
      import qualified Dindo.Import.ByteString as B
\end{code}

\begin{code}
      pushLogStrTime :: LogStr -> LoggerSet -> IO ()
      pushLogStrTime i s = getCurrentTime >>= (pushLogStr s.(<>i).toLogStr.show)
      pushLogStrLnTime :: LogStr -> LoggerSet -> IO ()
      pushLogStrLnTime i s = getCurrentTime >>= (pushLogStrLn s.(<>i).toLogStr.show)
\end{code}

\begin{code}
      logger :: LogStr -> RIO cfg ()
      logger ls = RIO $ pushLogStrTime ls.rdLogger
      loggerLn :: LogStr -> RIO cfg ()
      loggerLn ls = RIO $ pushLogStrLnTime ls.rdLogger
\end{code}

\begin{code}
      apLogger :: RIO cfg Response -> RIO cfg Response
      apLogger i = do
        response <- i
        req <- getRequest
        loggerLn $ " "
          <> show' (remoteHost req) <> " "
          <> show' (requestMethod req) <> " "
          <> concat' (pathInfo req) <> " "
          <> show' (httpVersion req) <> " "
          <> resStatus response <> " "
          <> show' (requestBodyLength req) <> " "
          <> fromMaybe' (requestHeaderHost req) <> " "
          <> fromMaybe' (requestHeaderUserAgent req)
        return response
        where
          fromMaybe' :: ToLogStr a => Maybe a -> LogStr
          fromMaybe' = fromMaybe "-".fmap toLogStr
          show' :: Show a => a -> LogStr
          show' = toLogStr.show
          concat' = toLogStr.T.concat
          resStatus (ResponseFile s _ _ _) = toLogStr $ show s
          resStatus (ResponseBuilder s _ _) = toLogStr $ show s
          resStatus (ResponseStream s _ _) = toLogStr $ show s
          resStatus _ = "raw"
\end{code}
