




% src/Dindo/Logger.lhs

\begin{code}
module Dindo.Logger
    ( defaultBufSize
    , pushLogStr
    , pushLogStrTime
    , pushLogStrLn
    , pushLogStrLnTime
    , logger
    , loggerLn
    , apLogger
    , BufSize
    , module X
    ) where

      import System.Log.FastLogger as X
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
      pushLogStrTime i s = getLocalTime >>= (pushLogStr s.(<>i).toLogStr)
      pushLogStrLnTime :: LogStr -> LoggerSet -> IO ()
      pushLogStrLnTime i s = getLocalTime >>= (pushLogStrLn s.(<>i).toLogStr)
      getLocalTime :: IO String
      getLocalTime = do
        tz <- getCurrentTimeZone
        now <- getCurrentTime
        let nl = utcToLocalTime tz now
        return $ show nl ++" "++ show tz

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
        --liftIO $ putStrLn "hw"
        response <- i
        req <- getRequest
        loggerLn $ "\n\t"
          <> show' (remoteHost req) <> "\n\t"
          <> show' (requestMethod req) <> "\n\t/"
          <> concat' (pathInfo req) <> "\n\t"
          <> show' (httpVersion req) <> "\n\t"
          <> resStatus response <> "\n\t"
          <> show' (requestBodyLength req) <> "\n\t"
          <> fromMaybe' (requestHeaderHost req) <> "\n\t"
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
