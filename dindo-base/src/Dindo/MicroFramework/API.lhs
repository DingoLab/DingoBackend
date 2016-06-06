




% src/Dindo/MicroFramework/API.lhs

%%%
%%% 微服务中 关于 API 的内容
%%%

\begin{code}
module Dindo.MicroFramework.API
    ( APIble(..)
    , regAPI
    ) where
\end{code}


注册的 API 的类型类
\begin{description}
  \item[apis] 所公开注册的 API，（API名称，相关 Route 信息）
\end{description}

\begin{code}
      class APIble a where
        apis :: a -> [(String,String)]
\end{code}

\begin{code}
      regAPI :: APIble a => a -> IO Bool
      regAPI x = do
        -- 注册 API
        -- 实际上应该是 数据生成+http 请求，此处仅输出内容
        putStrLn "API 内容"
        print $ apis x
        return True
\end{code}
