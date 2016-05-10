




% src/Dindo/MicroFramework/Destroy.lhs

%%%
%%% 微服务中单个容器/进程停止的接口
%%%


\begin{code}
module Dindo.MicroFramework.Destory
    ( Destorible(..)
    , regDestory
    ) where
\end{code}

\begin{code}
      import Yesod.Core
\end{code}

服务实例销毁的类型类
\begin{description}
  \item[destoryAPI] 销毁的 API
  \item[destoryHead] 所需的 Head 中特定“签名的内容”
\end{description}

\begin{code}
      class ( Yesod a
            ) => Destorible a where
        destoryAPI :: a -> String
        destoryHead :: a ->  String
\end{code}

\begin{code}
      regDestory :: Destorible a => a -> IO Bool
      regDestory x = do
        -- 注册 销毁接口
        -- 实际上应该是 http 请求，此处仅输出内容
        putStrLn "销毁接口 注册"
        print $ destoryAPI x
        print $ destoryHead x
        return True
\end{code}
