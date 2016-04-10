




% src/Dingo/MicroFramework/Register.lhs

%%%
%%% 微服务中注册服务实例（容器或进程）
%%%


\begin{code}
module Dingo.MicroFramework.Register
    ( Registrable(..)
    , Heartbeatable(..)
    , register
    ) where
\end{code}

\begin{code}
      import Yesod.Core

      import Dingo.MicroFramework.API
      import Dingo.MicroFramework.Destory
\end{code}


可注册的服务的类型类。

\begin{description}
  \item[regSvrAddr] 注册目标的地址 ip或域名
  \item[regSvrPost] 访问端口
  \item[regAddr] 注册的服务的地址
  \item[regPort] 注册的端口
\end{description}

\begin{code}
      class ( Yesod a
            , APIble a
            , Destorible a
            , Heartbeatable a
            ) => Registrable a where
        regAddr :: a -> String
        refAddr = defRegAddr
        regPort :: a -> Int
        regPort = defRegPort
        regSvrAddr :: a -> String
        regSvrPort :: a -> Int
      defRegPort _ = 3000
      defRegAddr _ = "localhost"
\end{code}


状态获取的类型类

\begin{code}
      class ( Yesod a
            , RenderRoute a
            ) => Heartbeatable a where
        heartbeat :: a -> IO ()
\end{code}

注册服务实例的函数
\begin{code}
  \item[False] 注册失败
  \item[True] 注册成功
\end{code}
\begin{code}
      register :: Registrable a => a -> IO Bool
      register x = do
        -- 注册 服务
        -- 实际上应该是 http 请求，此处仅输出内容
        putStrLn "注册服务的端口"
        print $ regSvrPort x
        putStrLn "注册服务的地址"
        print $ regSvrAddr x
        putStrLn "被注册的实例的地址"
        print $ regPort x
        putStrLn "被注册的实例的端口"
        print $ regPort x
        regAPI' $ regDestory' $ do
          fokIO $ heartbeat x
          return True
        where
          regAPI' a = do
            ra <- regAPI x
            if ra then a else return False
          regDestory' a = do
            rd <- regDestory x
            if rd then a else return True
\end{code}
