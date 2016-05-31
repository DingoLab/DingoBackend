




% src/Dindo/AT/Handler.lhs

\begin{code}
module Dindo.AT.Handler
    ( postUtaskR
    , postCtaskR
    , postGtaskR
    , postBtaskR
    , postItaskR
    , postPtaskR
    , postAagentR
    , postDagentR
    , postSagentR
    ) where

      import Dindo.Import
      import Dindo.Import.Rable
      import Dindo.Import.Yesod
      import Dindo.Import.Database
      import Dindo.AT.Foundation
      import Dindo.AT.Data
      import Dindo.Import.Digest
      import Dindo.Import.ByteString as B hiding(unpack,pack,splitAt,take,map,null)
      import Dindo.Import.Text as T hiding(splitAt,take,map,null)
      import Dindo.Common.Auth(fromEntity,pickU,pickF)
      import Control.Exception(try,SomeException)
      import Control.Monad
\end{code}

任务发布的 API
\begin{code}
      postUtaskR :: Handler TypedContent
      postUtaskR = undefined
\end{code}
\begin{code}
      postCtaskR :: Handler TypedContent
      postCtaskR = undefined
\end{code}
\begin{code}
      postGtaskR :: Handler TypedContent
      postGtaskR = undefined
\end{code}
\begin{code}
      postBtaskR :: Handler TypedContent
      postBtaskR = undefined
\end{code}
\begin{code}
      postItaskR :: Handler TypedContent
      postItaskR = undefined
\end{code}
\begin{code}
      postPtaskR :: Handler TypedContent
      postPtaskR = undefined
\end{code}
\begin{code}
      postAagentR :: Handler TypedContent
      postAagentR = undefined
\end{code}
\begin{code}
      postDagentR :: Handler TypedContent
      postDagentR = undefined
\end{code}
\begin{code}
      postSagentR :: Handler TypedContent
      postSagentR = undefined
\end{code}
