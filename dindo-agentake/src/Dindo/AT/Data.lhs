




% src/Dindo/AT/Data.lhs

\begin{code}
module Dindo.AT.Data
    (
    ) where

      import Dindo.Import.Rable
      import Dindo.Import.Aeson as A
      import Dindo.Import.Yaml as Y
      import Dindo.Import.Text as T
      import Dindo.Import.ByteString as B
      import Dindo.Import.Yesod
      import Dindo.Import.Database
\end{code}

任务发布的返回数据
\begin{code}
      data RtUtask = RtUtaskSucc Text
                   | RtUtaskFail Text
      instance Varable RtUtask where
        toValue (RtUtaskSucc x) = object ["task-id" .= x]
        toValue (RtUtaskFail x) = object ["error" .= x]
        toNodes (RtUtaskSucc x) = [xml|<task-id>#{x}|]
        toNodes (RtUtaskFail x) = [xml|<error>#{x}|]
      instance Rable RtUtask where
        toWhere _ = RtBody
        toStatus (RtUtaskSucc _) = RtSucc
        toStatus (RtUtaskFail _) = RtFail
\end{code}

获得代收的返回数据
\begin{code}
      data RtGtask = RtGtaskSucc [Text]
                   | RtGtaskFail Text
      instance Varable RtGtask where
        toValue (RtGtaskSucc x) = object
          [ "count" .= length x
          , "data" .= x
          ]
        toValue (RtGtaskFail x) = object ["error" .= x]
        toNodes (RtGtaskSucc x) = [xml|
          <count>#{length x}
          <data>
            $forall i <- x
              <item>#{i}
        |]
        toNodes (RtGtaskFail x) = [xml|<error>#{x}|]
      instance Rable RtGtask where
        toWhere _ = RtBody
        toStatus (RtGtaskSucc _) = RtSucc
        toStatus (RtGtaskFail _) = RtFail
\end{code}

获取任务信息
\begin{code}
      data RtItask = RtItaskSucc Taskinfo Task Taskcost
                   | RtItaskFail Text
      instance Rable RtItask where
        toValue (RtItaskFail x) = object ["error".=x]
        toValue (RtItaskSucc Taskinfo{..} Task{..} Taskcost{..}) = object $
          [ "tid"       .= taskinfoTid
          , "ew"        .= taskinfoEw
          , "ns"        .= taskinfoNs
          , "r"         .= taskinfoR
          , "wei"       .= taskinfoWei
          , "size"      .= tSize
          , "note"      .= taskinfoNote
          , "cost"      .= taskinfoCost
          , "intr"      .= taskinfoDes
          , "costA"     .= taskcostAd
          , "costB"     .= taskcostBd
          , "linkedA"   .= taskCa
          , "linkedB"   .= taskCb
          ]
          where
            tSize = "("++show (taskinfoSize!!0)++","++show (taskinfoSize!!1)++","++show (taskinfoSize!!2)++")"
        toNodes (RtItaskFail x) = [xml|<error>#{x}|]
        toNodes (RtItaskSucc Taskinfo{..} Task{..} Taskcost{..}) = [xml|
          <tid>          #{taskinfoTid}
          <ew>           #{taskinfoEw}
          <ns>           #{taskinfoNs}
          <r>            #{taskinfoR}
          <wei>          #{taskinfoWei}
          <size>         #{tSize}
          <note>         #{takeinfoNote}
          <cost>         #{takeinfoCost}
          <intr>         #{taskinfoDes}
          <costA>        #{taskcostAd}
          <costB>        #{taskcostBd}
          <linkedA>      #{taskCa}
          <linkedB>      #{taskCb}
          |]
          where
            tSize = "("++show (taskinfoSize!!0)++","++show (taskinfoSize!!1)++","++show (taskinfoSize!!2)++")"
      instance Rable RtItask where
        toWhere _ = RtBody
        toStatus (RtItaskSucc _ _ _) = RtSucc
        toStatus (RtItaskFail _) = RtFail
\end{code}

可代收状态的增改
\begin{code}
      data RtAagent = RtAagentSucc Text
                    | RtAagentFail Text
      instance Varable RtAagent where
        toValue (RtAagentSucc x) = object ["did" .= x]
        toValue (RtAagentFail x) = object ["error" .= x]
        toNodes (RtAagentSucc x) = [xml|<did>#{x}|]
        toNodes (RtAagentFail x) = [xml|<error>#{x}|]
      instance Rable RtAagent where
        toWhere _ =RtBody
        toStatus (RtAagentSucc _) = RtSucc
        toStatus (RtAagentFail _) = RtFail
\end{code}

可代收状态查询
\begin{code}
      data RtSagent = RtSagentSucc Dd
                    | RtSagentFail Text
      instance Varable RtSagent where
        toValue (RtSagentFail x) = object ["error" .= x]
        toValue (RtSagentSucc Dd{..}) = object
          [ "dd" .= ddDd
          , "ew" .= ddEw
          , "ns" .= ddNs
          , "r"  .= ddR
          ]
        toNodes (RtSagentFail x) = [xml|<error>#{x}|]
        toNodes (RtSagentSucc Dd{..}) = [xml|
          <dd>  #{ddDd}
          <ew>  #{ddEw}
          <ns>  #{ddNs}
          <r>   #{ddR}
          |]

\end{code}
