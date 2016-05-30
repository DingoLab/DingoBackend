




% src/Dindo/Database.lhs

%%%
%%% Dingo 后端 数据库的设置
%%%



\begin{code}
module Dindo.Database where
\end{code}

\begin{code}
      import Prelude hiding (String)
      import Import
      import Data.Text
      import Data.ByteString
      import Paths_dindo_database
      import Data.Version
\end{code}


\begin{code}
      instance FromJSON ByteString where
        parseJSON (String x) = pure $ encodeUtf8 x
      instance ToJSON ByteString where
        toJSON = String . decodeUtf8
\end{code}

\begin{code}
      share [mkPersist sqlSettings] [persistLowerCase|
      Account json sql=table_account
        Id sql=
        uid Text sql=key_uid sqltype=varchar(64)
        pash Text sql=key_pash sqltype=varcher(64)
        tel Int sql=key_tel
        name Text sql=key_name sqltype=varchar(64)
        Primary uid
        deriving Show Eq
      Usr json sql=table_usr
        Id sql=
        uid Text sql=key_uid sqltype=varchar(64)
        email Text sql=key_email
        rname Text sql=key_rname sqltype=varchar(64)
        prcid Text sql=key_prcid sqltype=varchar(18)
        addr Text sql=key_addr
        status Text sql=key_status sqltype=varchar(1)
        Primary uid
        Foreign Account fkuid uid
        deriving Show Eq
      Addr json sql=table_addr
        Id sql=
        aid Text sql=key_aid sqltype=varchar(64)
        uid Text sql=key_uid sqltype=varchar(64)
        zip Text sql=key_zip sqltype=varchar(64)
        addr Text sql=key_addr
        Primary aid
        Foreign Account fkaddruid uid
        deriving Show Eq
      Apic sql=table_apic
        Id sql=
        pid Text sql=key_pic_id sqltype=varchar(64)
        uid Text sql=key_uid sqltype=varchar(64)
        bpic ByteString sql=binary_pic
        typ Int Maybe sql=key_status default=0
        Primary pid
        Foreign Account fkuidb uid
        deriving Show Eq
      Task json sql=table_task
        Id sql=
        tid Text sql=key_tid sqltype=varchar(64)
        ca Text Maybe sql=key_ca sqltype=varchar(64)
        cb Text Maybe sql=key_cb sqltype=varchat(64)
        Primary tid
        Foreign Account fkca ca
        Foreign Account fkvcb cb
        deriving Show Eq
      Taskinfo json sql=table_task_info
        Id sql=
        tid Text sql=key_tid sqltype=varchar(64)
        ew Double sql=key_ew
        ns Double sql=key_ns
        r Double sql=key_r
        wei Double sql=key_wei
        size [Double] sql=key_size
        note Text Maybe sql=key_note
        cost Int sql=key_cost
        des Text Maybe sql=key_des
        Primary tid
        Foreign Task fktid tid
        deriving Show Eq
      Taskcost json sql=table_task_cost
        Id sql=
        tid Text sql=key_tid sqltype=varchar(64)
        ad [Int] sql=key_ad
        bd [Int] sql=key_bd
        Primary tid
        Foreign Task fktidb tid
        deriving Show Eq
      Dd json sql=table_dd
        Id sql=
        did Text sql=key_did sqltype=varchar(64)
        uid Text sql=key_tid sqltype=varchar(64)
        dd Text sql=key_dd
        ew Double sql=key_ew
        ns Double sql=key_ns
        r Double sql=key_r
        Primary did
        Foreign Account fkuidc uid
      TmpToken json sql=table_tmptoken
        Id sql=
        tt Text sql=key_tmptoken sqltype=varchar(150)
        time UTCTime sql=key_timeup
        uid Text sql=key_uid sqltype=varchar(64)
        Primary tt
        Foreign Account fkuidd uid
      |]
\end{code}


\begin{code}
      dindo_database_version = version
      dindo_database_version_quasi = stringE $ showVersion version
\end{code}
