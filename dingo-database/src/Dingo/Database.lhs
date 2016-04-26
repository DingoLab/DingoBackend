




% src/Dingo/Database.lhs

%%%
%%% Dingo 后端 数据库的设置
%%%


\begin{code}
{-# LANGUAGE TemplateHaskell
           , FlexibleInstances
           , TypeFamilies
           , MultiParamTypeClasses
           , GADTs
           , GeneralizedNewtypeDeriving
           #-}
\end{code}


\begin{code}
module Dingo.Database  where
\end{code}

\begin{code}
import Prelude hiding (String)
import Import
import Data.Text
import Data.ByteString
import Paths_dingo_database
import Data.Version
\end{code}

    
\begin{code}
instance FromJSON ByteString where
  parseJSON (String x) = pure $ encodeUtf8 x
instance ToJSON ByteString where
  toJSON = String . decodeUtf8
\end{code}

\begin{code}
share [mkPersist sqlSettings] $(persistFileWithC lowerCaseSettings "database")
\end{code}


\begin{code}
dedbVer :: Q Exp
dedbVer = stringE $ showVersion version            
\end{code}
