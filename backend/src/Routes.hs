{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Routes where

import Data.Text (Text)
import GHC.Generics (Generic)
import Servant (Application, JSON, (:>))
import Servant.API (Get)
import Servant.API.Generic ((:-))
import Servant.Server.Generic (AsServerT, genericServe)

newtype Routes route = Routes
  { _hello :: route :- "hello" :> Get '[JSON] Text
  }
  deriving (Generic)

handlers :: Applicative m => Routes (AsServerT m)
handlers =
  Routes
    { _hello = pure "hello"
    }

app :: Application
app = genericServe handlers
