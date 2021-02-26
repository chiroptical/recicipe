{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Routes where

import Data.Aeson (ToJSON)
import Data.Data (Proxy (Proxy))
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.PureScript.Bridge (SumType, mkSumType)
import Language.PureScript.Bridge.TypeInfo (Language (Haskell))
import Network.Wai.Middleware.Cors
import Servant (Application, JSON, (:>))
import Servant.API (Get)
import Servant.API.Generic ((:-))
import Servant.Server.Generic (AsServerT, genericServe)

data Person = Person
  { _name :: Text,
    _age :: Int
  }
  deriving (Show, Generic, ToJSON)

myPurescriptTypes :: [SumType 'Haskell]
myPurescriptTypes =
  [ let p = (Proxy :: Proxy Person) in mkSumType p
  ]

newtype Routes route = Routes
  { _person :: route :- "person" :> Get '[JSON] Person
  }
  deriving (Generic)

handlers :: Applicative m => Routes (AsServerT m)
handlers =
  Routes
    { _person = pure $ Person "chiroptical" 21
    }

app :: Application
app = cors (const $ Just policy) $ genericServe handlers
  where
    policy =
      simpleCorsResourcePolicy
        { corsRequestHeaders = ["content-type"]
        }
