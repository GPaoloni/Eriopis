{-# LANGUAGE OverloadedStrings #-}

module AST where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T

-- AST returned by the parser
type Model = String

type Models = Maybe [Model]

type Middleware = String

type Middlewares = Maybe [Middleware]

data Method = Get | Post | Put | Delete
  deriving (Show, Ord, Eq)

type Controller = String

data Route = R String Method Middlewares Controller Routes
  deriving (Show)

type Routes = [Route]

data Error = InvalidJsonError String
           | SpaceInModelError Model
           | DuplicatedModelError Model
           | SpaceInMiddlewareError Middleware
           | DuplicatedMiddlewareError Middleware
           | InvalidMethodError Method
           | NonDeclaredMiddlewareError Middleware
           | DuplicatedRouteError Route
  deriving Show

-- Types used by Aeson for the JSON parsing
data TMethodValue = TMethodValue { controller :: String, middleware :: Maybe [String] }
  deriving Show

instance FromJSON TMethodValue where
  parseJSON = withObject "Options for the method" $ \o -> do 
    controller <- o .: "controller"
    middleware <- o .:? "middleware"
    return $ TMethodValue { controller = controller, middleware = middleware }

type TMethods = HM.HashMap String TMethodValue

data TRoute = TRoute { methods :: Maybe TMethods, routes :: TRoutes }
  deriving Show

instance FromJSON TRoute where
  parseJSON = withObject "Route definition" $ \o -> do
    methods <- o .:? "methods"
    routes <- o .:? "routes"
    return $ TRoute { methods = methods, routes = routes }

type TRoutes = Maybe (HM.HashMap String TRoute)  

data Definition = Definition 
  { 
    models :: Models
  , middlewares :: Middlewares
  , routing :: TRoutes
  }
  deriving (Show)

instance FromJSON Definition where
  parseJSON = withObject "Definition" $ \obj -> do
    models <- obj .:? "models"
    middlewares <- obj .:? "middlewares"
    routing <- obj .:? "routing"
    return (Definition { models = models, middlewares = middlewares, routing = routing })



--------------------
-- TO-DO: Clean this
--------------------
instance FromJSON Method where
  parseJSON (String s) =  pure $ mkMethod s
  parseJSON _ = fail "Failed to parse Method object"

instance ToJSON Method where
  toJSON Get = "GET"
  toJSON Post = "POST"
  toJSON Put = "PUT"
  toJSON Delete = "DELETE"

mkMethod :: T.Text -> Method
mkMethod "GET"    = Get
mkMethod "POST"   = Post
mkMethod "PUT"    = Put
mkMethod "DELETE" = Delete
mkMethod s        = error ("Invalid method provided" ++ show s)