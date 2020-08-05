{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE DeriveGeneric #-}

module AST where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T

-- import Text.Read (readMaybe)
-- import GHC.Generics

type Model = String

type Models = [Model]

type Middleware = String

type Middlewares = [Middleware]

data Method = Get | Post | Put | Delete
  deriving (Show, Ord, Eq)

type Controller = String


-------------- CHANGING --------------

type TMethodValue = HM.HashMap String Value

-- temporary storing a method with all the relevant information (for a route)
type TMethods = HM.HashMap String TMethodValue

-- instance FromJSON TMethods where
--   parseJSON val = TMethods <$> parseJSON val 
  --  -> (<$>) :: Functor f => (a -> b) -> f a -> f b

-- given a TMethods, retrieves the controller name (if any)
-- getController :: String -> TMethods -> Maybe Value
-- getController k (TMethods obj) = HM.lookup "controller" $ case HM.lookup k obj of 
--   Just obj -> obj
--   _        -> HM.empty

-- given a TMethods, retrieves the middleware list (if any)
-- getMiddlewares :: String -> TMethods -> Maybe Value
-- getMiddlewares k (TMethods obj) = HM.lookup "middlewares" $ case HM.lookup k obj of 
--   Just obj -> obj
--   _        -> HM.empty
  

-- data Route = R String Method Middlewares Controller [Route]
data Route = R String Method Middlewares Controller [Route]
  deriving (Show)

type Routes = [Route]

data Definition = Definition 
  { 
    models :: Models
  , middlewares :: Maybe Middlewares
  , method :: Method
  -- , routes :: Routes
  , methods :: TMethods
  }
  deriving (Show)

--------------------------------------

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

-- parseMiddlewares :: Value -> Parser Middlewares
-- parseMiddlewares = withArray "array of middlewares" $ \arr ->
--   mapM parseJSON (V.toList arr)

-- parseModels :: Value -> Parser Models
-- parseModels = withArray "array of models" $ \arr ->
--   mapM parseJSON (V.toList arr)

instance FromJSON Definition where
  parseJSON = withObject "Definition" $ \obj -> do
    models <- obj .: "models"
    middlewares <- obj .:? "middlewares"
    method <- obj .: "method"
    methods <- obj .: "methods"
    return (Definition { models = models, middlewares = middlewares, method = method, methods = methods })

data Error = InvalidMethod Method
           | DuplicatedModelError Model
           | DuplicatedMiddlewareError Middleware
           | NonDeclaredMiddlewareError Middleware
           | DuplicatedRouteError Route
  deriving Show