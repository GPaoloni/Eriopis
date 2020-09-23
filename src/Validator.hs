{-# LANGUAGE OverloadedStrings #-}

module Validator where

import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as HM
import Data.List (find)
import AST
import MonadError
-- import Data.Aeson

definitionParser :: Either String Definition -> MErr Definition
definitionParser def = case def of
  Left err -> throw $ InvalidJsonError err
  Right o  -> return o

-- validates that the models are valid (i.e. no space in model name, no repeated model)
modelsParser :: Definition -> MErr Definition
modelsParser def = do
  let ms = models def 
  validateModels ms
  return def
    where
      validateModels ms = do 
        checkSpaces ms
        checkRepeated ms
      checkSpaces ms = case ms of 
        Nothing -> return ms
        Just xs -> case find (elem ' ') xs of
          Nothing -> return ms
          Just m -> throw $ SpaceInModelError m
      checkRepeated ms = return ms

test :: Either String Definition -> MErr Definition
test d = do 
  def <- definitionParser d
  modelsParser def
