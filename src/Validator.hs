{-# LANGUAGE OverloadedStrings #-}

module Validator where

import qualified Data.ByteString.Lazy as LB
import qualified Data.HashMap.Strict as HM
import Data.List (find, sort)
import AST
import MonadError
-- import Data.Aeson

findRepeated :: (Eq a, Ord a) => [a] -> Maybe a
findRepeated xs = findRepeated' $ sort xs
  where 
    findRepeated' [] = Nothing
    findRepeated' [_] = Nothing
    findRepeated' (x:y:xs)
      | x == y    = Just x
      | otherwise = findRepeated (y:xs)

-- Errors if there is a list and any item contains a space.
checkSpaces :: (MonadError m) => Maybe [String] -> (String -> Error) -> m ()
checkSpaces ms errType = case ms of 
  Nothing -> return ()
  Just xs -> case find (elem ' ') xs of
    Just x -> throw $ errType x
    _      -> return ()

-- Errors if there is a list and any item is repeated.
checkRepeated :: (Ord a, MonadError m) => Maybe [a] -> (a -> Error) -> m ()
checkRepeated ms errType = case ms of
  Nothing -> return ()
  Just xs -> case findRepeated xs of
    Just x -> throw $ errType x
    _      -> return ()

definitionParser :: Either String Definition -> MErr Definition
definitionParser def = case def of
  Left err -> throw $ InvalidJsonError err
  Right o  -> return o

-- Validates that the models are valid (i.e. no space in model name, no repeated model)
modelsParser :: Definition -> MErr ()
modelsParser def = do
  let ms = models def 
  validateModels ms
    where 
      validateModels ms = do
        checkSpaces ms SpaceInModelError
        checkRepeated ms DuplicatedModelError

-- Validates that the models are valid (i.e. no space in middleware name, no repeated middleware)
middlewaresParser :: Definition -> MErr ()
middlewaresParser def = do
  let ms = middlewares def
  validateMiddlewares ms
    where 
      validateMiddlewares ms = do
        checkSpaces ms SpaceInMiddlewareError
        checkRepeated ms DuplicatedMiddlewareError

test :: Either String Definition -> MErr Definition
test d = do 
  def <- definitionParser d
  modelsParser def
  middlewaresParser def
  return def
