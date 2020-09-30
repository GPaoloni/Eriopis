{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Validator where

import qualified Data.ByteString.Lazy as LB
import Data.HashMap.Strict (foldrWithKey, toList)
import Data.List (concat, find, sort)
import Data.Maybe (maybeToList)
import Control.Monad (mapM, when)
import AST
import MonadError
-- import Data.Aeson

findRepeated :: (Ord a) => [a] -> Maybe a
findRepeated xs = findRepeated' $ sort xs
  where 
    findRepeated' [] = Nothing
    findRepeated' [_] = Nothing
    findRepeated' (x:y:xs)
      | x == y    = Just x
      | otherwise = findRepeated (y:xs)

throwIfSpace :: (MonadError m) => (String  -> Error) -> String -> m ()
throwIfSpace errType xs = when (' ' `elem` xs) $ throw $ errType xs

throwIfRepeated :: (Ord a, MonadError m) => (a -> Error) -> [a] -> m ()
throwIfRepeated errType xs = case findRepeated xs of
  Just x -> throw $ errType x
  _      -> return ()

-- Errors if there is a list and any item contains a space.
checkSpaces :: (MonadError m) => (String -> Error) -> Maybe [String] -> m ()
checkSpaces errType = mapM_ (mapM_ (throwIfSpace errType))

-- Errors if there is a list and any item is repeated.
checkRepeated :: (Ord a, MonadError m) => (a -> Error) -> Maybe [a] -> m ()
checkRepeated errType = mapM_ (throwIfRepeated errType)

-- Throws if the json parsing gone wrong, returns the definition otherwise
definitionParser :: Either String Definition -> MErr Definition
definitionParser def = case def of
  Left err -> throw $ InvalidJsonError err
  Right o  -> return o

-- Validates that the models are valid (i.e. no space in model name, no repeated model)
modelsParser :: Definition -> MErr ()
modelsParser def = do
  let ms = models def 
  checkSpaces SpaceInModelError ms
  checkRepeated DuplicatedModelError ms

-- Validates that the models are valid (i.e. no space in middleware name, no repeated middleware)
middlewaresParser :: Definition -> MErr ()
middlewaresParser def = do
  let ms = middlewares def
  checkSpaces SpaceInMiddlewareError ms
  checkRepeated DuplicatedMiddlewareError ms

-- Transform a routing object of type TRoutes to a list, to make easier iterating in next steps
transformRouting _      Nothing  = []
transformRouting prefix (Just o) = foldrWithKey (transformRouting' prefix) [] o
transformRouting' prefix k v xs =
  let methodsList = buildMethodList $ methods v
      subroutes = transformRouting (prefix ++ "/" ++ k) $ routes v
  in concat [xs, map (prefix ++ "/" ++ k,) methodsList, subroutes]
buildMethodList Nothing  = []
buildMethodList (Just o) = toList o

getRoute :: (String, (String, TMethodValue)) -> String
getRoute = fst

getMiddleware :: (String, (String, TMethodValue)) -> Maybe [String]
getMiddleware = middleware . snd . snd

-- routingParser :: Definition -> MErr ()
routingParser def = do
  let mw = concat $ maybeToList $ middlewares def
      r  = transformRouting "" $ routing def
  validateRoutes r
  validateMiddlewares mw r
  -- validateMethods r
  return ()
    where
      validateRoutes = mapM_ (throwIfSpace SpaceInRouteError . fst)
      validateMiddlewares mw = mapM_ (validateMiddlewares' mw)
      validateMiddlewares' mw r = mapM_ (mapM_ (throwIfInvalidMw mw $ getRoute r)) (getMiddleware r)
      throwIfInvalidMw mw r x = when (x `notElem` mw) $ throw $ NonDeclaredMiddlewareError (x ++ " in route " ++ r)

-- [
--   ("/some",("POST",TMethodValue {controller = "createOne", middleware = Just ["auth","isAdmin"]})),
--   ("/some",("GET",TMethodValue {controller = "findAll", middleware = Just ["auth","isAdmin"]})),
--   ("/some/help",("GET",TMethodValue {controller = "help", middleware = Nothing}))
-- ]

test :: Either String Definition -> MErr Definition
test d = do 
  def <- definitionParser d
  modelsParser def
  middlewaresParser def
  routingParser def
  return def
