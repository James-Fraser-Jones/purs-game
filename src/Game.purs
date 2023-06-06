module Game
  ( Game
  , Game'
  , Subscription(..)
  , View(..)
  , children
  , liftGame'
  , mkGame
  , render
  , subscriptions
  , update'
  )
  where

-----------------------------------------------------------------------------------

import Prelude

import Data.Exists (Exists, mkExists, runExists)
import Data.List (List)

-----------------------------------------------------------------------------------

data Subscription = Subscription

-----------------------------------------------------------------------------------

data View = View

-----------------------------------------------------------------------------------

newtype Game'' message state =
  Game'' {
    state'' :: state
  , update'' :: message -> state -> state
  , render'' :: state -> View
  , subscriptions'' :: state -> List Subscription
  , children'' :: state -> List Game
  }

mkGame'' :: 
  forall message state. 
  state 
  -> (message -> state -> state) 
  -> (state -> View) 
  -> (state -> List Subscription) 
  -> (state -> List Game) 
  -> Game'' message state
mkGame'' st u r s c = Game'' {state'': st, update'': u, render'': r, subscriptions'': s, children'': c}

update'' :: forall message state. message -> Game'' message state -> Game'' message state
update'' message (Game'' game'') = Game'' $ game'' {state'' = game''.update'' message game''.state''}

render'' :: forall message state. Game'' message state -> View
render'' (Game'' game'') = game''.render'' game''.state''

subscriptions'' :: forall message state. Game'' message state -> List Subscription
subscriptions'' (Game'' game'') = game''.subscriptions'' game''.state''

children'' :: forall message state. Game'' state message -> List Game
children'' (Game'' game'') = game''.children'' game''.state''

-----------------------------------------------------------------------------------

newtype Game' message = Game' (Exists (Game'' message))

liftGame'' :: forall message state. Game'' message state -> Game' message
liftGame'' = Game' <<< mkExists

mkGame' :: 
  forall message state. 
  state 
  -> (message -> state -> state) 
  -> (state -> View) 
  -> (state -> List Subscription) 
  -> (state -> List Game) 
  -> Game' message
mkGame' st u r s c = liftGame'' $ mkGame'' st u r s c

update' :: forall message. message -> Game' message -> Game' message
update' message (Game' game') = runExists (liftGame'' <<< update'' message) game'

render' :: forall message. Game' message -> View
render' (Game' game') = runExists render'' game'

subscriptions' :: forall message. Game' message -> List Subscription
subscriptions' (Game' game') = runExists subscriptions'' game'

children' :: forall message. Game' message -> List Game
children' (Game' game') = runExists children'' game'

-----------------------------------------------------------------------------------

newtype Game = Game (Exists Game')

liftGame' :: forall message. Game' message -> Game
liftGame' = Game <<< mkExists

mkGame :: 
  forall message state. 
  state 
  -> (message -> state -> state) 
  -> (state -> View) 
  -> (state -> List Subscription) 
  -> (state -> List Game) 
  -> Game
mkGame st u r s c = liftGame' $ mkGame' st u r s c

-- games must be parametrized by their messages to be updated
-- update :: ??? -> Game -> Game
-- update = ???

render :: Game -> View
render (Game game) = runExists render' game

subscriptions :: Game -> List Subscription
subscriptions (Game game) = runExists subscriptions' game

children :: Game -> List Game
children (Game game) = runExists children' game