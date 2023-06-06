module Game2
  ( Game
  , Game'
  , PixiGame
  , PixiView
  , children
  , children'
  , mkGame
  , mkGame'
  , render'
  , update'
  )
  where

-----------------------------------------------------------------------------------

import Prelude
import Data.List (List)
import Data.Exists2 (Exists2, mkExists2, runExists2)

-----------------------------------------------------------------------------------

newtype Game' view msg model =
  Game' {
    model' ∷ model
  , update' ∷ msg -> model -> model
  , render' ∷ model -> view msg
  , children' ∷ model -> List (Game view)
  --, subs' ∷ model -> List (sub msg)
  }

mkGame' ∷ 
  ∀ view msg model. 
  model 
  -> (msg -> model -> model) 
  -> (model -> view msg) 
  -> (model -> List (Game view)) 
  -> Game' view msg model
mkGame' m u r c = Game' {model': m, update': u, render': r, children': c}

update' ∷ 
  ∀ view msg model. 
  msg 
  -> Game' view msg model 
  -> Game' view msg model
update' message (Game' game') = Game' $ game' {model' = game'.update' message game'.model'}

render' ∷ 
  ∀ view msg model. 
  Game' view msg model 
  -> view msg
render' (Game' game') = game'.render' game'.model'

children' ∷ 
  ∀ view msg model. 
  Game' view msg model 
  -> List (Game view)
children' (Game' game') = game'.children' game'.model'

-----------------------------------------------------------------------------------

newtype Game view = Game (Exists2 (Game' view))

liftGame ∷ 
  ∀ view msg model. 
  Game' view msg model 
  -> Game view
liftGame = Game <<< mkExists2

mkGame ∷ 
  ∀ view msg model. 
  model 
  -> (msg -> model -> model) 
  -> (model -> view msg) 
  -> (model -> List (Game view)) 
  -> Game view
mkGame m u r c = liftGame $ mkGame' m u r c

children ∷ 
  ∀ view.
  (Game view)
  -> List (Game view)
children (Game game) = runExists2 children' game

-----------------------------------------------------------------------------------

newtype PixiView msg = PixiView Unit
type PixiGame = Game PixiView

-----------------------------------------------------------------------------------