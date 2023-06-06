--https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md
--spago repl
--import Game
--:r

module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "🍝"
