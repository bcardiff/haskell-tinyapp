module Main where

import TinyApp.Repl as Repl
import TinyApp.Text

main :: IO ()
main =
  Repl.runRepl
    Repl.Sandbox
      { initialize = 1 :: Int,
        prompt = \i -> tshow i <> "> ",
        update = \input state ->
          ( state + 1,
            input,
            if input == "quit"
              then Exit
              else Continue
          )
      }
