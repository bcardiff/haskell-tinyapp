module Main where

import TinyApp.Repl as Repl

main :: IO ()
main = Repl.runRepl echo

echo :: Repl.Sandbox Int
echo =
  Repl.Sandbox
    { initialize = 1,
      prompt = \i -> show i <> "> ",
      update = \input state ->
        ( state + 1,
          input,
          if input == "quit"
            then Exit
            else Continue
        )
    }
