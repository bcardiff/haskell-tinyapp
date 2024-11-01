module Main where

import TinyApp.Interactive as I

main :: IO ()
main = I.runInteractive cursor

maxSize :: Int
maxSize = 20

cursor :: I.Sandbox Int
cursor =
  I.Sandbox
    { initialize = 1,
      render = \i -> "\n[" <> replicate (i - 1) ' ' <> "*" <> (replicate (maxSize - i) ' ') <> "]",
      update = \(Key key _) i ->
        case key of
          KEsc -> (i, Exit)
          KChar 'q' -> (i, Exit)
          KLeft -> (max (i - 1) 1, Continue)
          KRight -> (min (i + 1) maxSize, Continue)
          _ -> (i, Continue)
    }
