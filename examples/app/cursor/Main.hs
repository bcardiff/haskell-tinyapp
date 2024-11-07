module Main where

import TinyApp.Interactive
import TinyApp.Interactive.Test

main :: IO ()
main = runInteractive cursor

maxSize :: Int
maxSize = 20

cursor :: Sandbox Int
cursor =
  Sandbox
    { initialize = 1,
      render = \i -> "\n[" <> replicate (i - 1) ' ' <> "*" <> replicate (maxSize - i) ' ' <> "]",
      update = \(Key key _) i ->
        case key of
          KEsc -> (i, Exit)
          KChar 'q' -> (i, Exit)
          KLeft -> (max (i - 1) 1, Continue)
          KRight -> (min (i + 1) maxSize, Continue)
          _ -> (i, Continue)
    }

-- >>> mainTests
-- ExitSuccess

mainTests :: IO ()
mainTests =
  runTestsFor
    cursor
    [ "starts at 2" ~> do
        expectStateEq 1
        expectRenderEq "\n[*                   ]",
      "exits on esc" ~> do
        pressKey KEsc
        expectExit,
      "don't go below 1" ~> do
        pressKey KLeft
        expectStateEq 1,
      "goes right and left" ~> do
        pressKey KRight
        expectStateEq 2
        pressKey KLeft
        expectStateEq 1,
      "don't go above 20" ~> do
        pressKeys (replicate 30 KRight)
        expectStateEq 20
        expectRenderEq "\n[                   *]"
        pressKey KLeft
        expectStateEq 19
        expectRenderEq "\n[                  * ]"
    ]
