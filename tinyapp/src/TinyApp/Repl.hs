module TinyApp.Repl where

import Control.Exception
import Control.Monad
import System.IO

data ContinueExit = Continue | Exit
  deriving (Eq, Show)

data Sandbox state = Sandbox
  { -- Initial state
    initialize :: state,
    -- The prompt to show. It can depend on the state
    prompt :: state -> String,
    -- Process the user input given the current state
    -- Returns the next state, the output and whether to continue or not the program
    update :: String -> state -> (state, String, ContinueExit)
  }

runRepl :: Sandbox s -> IO ()
runRepl = Control.Monad.void . runRepl'

runRepl' :: forall s. Sandbox s -> IO s
runRepl' config =
  let go :: s -> IO s
      go state = do
        System.IO.putStr (config.prompt state)
        -- Since the prompt does not finish in a newline we force a flush right after
        hFlush stdout
        -- When Control-D an exception is thrown
        input <- Control.Exception.catch @IOException (Just <$> System.IO.getLine) (\_ -> pure Nothing)
        case input of
          Nothing -> pure state
          Just input' -> do
            let (state', output, continue) = config.update input' state
            System.IO.putStrLn output
            case continue of
              Continue ->
                go state'
              Exit ->
                pure state'
   in do
        go config.initialize
