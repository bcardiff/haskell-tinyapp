-- | Build REPL apps
module TinyApp.Repl where

import Control.Exception
import Control.Monad
import System.IO

-- | Signals whether the application should continue asking input from the user or exit.
data ContinueExit = Continue | Exit
  deriving (Eq, Show)

-- | Defines a REPL application that is not allowed to perform arbitrary IO while executing.
data Sandbox state = Sandbox
  { -- | Initial state
    initialize :: state,
    -- | The prompt to show. It can depend on the state
    prompt :: state -> String,
    -- | Process the user input given the current state
    -- Returns the next state, the output and whether to continue or not the program
    update :: String -> state -> (state, String, ContinueExit)
  }

-- | Executes the REPL application.
runRepl :: Sandbox s -> IO ()
runRepl = Control.Monad.void . runRepl'

-- | Executes the REPL application returning its final state.
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
