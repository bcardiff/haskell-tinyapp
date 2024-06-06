module TinyApp.Repl where

import Control.Exception
import Control.Monad
import Data.Text
import Data.Text.IO
import System.IO

data ContinueExit = Continue | Exit
  deriving (Eq, Show)

data Sandbox state = Sandbox
  { -- Initial state
    initialize :: state,
    -- The prompt to show. It can depend on the state
    prompt :: state -> Text,
    -- Process the user input given the current state
    -- Returns the next state, the output and whether to continue or not the program
    update :: Text -> state -> (state, Text, ContinueExit)
  }

runRepl :: Sandbox s -> IO ()
runRepl = Control.Monad.void . runRepl'

runRepl' :: Sandbox s -> IO s
runRepl' config =
  let go :: Sandbox s -> s -> IO s
      go config_ state = do
        Data.Text.IO.putStr (config_.prompt state)
        -- Since the prompt does not finish in a newline we force a flush right after
        hFlush stdout
        -- When Control-D an exception is thrown
        input <- Control.Exception.catch @IOException (Just <$> Data.Text.IO.getLine) (\_ -> pure Nothing)
        case input of
          Nothing -> pure state
          Just input' -> do
            let (state', output, continue) = config_.update input' state
            Data.Text.IO.putStrLn output
            case continue of
              Continue ->
                go config_ state'
              Exit ->
                pure state'
   in do
        go config config.initialize
