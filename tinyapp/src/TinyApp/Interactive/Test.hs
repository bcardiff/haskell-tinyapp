module TinyApp.Interactive.Test
  ( TestCase,
    (~>),
    runTestsFor,
    expectRenderEq,
    expectRenderIs,
    expectStateEq,
    expectStateIs,
    expectContinue,
    expectExit,
    pressKey,
    pressKeys,
    pressKey',
    inputString,
    sendEvent,
  )
where

import Control.Monad.Reader qualified as MTL
import Data.IORef
import GHC.Stack qualified as Stack
import System.Exit (exitFailure, exitSuccess)
import System.IO.Error (ioeGetErrorString, tryIOError)
import TinyApp.Interactive

data AppState s = AppState
  { state :: s,
    continue :: ContinueExit,
    output :: String
  }

data Env s = Env
  { app :: Sandbox s,
    appState :: IORef (AppState s)
  }

newtype InteractiveM s a = InteractiveM (MTL.ReaderT (Env s) IO a)
  deriving (Functor, Applicative, Monad, MTL.MonadIO, MTL.MonadFail)

data TestCase s = TestCase
  { description :: String,
    t :: InteractiveM s ()
  }

infix 0 ~>

(~>) :: String -> InteractiveM s () -> TestCase s
(~>) description test = TestCase {description = description, t = test}

runTestsFor :: Sandbox s -> [TestCase s] -> IO ()
runTestsFor app tests = do
  res <-
    MTL.forM
      tests
      ( \TestCase {description, t} -> do
          res <- tryIOError (runInteractiveTest app t)
          case res of
            Left err -> do
              putStrLn ("🛑 " <> description <> ": " <> ioeGetErrorString err)
              pure False
            Right _ -> do
              putStrLn ("✅ " <> description)
              pure True
      )
  if and res
    then
      exitSuccess
    else
      exitFailure

expectRenderEq :: (Stack.HasCallStack) => String -> InteractiveM s ()
expectRenderEq value = do
  AppState {output} <- readAppState
  _expectEq Stack.callStack output value

expectRenderIs :: (Stack.HasCallStack, Show s, Eq s) => (String -> Bool) -> InteractiveM s ()
expectRenderIs f = do
  AppState {output} <- readAppState
  _expectIs Stack.callStack "render" output f

expectStateEq :: (Stack.HasCallStack, Show s, Eq s) => s -> InteractiveM s ()
expectStateEq value = do
  AppState {state} <- readAppState
  _expectEq Stack.callStack state value

expectStateIs :: (Stack.HasCallStack, Show s, Eq s) => (s -> Bool) -> InteractiveM s ()
expectStateIs f = do
  AppState {state} <- readAppState
  _expectIs Stack.callStack "state" state f

expectContinue :: (Stack.HasCallStack) => InteractiveM s ()
expectContinue =
  _expectContinue Stack.callStack

_expectContinue :: Stack.CallStack -> InteractiveM s ()
_expectContinue callstack = do
  AppState {continue} <- readAppState
  _expectEq callstack continue Continue

expectExit :: (Stack.HasCallStack) => InteractiveM s ()
expectExit = do
  AppState {continue} <- readAppState
  _expectEq Stack.callStack continue Exit

inputString :: String -> InteractiveM s ()
inputString str = pressKeys (map KChar str)

pressKey :: (Stack.HasCallStack) => Key -> InteractiveM s ()
pressKey key = Stack.withFrozenCallStack $ pressKey' key []

pressKeys :: (Stack.HasCallStack) => [Key] -> InteractiveM s ()
pressKeys keys = Stack.withFrozenCallStack $ MTL.forM_ keys pressKey

pressKey' :: (Stack.HasCallStack) => Key -> [Modifier] -> InteractiveM s ()
pressKey' key modifiers = _sendEvent Stack.callStack (Key key modifiers)

sendEvent :: (Stack.HasCallStack) => Event -> InteractiveM s ()
sendEvent = _sendEvent Stack.callStack

_sendEvent :: Stack.CallStack -> Event -> InteractiveM s ()
_sendEvent callstack event = do
  _expectContinue callstack
  app' <- getApp
  InteractiveM $ do
    appState' <- MTL.asks appState
    MTL.liftIO $ modifyIORef appState' (updateAppState app' event)

updateAppState :: Sandbox s -> Event -> AppState s -> AppState s
updateAppState app event AppState {state} =
  let (state', continue') = app.update event state
      output' = app.render state'
   in AppState
        { state = state',
          continue = continue',
          output = output'
        }

getApp :: InteractiveM s (Sandbox s)
getApp = InteractiveM $ MTL.asks app

readAppState :: InteractiveM s (AppState s)
readAppState = InteractiveM $ do
  appState' <- MTL.asks appState
  MTL.liftIO $ readIORef appState'

_expectEq :: (Show a, Eq a) => Stack.CallStack -> a -> a -> InteractiveM s ()
_expectEq callstack actual expected =
  if actual == expected
    then
      pure ()
    else
      fail $ "Expected " <> show expected <> " but got " <> show actual <> ".\n" <> Stack.prettyCallStack callstack

_expectIs :: Stack.CallStack -> String -> a -> (a -> Bool) -> InteractiveM s ()
_expectIs callstack label actual f =
  if f actual
    then
      pure ()
    else
      fail $ "Expected " <> label <> " to satisfy predicate.\n" <> Stack.prettyCallStack callstack

runInteractiveTest :: Sandbox s -> InteractiveM s a -> IO a
runInteractiveTest app (InteractiveM m) = do
  let state = app.initialize
  appState <-
    newIORef
      AppState
        { state = state,
          continue = Continue,
          output = app.render state
        }
  MTL.runReaderT m Env {app = app, appState = appState}
