-- | Build interactice apps that reacts to each keystroke and renders text
-- Requires `ghc-options: -threaded`
module TinyApp.Interactive
  ( Event (..),
    Key (..),
    Modifier (..),
    Sandbox (..),
    ContinueExit (..),
    runInteractive,
    runInteractive',
  )
where

import Brick qualified as B
import Brick.Main qualified as BM
import Brick.Types qualified as BT
import Brick.Widgets.Core qualified as BWC
import Control.Monad
import Control.Monad.State
import Graphics.Vty qualified as V
import Graphics.Vty.Input.Events qualified as VE

data Key
  = KEsc
  | KChar Char
  | KBS
  | KEnter
  | KLeft
  | KRight
  | KUp
  | KDown
  | KUpLeft
  | KUpRight
  | KDownLeft
  | KDownRight
  | KCenter
  | KFun Int
  | KBackTab
  | KPrtScr
  | KPause
  | KIns
  | KHome
  | KPageUp
  | KDel
  | KEnd
  | KPageDown
  | KBegin
  | KMenu
  deriving (Eq, Show, Read, Ord)

fromVKey :: V.Key -> Key
fromVKey V.KEsc = KEsc
fromVKey (V.KChar char) = KChar char
fromVKey V.KBS = KBS
fromVKey V.KEnter = KEnter
fromVKey V.KLeft = KLeft
fromVKey V.KRight = KRight
fromVKey V.KUp = KUp
fromVKey V.KDown = KDown
fromVKey V.KUpLeft = KUpLeft
fromVKey V.KUpRight = KUpRight
fromVKey V.KDownLeft = KDownLeft
fromVKey V.KDownRight = KDownRight
fromVKey V.KCenter = KCenter
fromVKey (V.KFun int) = KFun int
fromVKey V.KBackTab = KBackTab
fromVKey V.KPrtScr = KPrtScr
fromVKey V.KPause = KPause
fromVKey V.KIns = KIns
fromVKey V.KHome = KHome
fromVKey V.KPageUp = KPageUp
fromVKey V.KDel = KDel
fromVKey V.KEnd = KEnd
fromVKey V.KPageDown = KPageDown
fromVKey V.KBegin = KBegin
fromVKey V.KMenu = KMenu

-- Graphics.Vty.Input.Events.Modifier
data Modifier = MShift | MCtrl | MMeta | MAlt
  deriving (Eq, Show, Read, Ord)

fromVModifier :: V.Modifier -> Modifier
fromVModifier V.MShift = MShift
fromVModifier V.MCtrl = MCtrl
fromVModifier V.MMeta = MMeta
fromVModifier V.MAlt = MAlt

-- | Event the application can receive
data Event = Key Key [Modifier]

-- | Signals whether the application should continue waiting input from the user or exit.
data ContinueExit = Continue | Exit
  deriving (Eq, Show)

-- | Defines an interactive application that is not allowed to perform arbitrary IO while executing.
data Sandbox state = Sandbox
  { -- | Initial state
    initialize :: state,
    -- | What to draw based on the current state.
    -- The screen is cleared between render calls. Usually use '\n' or *Prelude.unlines* to render multiple lines.
    render :: state -> String,
    -- | Process the event given the current state
    -- Returns the next state and whether to continue or not the program
    update :: Event -> state -> (state, ContinueExit)
  }

-- | Executes the application.
runInteractive :: Sandbox s -> IO ()
runInteractive = Control.Monad.void . runInteractive'

-- | Executes the application returning its final state.
runInteractive' :: forall s. Sandbox s -> IO s
runInteractive' config =
  let app :: BM.App s e ()
      app =
        BM.App
          { BM.appDraw = \s -> [BWC.str (config.render s)],
            BM.appChooseCursor = \_ _ -> Nothing,
            BM.appHandleEvent = \e -> do
              s <- get
              case e of
                BT.VtyEvent (VE.EvKey k ms) ->
                  case config.update (Key (fromVKey k) (map fromVModifier ms)) s of
                    (s', Continue) -> do
                      put s'
                    (s', Exit) -> do
                      put s'
                      BM.halt
                _ ->
                  return (),
            BM.appStartEvent = return (),
            BM.appAttrMap = \_ -> B.attrMap V.defAttr []
          }
   in BM.defaultMain app config.initialize
