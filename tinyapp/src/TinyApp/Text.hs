module TinyApp.Text
  ( Text,
    tshow,
  )
where

import qualified Data.Text

type Text = Data.Text.Text

tshow :: (Show a) => a -> Text
tshow = Data.Text.pack . show