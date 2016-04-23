{-# LANGUAGE ScopedTypeVariables #-}
import Reflex.Dom

workshop :: MonadWidget t m => m ()
workshop = do
  text "Welcome to the workshop!"
  t <- textInput def
  let apiKey = _textInput_value t
  b <- button "Send Request"
  let apiKeyEvent = tagDyn apiKey b
  return ()

main :: IO()
main = mainWidget workshop
