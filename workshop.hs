{-# LANGUAGE ScopedTypeVariables #-}
import Reflex.Dom

workshop :: forall t m. MonadWidget t m => m ()
workshop = do
  text "Welcome to the workshop!"
  t <- textInput def
  let apiKey :: Dynamic t String = _textInput_value t
  b :: Event t () <- button "Send Request"
  let apiKeyButtonEvent :: Event t String = tagDyn apiKey b
      apiKeyEnterEvent :: Event t String = tagDyn apiKey $ textInputGetEnter t
  submittedApiKey :: Dynamic t String <- holdDyn "NO STRING SUBMITTED" apiKeyButtonEvent -- holdDyn :: a -> Event a -> m (Dynamic a)
  dynText submittedApiKey
  return ()

main :: IO()
main = mainWidget workshop
