{-# LANGUAGE ScopedTypeVariables #-}
import Reflex.Dom
import qualified Data.Text as T
import Data.Monoid
import Data.Maybe

workshop :: forall t m. MonadWidget t m => m ()
workshop = do
  text "Welcome to the workshop!"
  t <- textInput def
  let apiKey :: Dynamic t String = _textInput_value t
  b :: Event t () <- button "Send Request"
  let apiKeyButtonEvent :: Event t String = tagDyn apiKey b
      apiKeyEnterEvent :: Event t String = tagDyn apiKey $ textInputGetEnter t
      apiKeyEvent :: Event t String = leftmost [apiKeyButtonEvent, apiKeyEnterEvent]
  submittedApiKey :: Dynamic t String <- holdDyn "NO STRING SUBMITTED" apiKeyEvent -- holdDyn :: a -> Event a -> m (Dynamic a)
  dynText submittedApiKey
  let req = fmap apiKeyToXhrRequest apiKeyEvent
  rsp <- performRequestAsync req
  -- _xhrResponse_responseText :: _xhrResponse_responseText rsp
  let rspText :: Event t (Maybe T.Text) = fmap _xhrResponse_responseText rsp
      -- rspString :: Event t String = fmap (\rt -> T.unpack $ fromMaybe T.empty rt) rspText
      rspString :: Event t String = fmapMaybe (\mt -> fmap T.unpack mt) rspText
  rspDyn <- holdDyn "No Res" rspString
  dynText rspDyn
  return ()

apiKeyToXhrRequest :: String -> XhrRequest
apiKeyToXhrRequest k = XhrRequest { _xhrRequest_method = "GET"
                                  , _xhrRequest_url = "https://api.nasa.gov/planetary/apod?api_key=" <> k -- monoid mconcat
                                  , _xhrRequest_config = def
                                  }

main :: IO()
main = mainWidget workshop
