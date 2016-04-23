{-# LANGUAGE ScopedTypeVariables #-}
import Reflex.Dom

main :: IO ()
main = mainWidget $ do
  text "Welcome to the workshop!"
  t <- textInput def
  let dynString = _textInput_value t
  dynText dynString
  return ()
