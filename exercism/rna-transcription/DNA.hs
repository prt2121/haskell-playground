module DNA where

toRNA :: String -> String
toRNA = map f
          where f x = case x of
                    'G' -> 'C'
                    'C' -> 'G'
                    'T' -> 'A'
                    'A' -> 'U'
                    _   -> error "Invalid DNA"
