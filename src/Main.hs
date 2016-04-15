{-# LANGUAGE OverloadedStrings #-}

module Assignment4 where
  import Network.HTTP.Client

  startsWith :: Eq a => [a] -> [a] -> Bool
  startsWith [] _ = True
  startsWith _ [] = False
  startsWith a b = and $ zipWith (==) a b

  getSubStringIndex :: String -> String -> Maybe Int
  getSubStringIndex sub mainString = mediator sub 0 mainString
    where mediator :: String -> Int -> String -> Maybe Int
          mediator sub indexSoFar remaining@(x:xs)
            | (length sub) > (length remaining) = Nothing
            | otherwise =
                case startsWith sub remaining of
                True -> Just indexSoFar
                False -> mediator sub (indexSoFar + 1) xs

  main :: IO ()
  main = do
    manager <- newManager defaultManagerSettings

    request <- parseUrl "http://cps506.sarg.ryerson.ca"
    response <- httpLbs request manager

    let body = responseBody response
    print "Hi"
    -- print :t body

    -- print $ case getSubStringIndex "<html>" "<html>dsfslkfjfl" of
    --   Nothing  -> "Damn, didn't find."
    --   Just result -> show result
