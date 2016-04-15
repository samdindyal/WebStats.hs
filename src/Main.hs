import Network.HTTP.Client

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings

  request <- parseUrl "http://cps506.sarg.ryerson.ca"
  response <- httpLbs request manager

  let body = responseBody response

  print body
