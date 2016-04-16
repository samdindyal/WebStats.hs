-- Name:   Farzad Vafaeinezhad, Samuel Dindyal
-- Course: CPS506, Winter 2016, Assignment #4
-- Due:    2016.04.15 23:59
-- This is entirely my own work.


module Assignment4 where
  import Network.HTTP.Client
  import Network.HTTP.Types.Status (statusCode)
  import Network.URI

  import Text.Regex.Posix

  import Data.List
  import Data.List.Split

  import Data.Map (Map)
  import qualified Data.Map as Map

  crawl :: String -> Integer -> IO()
  crawl url maxFollow = do
    putStrLn url
    manager <- newManager defaultManagerSettings

    request <- parseUrl url
    response <- httpLbs request manager
    if ((show $ statusCode $ responseStatus response) <= (show 299))
      then do
        parseHTML (show (responseBody response)) url maxFollow
      else print("")

  parseHTML :: String -> String -> Integer -> IO()
  parseHTML body url maxFollow = do
    let tags = concat(Prelude.map extractTags $ splitOn ">" body)
    let links = concat(Prelude.map extractLinks $ splitOn "/a>" body)


    -- Get a list with one of each tag type
    let tagsFound = Data.List.nub tags

    putStrLn ("-------------------------------")
    putStrLn ("URL: " ++ url)
    putStrLn ("-------------------------------")

    printTagCounts tagsFound body
    follow links maxFollow maxFollow

  follow :: [String] -> Integer -> Integer -> IO()
  follow _ 0 _ = print ""
  follow [] _ _ = print ""
  follow (link:links) counter maxFollow = do
    putStrLn (show counter)
    crawl link maxFollow
    follow links (counter - 1) maxFollow


  printTagCounts :: [String] -> String -> IO()
  printTagCounts (tag:tags) html = do
    let count = length (splitOn tag html) - 1
    putStrLn (tag ++ " " ++ (show count))
    printTagCounts tags html
  printTagCounts [] _ = putStrLn ("-------------------------------")


  extractTags :: String -> [String]
  extractTags text = do
    let (_, tag, content) = text =~ "<([a-zA-Z][a-zA-Z0-9]*[ ])" :: (String, String, String)

    -- If a match was found, add it to a map
    if not (tag == "" && content == "")
      then do
        Prelude.map last (text =~  "<([a-zA-Z][a-zA-Z0-9]*[ ])" :: [[String]])
      else []

  extractLinks :: String -> [String]
  extractLinks text = do
    let (_, link, content) = text =~ "(http://[^\"]*)" :: (String, String, String)
    if not (link == "" && content == "")
      then do
        Prelude.map last (text =~ "(http://[^\"]*)" :: [[String]])
      else []

  startOn :: String -> [String] -> IO()
  startOn url [] = case parseURI url of
    Nothing -> print "Invalid URL."
    Just _ -> crawl url 3

  main :: IO ()
  main = do
    startOn "http://cps506.sarg.ryerson.ca" []
