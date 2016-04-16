-- Name:   Farzad Vafaeinezhad, Samuel Dindyal
-- Course: CPS506, Winter 2016, Assignment #4
-- Due:    2016.04.15 23:59
-- This is entirely my own work.


module Assignment4 where
  import Network.HTTP.Client

  import Text.Regex.Posix

  import Data.List
  import Data.List.Split

  import Data.Map (Map)
  import qualified Data.Map as Map

  crawl :: String -> IO()
  crawl url = do
    manager <- newManager defaultManagerSettings

    request <- parseUrl url
    response <- httpLbs request manager

    parseHTML (show (responseBody response)) url

  parseHTML :: String -> String -> IO()
  parseHTML body url = do
    let tags = concat(Prelude.map extractTags $ splitOn ">" body)
    let links = concat(Prelude.map extractTags $ splitOn "/a>" body)

    -- Get a list with one of each tag type
    let tagsFound = Data.List.nub tags

    putStrLn ("-------------------------------")
    putStrLn ("URL: " ++ url)
    putStrLn ("-------------------------------")

    printTagCounts tagsFound body

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
    let (_, link, content) = text =~ "href=\"(http://[^\"]*)\"" :: (String, String, String)

    if not (link == "" && content == "")
      then do
        Prelude.map last (text =~ "href=\"(http://[^\"]*)\"" :: [[String]])
      else []

  startOn :: String -> [String] -> IO()
  startOn url [] = crawl url

  main :: IO ()
  main = do
    startOn "http://cps506.sarg.ryerson.ca" []
