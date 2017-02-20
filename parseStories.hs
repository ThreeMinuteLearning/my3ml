import Text.HTML.TagSoup
import qualified Data.Text as T

data Story = Story
    { id_ :: String
    , img :: String
    , title :: String
    , tags :: [String]
    , level :: String
    , completed :: String
    , date :: String
    , content :: String
    } deriving (Show)


parseSection :: [Tag String] -> Story
parseSection s =
    let cols = partitions (~== "<td>") s
        [image, title, tag1, tag2, tag3, level, completed, date, _] = reverse $ foldl accTxt [] (take 9 cols)
        accTxt ss t = (trim (innerText t)) : ss
        image' = parseImage (head cols)
        ts = filter (not . null) [tag1, tag2, tag3]
        rem = drop 9 cols
        id_ = parseIdFromLink (head rem)
        content = parseContent (head (tail rem))
    in
        Story id_ image' title ts level completed (parseDate date) content


parseContent :: [Tag String] -> String
parseContent = trim . renderTags . takeWhile (~/= "</td>") . drop 1


parseImage :: [Tag String] -> String
parseImage = takeWhile (/= ')') . drop 23 . trim . fromAttrib "style" . head . dropWhile (~/= TagOpen "div" [])

parseIdFromLink :: [Tag String] -> String
parseIdFromLink = tail . dropWhile (/= '=') . fromAttrib "href" . head . dropWhile (~/= "<a>")

parseDate :: String -> String
parseDate = trim . take 19


trim :: String -> String
trim = T.unpack . T.strip . T.pack


findStartOfStories :: [Tag String] -> [Tag String]
findStartOfStories = dropWhile (~/= TagOpen "tr" []) . dropWhile (~/= TagOpen "tbody" [])


parseSections = partitions (~== "<tr>") . findStartOfStories . parseTags

main :: IO ()
main = do
    htmlStories <- readFile "allstories.txt"
    let secs = parseSections htmlStories

        stories = map parseSection secs
    print stories
