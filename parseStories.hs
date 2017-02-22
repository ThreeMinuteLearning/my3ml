{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics
import Prelude hiding (id)
import Text.HTML.TagSoup
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Data.List (nub)
import qualified Data.Aeson as J


data Story = Story
    { id :: String
    , img :: String
    , title :: String
    , tags :: [String]
    , level :: Int
    , completed :: Int
    , defineWords :: [String]
    , date :: String
    , content :: String
    } deriving (Show, Generic)


instance J.ToJSON Story where
    toJSON     = J.genericToJSON J.defaultOptions
    toEncoding = J.genericToEncoding J.defaultOptions

parseSection :: [Tag String] -> Story
parseSection s =
    let
        cols = partitions (~== "<td>") s
        [image, title, tag1, tag2, tag3, level, completed, date, _] = reverse $ foldl accTxt [] (take 9 cols)
        accTxt ss t = (trim (innerText t)) : ss
        image' = parseImage (head cols)
        ts = filter (not . null) [tag1, tag2, tag3]
        rem = drop 9 cols
        id_ = parseIdFromLink (head rem)
        (content, defs) = parseContent (head (tail rem))
    in
        Story id_ image' title ts (read level) (read completed) (nub defs) (parseDate date) content


parseContent :: [Tag String] -> (String, [String])
parseContent ts = go ("", []) (drop 1 ts)
  where
    go acc [] = acc
    go acc@(content, defs) (t:ts) =
        case t of
            TagOpen "span" [("style", _)] -> go acc ts
            TagOpen "font" _ -> go acc ts
            TagClose "font" -> go acc ts
            TagOpen "div" _ -> go acc ts
            TagClose "div" -> go acc ts
            TagOpen "center" _ -> go acc ts
            TagClose "center" -> go acc ts
            TagOpen "o:p" _ -> go acc ts
            TagClose "o:p" -> go acc ts
            TagClose "span" -> go acc ts
            TagOpen "span" (("class", "apple-converted-space"):_) -> go acc ts
            TagOpen "span" (("class", d):_cs) ->
                if d `elem` ["define","define2","define3","define4"]
                  then
                      let
                         definition = case innerText $ takeWhile (~/= TagClose "span") ts of
                             "Words in pop-ups" -> ""
                             txt -> txt
                      in
                         go (content ++ definition, definition : defs) (dropWhile (~/= TagClose "span") ts)
                  else error ("Unhandled span class " ++ d)
                      -- go acc ts
            TagOpen "span" [] -> go acc ts
            TagOpen "em" _ -> go (content ++ "_", defs) ts
            TagOpen "p" _ -> go (content ++ "\n\n", defs) ts
            TagOpen "a" _ ->
                let
                    url = fromAttrib "href" t
                    inside = takeWhile (~/= TagClose "a") ts
                    lnk = "[" ++ innerText inside ++ "](" ++ url ++ ")"
                in
                    go (content ++ lnk, defs) (tail $ dropWhile (~/= TagClose "a") ts)
            TagClose"em" -> go (content ++ "_ ", defs) ts
            TagOpen "strong\"" _ -> go acc (drop 2 ts) -- specific tag error in J Cooper story
            TagOpen "strong" _ -> case ts of
                (TagText s : TagClose "strong" : ts') -> case trim s of
                    "Words in pop-ups" -> go acc ts'
                    _ -> go (content ++ "__" ++ s ++ "__", defs) ts'
                _ -> case head ts of
                    TagOpen "span" _ -> go acc (tail ts) -- style span within strong
                    _ -> error ("open strong tag with weird successors" ++ show (head ts))
            TagOpen "img" _ ->
                let
                    src = fromAttrib "src" t
                    img = "![fixme](" ++ src ++ ")"
                in
                    go (content ++ img, defs) (tail ts)

            TagClose "img" -> go acc ts
            TagOpen "ul" _ -> go acc (dropWhile (~/= "<li>") ts)
            TagOpen "li" _ ->
                let
                  txt = innerText $ takeWhile (~/= TagClose "li") ts
                in
                  go (content ++ "\n+" ++ txt, defs) (dropWhile (~/= TagClose "li") ts)

            TagClose "li" -> go acc ts
            TagClose "ul" -> go acc ts
            TagClose "td" -> (trim content, defs)
            _ -> go (content ++ renderTags [t], defs) ts


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
        printStory s = B.putStr s >> putStrLn ","
    putStrLn "{ \"stories\": ["
    mapM_ (printStory . J.encode) stories
    putStrLn "\n]\n}"
