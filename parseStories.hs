{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics
import Prelude hiding (id)
import Text.HTML.TagSoup
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Data.List (nub, elemIndex)
import qualified Data.Aeson as J


data Story = Story
    { id :: String
    , img :: String
    , title :: String
    , tags :: [String]
    , level :: Int
    , completed :: Int
    , definitions :: [Definition]
    , date :: String
    , content :: String
    , popups :: [Definition]
    } deriving (Show, Generic)

data Definition = Definition
    { word :: String
    , index :: Int
    } deriving (Eq, Show, Generic)

instance J.ToJSON Story where
    toJSON     = J.genericToJSON J.defaultOptions
    toEncoding = J.genericToEncoding J.defaultOptions

instance J.ToJSON Definition where
    toJSON     = J.genericToJSON J.defaultOptions
    toEncoding = J.genericToEncoding J.defaultOptions


parseSection :: [Tag String] -> Story
parseSection s =
    let
        cols = partitions (~== "<td>") s
        [image, title, tag1, tag2, tag3, level, completed, date, _] = reverse $ foldl accTxt [] (take 9 cols)
        accTxt ss t = trim (innerText t) : ss
        image' = parseImage (head cols)
        ts = filter (not . null) [tag1, tag2, tag3]
        rem = drop 9 cols
        id_ = parseIdFromLink (head rem)
        (content, defs, popups) = parseContent (head (tail rem))
    in
        Story id_ image' title ts (read level) (read completed) (nub defs) (parseDate date) content popups

parseDefinition :: String -> [Tag String] -> Maybe Definition
parseDefinition d ts = case d `elemIndex` ["define","define2","define3","define4"] of
    Just i -> case innerText $ takeWhile (~/= TagClose "span") ts of
        "Words in pop-ups" -> Nothing -- There is one story where this happens
        txt -> Just $ Definition txt i
    Nothing -> error ("Unhandled span class " ++ d)


parsePopUps :: [Tag String] -> [Definition]
parsePopUps [] = error "End of input while parsing popup words"
parsePopUps (t:ts) = case t of
    TagOpen "span" (("class", d):_cs) ->
        case parseDefinition d ts of
            Just defn -> defn : parsePopUps (tail ts)
            Nothing -> parsePopUps ts
    TagClose "td" -> []
    _ -> parsePopUps ts


parseContent :: [Tag String] -> (String, [Definition], [Definition])
parseContent ts = go ("", [], []) (drop 1 ts)
  where
    go acc [] = acc
    go acc@(content, defs, popups) (t:ts) =
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
                let
                    newAcc = case parseDefinition d ts of
                        Nothing -> acc
                        Just defn -> (content ++ word defn, defn : defs, popups)
                in
                    go newAcc (dropWhile (~/= TagClose "span") ts)
            TagOpen "span" [] -> go acc ts
            TagOpen "em" _ -> go (content ++ "_", defs, popups) ts
            TagOpen "p" _ -> go (content ++ "\n\n", defs, popups) ts
            TagOpen "a" _ ->
                let
                    url = fromAttrib "href" t
                    inside = takeWhile (~/= TagClose "a") ts
                    lnk = "[" ++ innerText inside ++ "](" ++ url ++ ")"
                in
                    go (content ++ lnk, defs, popups) (tail $ dropWhile (~/= TagClose "a") ts)
            TagClose"em" -> go (content ++ "_ ", defs, popups) ts
            TagOpen "strong\"" _ -> go acc (drop 2 ts) -- specific tag error in J Cooper story
            TagOpen "strong" _ -> case ts of
                (TagText s : TagClose "strong" : ts') -> case trim s of
                    "Words in pop-ups" -> (content, defs, parsePopUps ts')
                    _ -> go (content ++ "__" ++ s ++ "__", defs, popups) ts'
                _ -> case head ts of
                    TagOpen "span" _ -> go acc (tail ts) -- style span within strong
                    _ -> error ("open strong tag with weird successors" ++ show (head ts))
            TagOpen "img" _ ->
                let
                    src = fromAttrib "src" t
                    img = "![fixme](" ++ src ++ ")"
                in
                    go (content ++ img, defs, popups) (tail ts)

            TagClose "img" -> go acc ts
            TagOpen "ul" _ -> go acc (dropWhile (~/= "<li>") ts)
            TagOpen "li" _ ->
                let
                  txt = innerText $ takeWhile (~/= TagClose "li") ts
                in
                  go (content ++ "\n+" ++ txt, defs, popups) (dropWhile (~/= TagClose "li") ts)

            TagClose "li" -> go acc ts
            TagClose "ul" -> go acc ts
            TagClose "td" -> (trim content, defs, popups)
            _ -> go (content ++ renderTags [t], defs, popups) ts


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
