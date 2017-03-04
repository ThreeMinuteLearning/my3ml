{-# LANGUAGE DeriveGeneric, RecordWildCards, TupleSections #-}
import Control.Monad (join)
import GHC.Generics
import Data.Char (ord)
import Prelude hiding (id)
import qualified Data.Csv as Csv
import Text.HTML.TagSoup
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (nub, elemIndex, sortOn, sort)
import Data.Vector (toList)
import qualified Data.Aeson as J

import Debug.Trace

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
    { word_ :: String
    , index :: Int
    } deriving (Eq, Ord, Show, Generic)

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
        txt -> Just $ Definition (trim txt) i
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
                        Just defn -> (content ++ word_ defn, defn : defs, popups)
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

decodeStories = do
    htmlStories <- readFile "allstories.txt"
    let secs = parseSections htmlStories
    return $ map parseSection secs

printStories stories = do
    putStrLn "{ \"stories\": "
    B.putStr $ J.encode stories
    putStrLn "\n}"


main :: IO ()
main = do
    dict <- makeSciDict
    stories <- decodeStories
    let subDefs = mergeDuplicates $ nub $ sort $ join $ map (subDefinitions dict) stories
        newDict = sortOn (T.toLower . fst) $ addSubDefsToDict subDefs (M.toList dict)

    B.putStr $ J.encode $ M.fromList newDict


addSubDefsToDict :: [(Definition, [Definition])] -> [(T.Text, [T.Text])] -> [(T.Text, [(T.Text, [(T.Text, Int)])])]
addSubDefsToDict _ [] = []
addSubDefsToDict defs ((w, ds):rem) =
    let
        toTuple d = (T.pack $ word_ d, index d)
        matchingDefs = map (map toTuple . snd) $ filter ((== w) . T.pack . word_ . fst) defs
    in
        (w, zip ds (matchingDefs ++ [[],[],[],[],[]] )) : addSubDefsToDict defs rem


mergeDuplicates :: [(Definition, [Definition])] -> [(Definition, [Definition])]
mergeDuplicates [] = []
mergeDuplicates [d] = [d]
mergeDuplicates (x@(d,ds):y@(d1,d1s):rem) =
    if d == d1
        then mergeDuplicates $ (d, nub (ds ++ d1s)) : rem
        else x : mergeDuplicates (y : rem)

subDefinitions :: M.HashMap T.Text [T.Text] -> Story -> [(Definition, [Definition])]
subDefinitions dict story = filter (not . null . snd) $ defPopups ++ popupsForPopups
  where
    defPopups = map (`popupsForDefinition` storyPopups) (definitions story)
    storyPopups = popups story

    go d = popupsForDefinition d storyPopups

    popupsForPopups =  map (\p -> popupsForDefinition p (filter (/= p) storyPopups)) storyPopups

    popupsForDefinition d ps =
        let
            wordToLookup = T.pack (word_ d)
            dictEntry = case M.lookup wordToLookup dict of
                Nothing -> error $ "No dict entry for " ++ show (word_ d)
                Just e  -> if index d >= length e
                    then error $ "There is no entry for " ++ show (word_ d) ++ " at index " ++ show (index d) ++ " in " ++ show e
                    else e !! index d
            dPopups = filter (\p -> T.pack (word_ p) `T.isInfixOf` dictEntry) ps
        in
            (d, dPopups)

myCSVOptions = Csv.defaultDecodeOptions {
      Csv.decDelimiter = fromIntegral (ord '\t')
    }

makeSciDict = do
    bytes <- B.readFile "scidict.txt"
    return $ case Csv.decodeWith myCSVOptions Csv.NoHeader bytes of
        Left e -> error e
        Right v -> foldl insertDef M.empty (Data.List.sortOn word (toList v))


insertDef :: M.HashMap T.Text [T.Text] -> Def -> M.HashMap T.Text [T.Text]
insertDef dict Def {..} =
    let
        word' = T.strip word
        (w, i) = case T.unpack $ T.takeEnd 2 word' of
            " 2" -> (T.dropEnd 2 word', 1)
            " 3" -> (T.dropEnd 2 word', 2)
            " 4" -> (T.dropEnd 2 word', 3)
            " 5" -> (T.dropEnd 2 word', 4)
            _   -> (word', 0)
        entry = M.lookup w dict
    in
        case entry of
            Just [] -> error $ "Empty dict entry for " ++ T.unpack w
            Nothing ->
                if i > 0
                    then error $ "non zero index defn but no existing entry for " ++ (show word) ++ (show def)
                    else M.insert w [def] dict
            Just ds ->
                if i > 0 && length ds == i
                    then M.insert w (reverse $ def : reverse ds) dict
                    else if i == 0 && length ds == 1
                            then M.insert w [def] dict -- overwrite the existing entry
                            else error $ "Not enough preceding entries for " ++ T.unpack w ++ " " ++ show i ++ (show ds)

data Def = Def
    { word :: T.Text
    , def :: T.Text
    } deriving (Eq, Show, Generic)

instance Csv.FromRecord Def
instance Csv.ToRecord Def
instance J.FromJSON Def
