{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module HasqlDB where

import           Control.Monad (replicateM)
import           Data.Functor.Contravariant
import           Data.List (foldl')
import           Data.Maybe (fromJust)
import           Data.Monoid
import           Data.Text (Text)
import           Data.Vector (Vector)
import           Hasql.Query (Query)
import qualified Hasql.Query as Q
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import           Prelude hiding (id, words)

import Api.Types

dvText :: D.Row Text
dvText = D.value D.text

evText :: E.Params Text
evText = E.value E.text

selectAllStories :: Query () (Vector Story)
selectAllStories =
    Q.statement sql mempty (D.rowsVector decoder) True
  where
    sql = "SELECT id, title, img_url, level, curriculum, tags, content, words, created_at FROM story"

    dictEntryValue = D.composite (DictEntry <$> D.compositeValue D.text <*> (fromIntegral <$> D.compositeValue D.int2))
    array v = D.value (D.array (D.arrayDimension replicateM (D.arrayValue v)))

    decoder = Story
        <$> (Just <$> dvText)
        <*> dvText
        <*> dvText
        <*> (fromIntegral <$> D.value D.int2)
        <*> dvText
        <*> array D.text
        <*> dvText
        <*> array dictEntryValue
        <*> D.value D.timestamptz

insertStory :: Query Story ()
insertStory = Q.statement sql storyEncoder D.unit True
  where
    sql = "INSERT INTO story (id, title, img_url, level, curriculum, tags, content, words) \
                 \VALUES ($1, $2, $3, $4, $5, $6, $7, (array(select word::dict_entry from unnest ($8, $9) as word)))"

storyEncoder :: E.Params Story
storyEncoder =
    contramap (fromJust . storyId) evText <>
    contramap title evText <>
    contramap img evText <>
    contramap (fromIntegral . storyLevel) (E.value E.int4) <>
    contramap curriculum evText <>
    contramap tags (array E.text) <>
    contramap content evText <>
    contramap (map word . words) (array E.text) <>
    contramap (map (fromIntegral . index) . words) (array E.int2)
  where
    array v = E.value (E.array (E.arrayDimension foldl' (E.arrayValue v)))
    storyId = id :: Story -> Maybe Text
    storyLevel = level :: Story -> Int
