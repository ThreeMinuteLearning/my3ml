{-# LANGUAGE OverloadedStrings    #-}

module TestData where

import Api.Types

schools :: [School]
schools =
    [ School "3" "T&J Academy" Nothing
    , School "4" "Pirate School" Nothing
    ]

classes :: [Class]
classes =
    [ Class "1" "A1" Nothing "3" []
    , Class "2" "A2" Nothing "3" []
    , Class "3" "B1" Nothing "3" []
    , Class "4" "P1" Nothing "4" []
    , Class "5" "P2" Nothing "4" []
    ]

students :: [Student]
students =
    [ Student "1" "Jerry Mouse" Nothing 5 "" "3"
    , Student "2" "Tom Cat" Nothing 3 "" "3"
    , Student "3" "Butch" Nothing 3 "" "3"
    , Student "4" "Nibbles" Nothing 2 "" "3"
    , Student "5" "Tyke" Nothing 2 "" "3"
    , Student "6" "Jack Sparrow" Nothing 8 "" "4"
    ]
