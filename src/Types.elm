module Types exposing (..)

import Array exposing (Array)
import Date exposing (Date)
import Dict exposing (Dict)
import Form exposing (Form)
import Login exposing (User(..))
import RemoteData exposing (WebData)
import Routing exposing (Page(..))
import Table


type Msg
    = ChangePage Page
    | Navigate Page
    | StoriesResponse (WebData (List Story))
    | DictResponse (WebData WordDict)
    | LoginMsg Login.Msg
    | StoryFilterInput String
    | SetTableState Table.State
    | ToggleDrawer DrawerType
    | FormMsg Form.Msg



-- TODO refactor connect etc into a separate type since they are used in several places


type DrawerType
    = Connect
    | Question
    | Summarise
    | Clarify


type alias Story =
    { id : String
    , img : String
    , title : String
    , tags : List String
    , level : Int
    , words : List DictEntry
    , date : Date
    , content : String
    }


type DictEntry
    = DictEntry String Int


type alias Definition =
    List ( String, List DictEntry )


type alias WordDict =
    Dict String (Array Definition)


type alias Model =
    { login : Login.Model
    , user : Login.User
    , page : Page
    , stories : WebData (List Story)
    , storyFilter : String
    , tableState : Table.State
    , showDrawer : Maybe DrawerType
    , answersForm : Form CustomError Answers
    , wordDict : WebData WordDict
    }


type ClarifyWord
    = ClarifyWord String


type ClarifyMethod
    = ReadAround
    | BreakDown
    | Substitution


type CustomError
    = InvalidClarifyMethod


type alias Answers =
    { connectAnswer : String
    , questionAnswer : String
    , summary : String
    , clarification : String
    , clarificationMethod : ClarifyMethod
    }
