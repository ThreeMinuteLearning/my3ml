module Types exposing (Model, Msg(..), Answers, Story, ClarifyMethod(..), CustomError(..), Definition(..), DrawerType(..))

import Date exposing (Date)
import Form exposing (Form)
import Login exposing (User(..))
import RemoteData exposing (WebData)
import Routing exposing (Page(..))
import Table


type Msg
    = ChangePage Page
    | Navigate Page
    | StoriesResponse (WebData (List Story))
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
    , words : List Definition
    , date : Date
    , content : String
    }


type Definition
    = Definition String Int


type alias Model =
    { login : Login.Model
    , user : Login.User
    , page : Page
    , stories : WebData (List Story)
    , storyFilter : String
    , tableState : Table.State
    , showDrawer : Maybe DrawerType
    , answersForm : Form CustomError Answers
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
