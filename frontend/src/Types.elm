module Types exposing (..)

import AnswersForm
import Api exposing (Story, DictEntry)
import Dict exposing (Dict)
import Form
import Login
import RemoteData exposing (WebData)
import Routing exposing (Page(..))
import Table


type Msg
    = ChangePage Page
    | Navigate Page
    | StoriesResponse (WebData (List Story))
    | DictResponse (WebData WordDict)
    | LoginMsg (Login.Msg Api.Login)
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


type alias Definition =
    ( String, List ( String, Int ) )


type alias WordDict =
    Dict String (List Definition)


type alias Model =
    { login : Login.Model
    , user : User
    , page : Page
    , stories : WebData (List Story)
    , storyFilter : String
    , tableState : Table.State
    , showDrawer : Maybe DrawerType
    , answersForm : AnswersForm.Model
    , wordDict : WebData WordDict
    }


type User
    = Guest
    | User String UserType String


type UserType
    = Student
    | Teacher
    | Editor
    | Admin


type ClarifyWord
    = ClarifyWord String
