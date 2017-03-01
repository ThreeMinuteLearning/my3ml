module Main exposing (main)

import Date exposing (Date)
import Form exposing (Form)
import Form.Input as Input
import Form.Validate as Validate exposing (..)
import Html exposing (Html, button, br, div, fieldset, img, h1, h2, h3, p, program, text, textarea, ul, li, label, input)
import Html.Attributes exposing (attribute, id, class, checked, disabled, for, name, src, style, type_, value, width)
import Html.Events exposing (onClick, onCheck, onInput, onSubmit)
import Http exposing (..)
import Json.Decode as JD
import Login exposing (User(..))
import Markdown
import Navigation exposing (..)
import RemoteData exposing (WebData)
import Regex
import Routing exposing (..)
import Table


type Msg
    = ChangePage Page
    | Navigate Page
    | StoriesResponse (WebData (List Story))
    | LoginMsg Login.Msg
    | StoryFilterInput String
    | SetTableState Table.State
    | ToggleDrawer Drawer
    | FormMsg Form.Msg



-- TODO refactor connect etc into a separate type since they are used in several places


type Drawer
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
    , showDrawer : Maybe Drawer
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


dateDecoder : JD.Decoder Date
dateDecoder =
    JD.string
        |> JD.andThen
            (\str ->
                case Date.fromString str of
                    Err err ->
                        JD.fail err

                    Ok date ->
                        JD.succeed date
            )


definitionDecoder : JD.Decoder Definition
definitionDecoder =
    JD.map2 Definition
        (JD.field "word" JD.string)
        (JD.field "index" JD.int)


storiesDecoder : JD.Decoder (List Story)
storiesDecoder =
    JD.field "stories" <|
        JD.list <|
            JD.map8 Story
                (JD.field "id" JD.string)
                (JD.field "img" JD.string)
                (JD.field "title" JD.string)
                (JD.field "tags" (JD.list JD.string))
                (JD.field "level" JD.int)
                (JD.field "definitions" (JD.list definitionDecoder))
                (JD.field "date" dateDecoder)
                (JD.field "content" JD.string)


getStories : Cmd Msg
getStories =
    Http.get "allstories.json" storiesDecoder
        |> RemoteData.sendRequest
        |> Cmd.map StoriesResponse


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( page, cmd ) =
            authRedirect (locationToPage location) Guest

        ( loginModel, loginCmd ) =
            Login.init

        initialModel =
            { login = loginModel
            , user = Guest
            , page = page
            , stories = RemoteData.Loading
            , storyFilter = ""
            , tableState = Table.initialSort "Title"
            , showDrawer = Nothing
            , answersForm = Form.initial [] answerFormValidation
            }
    in
        ( initialModel, Cmd.batch [ cmd, Cmd.map LoginMsg loginCmd, getStories ] )


authRequired : ( Page, User ) -> Bool
authRequired pageUser =
    case pageUser of
        ( HomePage, Guest ) ->
            False

        ( LoginPage, _ ) ->
            False

        _ ->
            False


authRedirect : Page -> User -> ( Page, Cmd Msg )
authRedirect page user =
    if authRequired ( page, user ) then
        ( LoginPage, Navigation.modifyUrl <| pageToUrl LoginPage )
    else
        ( page, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        ChangePage page ->
            let
                ( newPage, cmd ) =
                    authRedirect page m.user
            in
                ( { m | page = newPage, showDrawer = Nothing }, cmd )

        Navigate page ->
            ( m, Navigation.newUrl <| pageToUrl page )

        LoginMsg msg ->
            let
                ( loginModel, cmd, user ) =
                    Login.update msg m.login

                newUser =
                    Maybe.withDefault m.user user
            in
                ( { m | login = loginModel, user = newUser }, Cmd.map LoginMsg cmd )

        StoriesResponse s ->
            { m | stories = s } ! []

        SetTableState t ->
            { m | tableState = t } ! []

        StoryFilterInput f ->
            { m | storyFilter = f } ! []

        ToggleDrawer d ->
            if m.showDrawer == Just d then
                { m | showDrawer = Nothing } ! []
            else
                { m | showDrawer = Just d } ! []

        FormMsg formMsg ->
            { m | answersForm = Form.update answerFormValidation formMsg m.answersForm } ! []


clarifyMethods : List String
clarifyMethods =
    [ "ReadAround", "BreakDown", "Substitution" ]


answerFormValidation : Validation CustomError Answers
answerFormValidation =
    let
        nonEmptyString =
            string |> andThen nonEmpty

        validateClarifyMethod =
            customValidation
                string
                (\s ->
                    case s of
                        "ReadAround" ->
                            Ok ReadAround

                        "BreakDown" ->
                            Ok BreakDown

                        "Substitution" ->
                            Ok Substitution

                        _ ->
                            Err (customError InvalidClarifyMethod)
                )
    in
        Validate.map5 Answers
            (field "connect" nonEmptyString)
            (field "question" nonEmptyString)
            (field "summarise" nonEmptyString)
            (field "clarify" nonEmptyString)
            (field "clarifyMethod" validateClarifyMethod)


subscriptions : Model -> Sub Msg
subscriptions m =
    let
        loginSubs =
            Login.subscriptions m.login
    in
        Sub.batch
            [ Sub.map LoginMsg loginSubs
            ]


view : Model -> Html Msg
view m =
    let
        pageContent =
            case m.page of
                HomePage ->
                    div []
                        [ dashBoard m
                        , storyTiles m
                        ]

                LoginPage ->
                    Html.map LoginMsg (Login.view m.login)

                FindStoryPage ->
                    div []
                        [ dashBoard m
                        , viewStories m
                        ]

                StoryPage id_ ->
                    div []
                        [ dashBoard m
                        , viewStory m id_
                        , div [ id "activities", class "section" ]
                            [ sectionHeading [ h2 [] [ text "Answers" ] ]
                            , (Html.map FormMsg (answersFormView m.answersForm))
                            ]
                        , drawer m
                        ]

                _ ->
                    div []
                        [ dashBoard m
                        , text "Haven't implemented this page yet"
                        , text (toString m)
                        ]
    in
        div [ id "root" ]
            [ pageContent ]


answersFormView : Form CustomError Answers -> Html Form.Msg
answersFormView form =
    let
        answerField nm lbl =
            Form.getFieldAsString nm form
                |> \fld ->
                    div [ class (errorClass fld.liveError)]
                        [ label [ for (nm ++ "Input") ] [ text lbl ]
                        , Input.textArea fld [ class "form-control", id (nm ++ "Input")]
                        ]

        clarifyMethodOptions =
            ( "", "Which clarify method worked best for you?" ) :: List.map (\s -> ( s, s )) clarifyMethods

        errorClass maybeError =
            Maybe.map (\_ -> "has-error") maybeError |> Maybe.withDefault ""
    in
        Html.form [ ]
            [ div [ class "form-group" ]
                [ answerField "connect" "Connect this story with yourself or something you know about."
                , answerField "question" "Think of a question the story makes you want to ask and type it here."
                , answerField "summarise" "Write one sentence that captures the main idea."
                , answerField "clarify" "Work through the clarify methods, then type what you think the word means."
                , div []
                    [ label [] []
                    , Input.selectInput clarifyMethodOptions (Form.getFieldAsString "clarifyMethod" form) [ class "form-control" ]
                    ]
                , button [ class "btn btn-primary", type_ "submit", onClick Form.Submit ] [ text "Submit your answers" ]
                ]
            ]


drawerButtons : Html Msg
drawerButtons =
    div [ class "btn-group" ]
        [ button [ class "connectbutton", onClick (ToggleDrawer Connect) ] []
        , button [ class "questionbutton", onClick (ToggleDrawer Question) ] []
        , button [ class "summarisebutton", onClick (ToggleDrawer Summarise) ] []
        , button [ class "clarifybutton", onClick (ToggleDrawer Clarify) ] []
        ]

drawerButtons2 : Html Msg
drawerButtons2 =
    div [ class "btn-group" ]
        [ button [ class "connectbutton", onClick (ToggleDrawer Connect) ] []
        , button [ class "questionbutton", onClick (ToggleDrawer Question) ] []
        , button [ class "summarisebutton", onClick (ToggleDrawer Summarise) ] []
        , button [ class "clarifybutton", onClick (ToggleDrawer Clarify) ] []
        ]


drawer : Model -> Html Msg
drawer m =
    let
        currentDrawer =
            Maybe.withDefault Connect m.showDrawer

        listItem s =
            li [] [ text s ]

        mkList is =
            List.map listItem is
                |> ul []

        drawerHeader =
            div [ class "panelheader" ]
                [ img [ src ("img/" ++ hdrImage), width 25 ] []
                , h1 []
                    [ text (toString currentDrawer)
                    ]
                , Html.a [ class "closebutton", onClick (ToggleDrawer currentDrawer) ]
                    [ img [ src "img/closeblack.png" ] [] ]
                ]

        ( content, hdrImage, panelStyle ) =
            case currentDrawer of
                Connect ->
                    (,,)
                        [ mkList
                            [ "Do I know something about this already?"
                            , "Has something like this ever happened to me?"
                            , "Have I read about something like this?"
                            , "What does this remind me of in the real world?"
                            ]
                        ]
                        "connectblack.png"
                        "connectpanel"

                Summarise ->
                    (,,)
                        [ p [] [ text "We want one sentence on what this story is all about." ]
                        , p [] [ text "It doesn't have to be your own words. If there's a sentence in the story that does the job, copy and paste it. Here's what to do if there isn't:" ]
                        , mkList
                            [ "Skim the story fast, looking for good words or phrases."
                            , "Write them down."
                            , "Make a sentence by putting the words together with words of your own."
                            , "Read your sentence aloud. If it doesn't make sense, change it to make it better."
                            , "Take a last look at the story to see if you've missed any important point."
                            ]
                        ]
                        "summariseblack.png"
                        "summarisepanel"

                Question ->
                    (,,)
                        [ p [] [ text "Here are a few questions you could ask when you're reading. Feel free to think up more of your own." ]
                        , mkList
                            [ "What does that sentence mean?"
                            , "Does this part make sense to me?"
                            , "How does the writer know that?"
                            , "Is that fact or opinion?"
                            , "How did they do that?"
                            , "Why did they do that?"
                            , "What if they had done it this way instead?"
                            , "What question is this person trying to answer?"
                            , "What happens next?"
                            ]
                        ]
                        "questionblack.png"
                        "questionpanel"

                Clarify ->
                    (,,)
                        [ p [] [ text "Try to figure out what the word means using these methods:" ]
                        , mkList
                            [ "Read a line or two around the word, looking for clues."
                            , "Look for parts of words or whole words in the unknown word."
                            , "Imagine the word isn&#39;t there and try another word or words in its place."
                            ]
                        ]
                        "clarifyblack.png"
                        "clarifypanel"
    in
        div []
            [ input
                [ type_ "checkbox"
                , id "toggle-drawer"
                , onCheck (\_ -> ToggleDrawer currentDrawer)
                , checked (m.showDrawer /= Nothing)
                ]
                []
            , div [ id "drawer", class panelStyle ]
                [ drawerHeader
                , div [ id "drawercontent" ] content
                ]
            ]


mapStories : (List Story -> Html Msg) -> WebData (List Story) -> Html Msg
mapStories f stories =
    case stories of
        RemoteData.NotAsked ->
            text "Unexpected state (no stories asked for)"

        RemoteData.Loading ->
            text "Loading stories ..."

        RemoteData.Failure err ->
            text ("Error loading stories: " ++ toString err)

        RemoteData.Success stories ->
            f stories


storyTiles : Model -> Html Msg
storyTiles m =
    let
        stories_ =
            mapStories (mkTiles << List.take 18) m.stories

        mkTiles stories =
            div [ class "storytiles" ] (List.map storyTile stories)

        storyStyle s =
            style [ ( "background", "url(pix/" ++ s.img ++ ")" ), ( "background-size", "cover" ) ]

        storyTile s =
            Html.a [ class "storytile", storyStyle s, Html.Attributes.href (pageToUrl (StoryPage s.id)) ] [ h3 [] [ text s.title ] ]
    in
        div [ id "stories", class "section" ]
            [ sectionHeading [ h1 [] [ text "Starter Stories" ] ]
            , stories_
            ]


sectionHeading : List (Html Msg) -> Html Msg
sectionHeading =
    div [ class "sectionheading" ]


viewStories : Model -> Html Msg
viewStories m =
    let
        tag i s =
            s.tags
                |> List.drop (i - 1)
                |> List.head
                |> Maybe.withDefault ""

        cfg =
            Table.config
                { toId = .id
                , toMsg = SetTableState
                , columns =
                    [ Table.stringColumn "Title" .title
                    , Table.stringColumn "Tag1" (tag 1)
                    , Table.stringColumn "Tag2" (tag 2)
                    , Table.stringColumn "Tag3" (tag 3)
                    , Table.intColumn "Level" .level
                    , storyLinkColumn
                    ]
                }

        storyLinkColumn : Table.Column Story Msg
        storyLinkColumn =
            Table.veryCustomColumn
                { name = ""
                , viewData = viewStoryLink
                , sorter = Table.unsortable
                }

        viewStoryLink : Story -> Table.HtmlDetails Msg
        viewStoryLink s =
            Table.HtmlDetails []
                [ button [ onClick (Navigate (StoryPage s.id)) ] [ text "View" ]
                ]
    in
        div [ id "stories", class "section" ]
            [ sectionHeading
                [ h1 [] [ text "Stories" ]
                  --, drawerButtons
                ]
            , div [ id "storyfilter" ]
                [ label [] [ text "Search" ]
                , input
                    [ type_ "text"
                    , value m.storyFilter
                    , onInput StoryFilterInput
                    ]
                    []
                ]
            , mapStories (Table.view cfg m.tableState << filterStories m.storyFilter) m.stories
            ]


filterStories : String -> List Story -> List Story
filterStories storyFilter stories =
    if String.length storyFilter < 3 then
        stories
    else
        let
            r =
                Regex.caseInsensitive (Regex.regex storyFilter)

            match story =
                Regex.contains r story.title || Regex.contains r story.content
        in
            List.filter match stories


viewStory : Model -> String -> Html Msg
viewStory m id_ =
    case m.stories of
        RemoteData.Success stories ->
            case List.filter (\s -> s.id == id_) stories of
                s :: _ ->
                    div [ class "section" ]
                        [ sectionHeading
                            [ h1 [] [ text s.title ]
                            , drawerButtons
                            ]
                        , div
                            [ id "storycontainer" ]
                            [ div [ id "storypic" ]
                                [ img [ src ("pix/" ++ s.img) ] []
                                ]
                            , div [ id "storycontent" ]
                                [ Markdown.toHtml [] s.content
                                ]
                            , div [ id "storyfooter" ]
                                [ p [] [ text (String.join ", " s.tags), br [] [], text ("Level: " ++ toString s.level) ]
                                ]
                            ]
                        ]

                _ ->
                    text "Story not found"

        _ ->
            text "Stories have not been loaded"


dashBoard : Model -> Html Msg
dashBoard m =
    div [ id "dashboard", class "section" ]
        [ h1 [] [ text "Dashboard" ]
        , div [ id "innerdash" ]
            [ nav m
            ]
        ]


nav : Model -> Html Msg
nav m =
    let
        btn page txt =
            button
                [ onClick (Navigate page)
                , disabled (page == m.page)
                ]
                [ text txt ]
    in
        div [ id "nav" ]
            [ btn HomePage "Home"
            , btn FindStoryPage "Find a story"
            , btn AccountPage "My 3ML"
            , btn LeaderBoardPage "Leader board"
            , btn TrailsPage "Trails"
            ]


main : Program Never Model Msg
main =
    Navigation.program (locationToPage >> ChangePage)
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
