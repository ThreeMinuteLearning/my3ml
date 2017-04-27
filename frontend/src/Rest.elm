module Rest exposing (handleRemoteData, getStories, getDictionary, getSchoolClasses, getSchoolStudents, createStudentAccounts, createClass, submitAnswers, getAnswers)

import AnswersForm exposing (Answers)
import Api
import Html exposing (Html, text)
import Http exposing (Request)
import RemoteData exposing (WebData)
import Types exposing (AccessToken(..), AppMode(..), Msg(..), SchoolDataMsg(..), StoriesMsg(..))


handleRemoteData : (a -> Html Msg) -> WebData a -> Html Msg
handleRemoteData f rd =
    case rd of
        RemoteData.NotAsked ->
            text "Data not loaded."

        RemoteData.Loading ->
            text "Loading ..."

        RemoteData.Failure err ->
            text ("Error loading data: " ++ toString err)

        RemoteData.Success d ->
            f d


sendRequest : Request a -> (WebData a -> Msg) -> Cmd Msg
sendRequest r m =
    Cmd.map m (RemoteData.sendRequest r)


getStories : AccessToken -> Cmd Msg
getStories (AccessToken t) =
    sendRequest (Api.getStories t) (StoriesMsg << StoriesResponse)


getSchoolClasses : AccessToken -> Cmd Msg
getSchoolClasses (AccessToken t) =
    sendRequest (Api.getSchoolClasses t) (SchoolDataMsg << ClassesResponse)


getSchoolStudents : AccessToken -> Cmd Msg
getSchoolStudents (AccessToken t) =
    sendRequest (Api.getSchoolStudents t) (SchoolDataMsg << StudentsResponse)


getDictionary : Cmd Msg
getDictionary =
    sendRequest Api.getDictionary (StoriesMsg << DictResponse)


createStudentAccounts : AccessToken -> List String -> Cmd Msg
createStudentAccounts (AccessToken t) studentNames =
    sendRequest (Api.postSchoolStudents t studentNames) (SchoolDataMsg << AddStudentsResponse)


createClass : AccessToken -> ( String, String ) -> Cmd Msg
createClass (AccessToken t) classInfo =
    sendRequest (Api.postSchoolClasses t classInfo) (SchoolDataMsg << AddClassResponse)


getAnswers : AccessToken -> Cmd Msg
getAnswers (AccessToken t) =
    sendRequest (Api.getSchoolAnswers t) (StoriesMsg << GetAnswersResponse)


submitAnswers : AccessToken -> Api.Story -> Answers -> Cmd Msg
submitAnswers (AccessToken t) s ans =
    let
        answer =
            Api.Answer "" s.id "" ans.connectAnswer ans.questionAnswer ans.summary ans.clarification
    in
        sendRequest (Api.postSchoolAnswers t answer) (StoriesMsg << SubmitAnswersResponse)
