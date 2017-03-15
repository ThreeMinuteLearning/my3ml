module Rest exposing (getStories, getDictionary)

import Api
import Http exposing (Request)
import RemoteData exposing (WebData)
import Types exposing (AccessToken(..), AppMode(..), Msg(..), StoriesMsg(..))


sendRequest : Request a -> (WebData a -> Msg) -> Cmd Msg
sendRequest r m =
    Cmd.map m (RemoteData.sendRequest r)


getStories : AccessToken -> Cmd Msg
getStories (AccessToken t) =
    sendRequest (Api.getStories t) (StoriesMsg << StoriesResponse)


getDictionary : Cmd Msg
getDictionary =
    sendRequest Api.getDictionary (StoriesMsg << DictResponse)
