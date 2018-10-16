import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD exposing (field, Decoder, list, string)
import Url.Builder as Url
import String
import Task

-- MAIN

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

-- MODEL

type alias Model = 
    { breed : String
    , imgUrl : String
    , breeds : List String }

init : () -> (Model, Cmd Msg)
init _ = 
    ( Model "corgi" "" ["corgi", "terrier", "retriever"]
    , getImage "corgi"
    )

-- UPDATE

type Msg
    = NewImage
    | ShowImage (Result Http.Error String)
    | UpdateBreed String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewImage ->
            ( model
            , getImage model.breed
            )
        
        ShowImage imgurl ->
            case imgurl of
                Ok url ->
                    ( { model | imgUrl = url }
                    , Cmd.none
                    )
                Err _ ->
                    ( model
                    , Cmd.none
                    )

        UpdateBreed breed ->
            ( { model | breed = breed } 
            , getImage breed
            )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW

view : Model -> Html Msg
view model = 
    div [] [
        h2 [] [ text "Pictures of dogs" ]
        , img [ src model.imgUrl ] []
        , div[] [ button [ onClick NewImage ] [ text "New Image" ] ]
        , select [ onInput UpdateBreed ] 
            (List.map (\breed -> option [] [ text breed ]) model.breeds)
    ]

-- HTTP

imageDecoder : Decoder String
imageDecoder = 
    JD.field "message" JD.string

buildImageURI breed =
    Url.crossOrigin "https://cors-anywhere.herokuapp.com/https://dog.ceo" ["api", "breed", breed, "images", "random"] []

getImage : String -> Cmd Msg
getImage breed =
    Http.send ShowImage (Http.get (buildImageURI breed) imageDecoder)
    