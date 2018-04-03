module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Mouse exposing (Position)
import Collage exposing (..)
import Element exposing (toHtml)
import Color exposing (..)



main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { mouseDown : Bool
    , pencilPath : List (List Position)
    --I want to add layers
    --Delete Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model False [], Cmd.none )

-- UPDATE


type Msg --This is following the drag model in elm-lang offical examples
    = DragStart Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
      DragStart xy ->
          ({mouseDown = True , pencilPath = [xy]::model.pencilPath}, Cmd.none)
      DragAt xy ->
            case model.pencilPath of
              x :: xs ->
                ({mouseDown = True , pencilPath = (x++[xy])::xs}, Cmd.none)
              _ ->
                ({mouseDown = True ,pencilPath = [[xy]]}, Cmd.none)
      DragEnd _ ->
          ({mouseDown = False, pencilPath = model.pencilPath}, Cmd.none)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.mouseDown of
        False ->
            Mouse.downs DragStart
        True ->
            Sub.batch[ Mouse.moves DragAt, Mouse.ups DragEnd]



-- VIEW


view : Model -> Html Msg
view model =
    let
        toPaths positions =
            path (List.map (\{ x, y } ->( toFloat x, toFloat -y)) positions)
                |> traced (defaultLine)
                |> move ( -750, 750 )
    in
        div []
            [ toHtml(collage 1500 1500 (List.map toPaths model.pencilPath))]
