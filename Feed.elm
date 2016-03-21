import StartApp
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects exposing (Effects)
import Signal
import Time exposing (Time)
import Date exposing (Date)
import Json.Decode as Json
import Task exposing (Task)
import Date.Format

-- Model


type alias Message =
  { text: String
  , createdBy: String
  , createdAt: Date
  }


type alias User =
  { name: String
  }


type Session = Guest | Authenticated User


type alias Model =
  { messages: List Message
  , session: Session
  , currentTime: Time
  , messageField: String
  , loginField: String
  }


init : ( Model, Effects a )
init =
  let initialModel =
    { messages = []
    , session = Guest
    , currentTime = 0.0
    , messageField = ""
    , loginField = ""
    }
  in
    (initialModel, Effects.none)


currentUserName : Session -> String
currentUserName session =
  case session of
    Guest ->
      "anonymous"

    Authenticated user ->
      user.name


newMessage : Model -> Message
newMessage model =
  { text = model.messageField
  , createdBy = currentUserName model.session
  , createdAt = Date.fromTime model.currentTime
  }


publishMessage : Model -> List Message
publishMessage model =
  (newMessage model) :: model.messages


-- Update


type Action
  = LogIn
  | LogOut
  | PublishMessage
  | TimerTick Time
  | UpdateMessageField String
  | UpdateLoginField String


updateModel : Action -> Model -> Model
updateModel action model =
  case action of
    LogIn ->
      { model |
        session = Authenticated { name = model.loginField }
      , loginField = ""
      }

    LogOut ->
      { model |
        session = Guest
      , loginField = ""
      }

    PublishMessage ->
      { model |
        messages = publishMessage model
      , messageField = ""
      }

    TimerTick time ->
      { model | currentTime = time }

    UpdateMessageField text ->
      { model | messageField = text }

    UpdateLoginField text ->
      { model | loginField = text }


update : Action -> Model -> ( Model, Effects Action )
update action model =
  (updateModel action model, Effects.none)


clock : Signal Action
clock =
  Signal.map TimerTick (Time.every Time.second)


-- View


onEnter : Signal.Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address value)


is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"

renderInputMessage : Signal.Address Action -> String -> Html
renderInputMessage address messageField =
  div []
    [ input
        [ placeholder "Enter a message ..."
        , autofocus True
        , value messageField
        , on "input" targetValue (Signal.message address << UpdateMessageField)
        , onEnter address PublishMessage
        ]
        []
    , button
        [ onClick address PublishMessage
        ]
        [ text "Send" ]
    ]


createdAt : Date -> String
createdAt date =
  Date.Format.format "%A, %B %d, %Y %I:%M:%S %p" date


formatTime : Time -> String
formatTime time =
  if time == 0.0 then
    "--:--:-- --"
  else
    Date.fromTime time
      |> Date.Format.format "%I:%M:%S %p"


renderMessage : Message -> Html
renderMessage message =
  li [] [ text (message.text ++ " by @" ++ message.createdBy ++ " at " ++ (createdAt message.createdAt)) ]


renderMessages : List Message -> Html
renderMessages messages =
  if List.isEmpty messages then
    text "There are no messages :-("
  else
    ul [] (List.map renderMessage messages)


renderAuthenticatedUser : Signal.Address Action -> User -> Html
renderAuthenticatedUser address user =
  div []
    [ text user.name
    , button
        [ onClick address LogOut
        ]
        [ text "LogOut" ]
    ]


renderGuestUser : Signal.Address Action -> Html
renderGuestUser address =
  div []
    [ input
        [ placeholder "Enter your username"
        , on "input" targetValue (Signal.message address << UpdateLoginField)
        , onEnter address LogIn
        ]
        []
    , button
        [ onClick address LogIn
        ]
        [ text "LogIn" ]
    ]


renderSession : Signal.Address Action -> Session -> Html
renderSession address session =
  case session of
    Guest ->
      renderGuestUser address

    Authenticated user ->
      renderAuthenticatedUser address user


view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ renderMessages model.messages
    , renderInputMessage address model.messageField
    , renderSession address model.session
    , text (formatTime model.currentTime)
    ]


-- Main

app =
  StartApp.start { init = init, update = update, view = view, inputs = [clock] }


main =
  app.html


port tasks : Signal (Task Effects.Never ())
port tasks =
    app.tasks
