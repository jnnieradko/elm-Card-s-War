-- Elm 0.19.0
module Main exposing (..)
import Browser
import Html exposing (..)
import List
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Debug exposing (..)


main =
  Browser.sandbox { init = init, view = view, update = update }

type CardColor = Spades | Hearts | Clubs | Diamonds
type Figures = King | Queen | Jack | As
type Card = FaceCard | Numeral

type Model =
  Model String (List (String , Int)) (List String) (List {color : CardColor, cardType: Card })

init =
  Model
  "hello, world!"
  [("hej",2),("tak",3),("nie",4),("OK",5)]
  ["Spades", "Hearts", "Clubs", "Diamonds"]
  [{color = Spades, cardType = FaceCard}, {color = Hearts, cardType = FaceCard}, {color = Clubs, cardType = Numeral}, {color = Diamonds, cardType = Numeral}]


type Msg 
  = NoOp | Dodaj

update : Msg -> Model -> Model
update msg (Model m l a r) =
  case msg of
    NoOp ->
      (Model m l a r)
    Dodaj -> (Model m (l ++ [(" aa",66)]) (a ++ [" Karta"]) r )


view : Model -> Html Msg
view (Model m l a r) =
    div []
    [ div [ style "background-color" "red"
              , style "height" "30px"
              , style "width" "50%"
              ] [ text m , text "aaa" ]
    , div [onClick Dodaj] [ text "Cześć"]
    , tabelka l
    , karty a
    ]

karty : List String -> Html a
karty a  =
    let listOfColors = List.map (\x -> div [] [text x] ) a
    in ul [style "background-color" "lightblue"] listOfColors

tabelka : List (String , Int) -> Html a
tabelka l =
    let w = List.map (\(a,b) -> div [] [text a , text (String.fromInt b) ] ) l
    in div [style "background-color" "green"] w

listaKart : List(String , Int) -> Html a
listaKart = todo ""

