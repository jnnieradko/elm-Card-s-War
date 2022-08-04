-- Elm 0.19.0
module Main exposing (..)
import Browser
import Html exposing (..)
import List
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Debug exposing (..)

type CardColor = Spades | Hearts | Clubs | Diamonds

myColor : CardColor
myColor = Spades

polishName : CardColor -> String
polishName x =
    case x of
      Spades -> "Pik"
      Hearts -> "Kier"
      Clubs -> "Trefl"
      Diamonds -> "Karo"

type Fig = King | Queen | As | Jack

type Card = Figure CardColor Fig  | Regular Int CardColor

type CardNameResult = ValidName String | InvalidCard

cardColor : Card -> CardColor
cardColor = \x ->
    case x of
        Figure y _ -> y
        Regular _ y -> y


cardName : Card -> CardNameResult
cardName = \x ->
    case x of
        Figure y z ->
            let cN = polishName y
            in ValidName (case z of
                    King ->  ("Król " ++ cN)
                    Queen -> ("Królowa " ++ cN)
                    As -> ("As " ++ cN)
                    Jack -> ("Walet " ++ cN))
        Regular y z ->
            let cN = polishName z
            in case y of
                2 -> ValidName ("dwójka " ++ cN)
                3 -> ValidName ("trójka " ++ cN)
                4 -> ValidName ("czwórka " ++ cN)
                5 -> ValidName ("piątka " ++ cN)
                6 -> ValidName ("szóstka " ++ cN)
                7 -> ValidName ("siódemka " ++ cN)
                8 -> ValidName ("ósemka " ++ cN)
                9 -> ValidName ("dziewiątka " ++ cN)
                10 -> ValidName ("dziesiątka " ++ cN)
                _ -> InvalidCard


main =
  Browser.sandbox { init = init, view = view, update = update }



type Model =
  Model String (List (String , Int)) (List Card)

init =
  Model
  "hello, world!"
  [("hej",2),("tak",3),("nie",4),("OK",5)]
  [(Figure Spades King), (Figure Hearts King) , (Regular 2 Diamonds) , (Regular 4 Clubs)]



type Msg 
  = NoOp | Dodaj

update : Msg -> Model -> Model
update msg (Model m l a ) =
  case msg of
    NoOp ->
      (Model m l a )
    Dodaj -> (Model m (l ++ [(" aa",66)]) (a) )


view : Model -> Html Msg
view (Model m l a ) =
    div []
    [ div [ style "background-color" "red"
              , style "height" "30px"
              , style "width" "50%"
              ] [ text m , text "aaa" ]
    , div [onClick Dodaj] [ text "Cześć"]
    , tabelka l
    ,karty a
    ]

karty : List Card -> Html a
karty a  =
    let listOfCards = List.map (\x ->
                           case x of
                               Figure Spades King -> div [] [text "OK"]
                               _ -> div [] [text "NOT OK"] ) a
    in ul [style "background-color" "lightblue"] listOfCards


tabelka : List (String , Int) -> Html a
tabelka l =
    let w = List.map (\(a,b) -> div [] [text a , text (String.fromInt b) ] ) l
    in div [style "background-color" "green"] w

listaKart : List(String , Int) -> Html a
listaKart = todo ""

