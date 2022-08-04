module Zadanie exposing (..)

type Parz = Dwa | Cztery
type Nieparz = Jeden | Trzy

type MniejNiz0 = MinusJeden | MinusDwa | MinusTrzy
type WiecejNiz0 = Nieparzyste Nieparz | Parzyste Parz
type MojInt = Ujemne MniejNiz0 | Dodatnie WiecejNiz0 | Zero

dajInt : MojInt -> Int
dajInt mi = case mi of
    Zero -> 0
    Ujemne MinusJeden -> -1
    Ujemne MinusDwa -> -2
    Ujemne MinusTrzy -> -3
    Dodatnie (Nieparzyste Jeden) -> 1
    Dodatnie (Parzyste Dwa) -> 2
    Dodatnie (Nieparzyste Trzy) -> 3
    Dodatnie (Parzyste Cztery) -> 4


--main = Browser.sandbox { init = init, view = view, update = update }


{-type Model =
  Model (List Card) (List String) String

init =
  Model
  deckOfCards
  listOfPlayers
  ""-}

{-update : Msg -> Model -> Model
update msg (Model lc ls s ) =
  case msg of
    NoOp ->
      (Model lc ls s)
    Dodaj c -> Model (lc ++ [ c ]) ls s
    Usun c -> Model ( wyrzucKarte c lc) ls s
    Reset  -> Model (deckOfCards) ls s
    Wybierz c -> Model [ c ] ls s
    NowyPlayer -> (Model lc (ls ++ [ s ]) "")
    UpdateNewPlayerName ss -> (Model lc ls ss)
    UsunGracza imiegracza -> Model lc (deletePlayer imiegracza ls) ""-}


{-view : Model -> Html Msg
view (Model lc ls s) =
    div []
    [ div [] [ h1 [] [ text "Wszyscy gracze"], text "Gracze" , (gracze ls) ]
    ,div [] [panelWyboruKarty ]
    ,ul [ style "background-color" "lightblue"
                    , style "height" "30px"
                    , style "width" "50%"
                    ] [text "aaa" ]
    ,div [ style "background-color" "red"
              , style "height" "30px"
              , style "width" "50%"
              ] [text "aaa" ]
    ,button [onClick Reset] [ text "Reset"]
    ,div [] [ listaKart lc]
    ]-}
