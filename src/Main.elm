-- Elm 0.19.0
module Main exposing (..)
import Browser
import Html exposing (..)
import List exposing (..)
import Html.Events exposing (onClick , onInput)
import Html.Attributes exposing (..)
import Debug exposing (..)
import Cards exposing (..)
import String exposing (..)
import Players exposing (..)

--main = Browser.sandbox { init = init, view = view, update = update }

main = Browser.sandbox { init = init2, view = view2, update = update2 }


type Model2 = Model2 {listaKart : List Card, listaGraczy : List String, graczDoDodania : String}

--******************************

type Model =
  Model (List Card) (List String) String

init =
  Model
  deckOfCards
  listOfPlayers
  ""

init2 =
    Model2 {listaKart = deckOfCards, listaGraczy = listOfPlayers, graczDoDodania = "wpisz imie"}

type Msg
  = NoOp | Dodaj Card | Usun Card | Reset | Wybierz Card | NowyPlayer | UpdateNewPlayerName String | UsunGracza String

update : Msg -> Model -> Model
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
    UsunGracza imiegracza -> Model lc (deletePlayer imiegracza ls) ""

update2 : Msg -> Model2 -> Model2
update2 msg (Model2 m ) =
  case msg of
    NoOp -> (Model2 m)
    Dodaj c -> Model2 { m | listaKart = m.listaKart ++ [ c ] }
    Usun c -> Model2 { m | listaKart = wyrzucKarte c m.listaKart }
    Reset -> Model2 { m | listaKart = deckOfCards }
    Wybierz c -> Model2 { m | listaKart = [c] }
    NowyPlayer -> Model2 { m | listaGraczy = m.listaGraczy ++ [m.graczDoDodania]
                             , graczDoDodania = ""}
    UpdateNewPlayerName s -> Model2 { m | graczDoDodania = s }
    UsunGracza imgr -> Model2 { m | listaGraczy = deletePlayer imgr m.listaGraczy }


view : Model -> Html Msg
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
    ]

view2 : Model2 -> Html Msg
view2 (Model2 m) =
     div []
     [ div [] [ h1 [] [ text "Wszyscy gracze"], text "Gracze" , (gracze m.listaGraczy) ]
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
     ,div [] [ listaKart m.listaKart  ]
     ]



cardHtml = \x ->
    case (cardName x ) of
       ValidName n -> div [onClick (Usun x) , style "border" "1px solid black"
                                , style "margin" "3px"
                                ,style "background-color" "pink"
                                ] [text n ]
       InvalidCard -> div [style "border" "1px solid red"
                                , style "margin" "3px"] [text "bald!"]

listaKart : List Card -> Html Msg
listaKart lc = div [style "background-color" "yellow"] (List.map cardHtml lc)


cardsOfColorHtml : Card -> Html Msg
cardsOfColorHtml = \x ->
    case (cardName x ) of
       ValidName n -> div [onClick (Usun x)
                                , style "display" "inline-block"
                                ,style "border" "1px solid black"
                                , style "margin" "3px"
                                ] [text n ]
       InvalidCard -> div [style "border" "1px solid red"
                                , style "margin" "3px"] [text "bald!"]

cardsOfColorListHtml : List Card -> Html Msg
cardsOfColorListHtml lc = div [] (List.map cardsOfColorHtml lc )

panelWyboruKarty : Html Msg
panelWyboruKarty = div [] (List.map panelWyboruKolor allColors)

przyciskKarty : Card -> Html Msg
przyciskKarty x =
        case (cardName x ) of
           ValidName n -> div [onClick (Wybierz x) , style "border" "1px solid black"
                                    , style "margin" "3px"
                                    ,style "background-color" "pink"
                                    , style "display" "inline-block"
                                    ] [text n ]
           InvalidCard -> div [style "border" "1px solid red"
                                    , style "margin" "3px"] [text "bald!"]

panelWyboruKolor : CardColor -> Html Msg
panelWyboruKolor cc =
    div []
      [ h3 [] [text (polishName cc)]
      , div [] ( List.map przyciskKarty (allCardsOfColor cc) )
      ]


gracze : List String -> Html Msg
gracze ls = ul [] ( (List.map (\s -> ( li [] [text s, button [onClick (UsunGracza s) ] [text "X"]] ) ) ls )
      ++  [ input [ onInput (\x -> UpdateNewPlayerName x ) ] []
      , button [onClick (NowyPlayer) ] [text "Dodaj"] ] )

deletePlayer : String -> List String -> List String
deletePlayer s ls=  List.filter (\x -> not ( s == x )) ls






