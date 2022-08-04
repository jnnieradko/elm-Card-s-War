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

main = Browser.sandbox { init = init2, view = view2, update = update2 }

type Model2 = Model2 {listaKart : List Card, listaGraczy : List ModelGracza, graczDoDodania : String}

init2 =
    Model2 {listaKart = deckOfCards, listaGraczy = listOfPlayers2, graczDoDodania = "wpisz imie"}

type Msg
  = NoOp | Dodaj Card | Usun Card | Reset | Wybierz Card | NowyPlayer | UpdateNewPlayerName String | UsunGracza String

update2 : Msg -> Model2 -> Model2
update2 msg (Model2 m ) =
  case msg of
    NoOp -> (Model2 m)
    Dodaj c -> Model2 { m | listaKart = m.listaKart ++ [ c ] }
    Usun c -> Model2 { m | listaKart = wyrzucKarte c m.listaKart }
    Reset -> Model2 { m | listaKart = deckOfCards }
    Wybierz c -> Model2 { m | listaKart = [c] }
    NowyPlayer -> Model2 { m | listaGraczy = m.listaGraczy ++ [ModelGracza { nazwaGracza = m.graczDoDodania, kartyWReku = [] }]
                             , graczDoDodania = ""}
    UpdateNewPlayerName s -> Model2 { m | graczDoDodania = s }
    UsunGracza imgr -> Model2 { m | listaGraczy = deletePlayer imgr m.listaGraczy }


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


type ModelGracza = ModelGracza {nazwaGracza : String, kartyWReku : List Card }

listOfPlayers2 : List ModelGracza
listOfPlayers2 = [ ModelGracza { nazwaGracza = "RoRo", kartyWReku = [ FaceCard King Spades,FaceCard Queen Hearts, Numeral 4 Diamonds] }
                    , ModelGracza { nazwaGracza = "Jan", kartyWReku = [FaceCard Jack Clubs, Numeral 9 Spades, Numeral 10 Hearts] }
                    , ModelGracza { nazwaGracza = "Stanislaw", kartyWReku = [FaceCard Ace Clubs, Numeral 5 Clubs, Numeral 6 Diamonds] } ]


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
cardsOfColorListHtml lc = div [] (List.map(\x -> cardsOfColorHtml x) lc )

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


gracze : List ModelGracza -> Html Msg
gracze lmg = ul [] ( (List.map (\(ModelGracza x ) -> ( li [] [text x.nazwaGracza, button [onClick (UsunGracza x.nazwaGracza) ] [text "X"] , div [] [cardsOfColorListHtml x.kartyWReku ] ])) lmg)
      ++  [ input [ onInput (\x -> UpdateNewPlayerName x ) ] []
      , button [onClick (NowyPlayer) ] [text "Dodaj"] ] )

deletePlayer : String -> List ModelGracza -> List ModelGracza
deletePlayer s lmg=  List.filter (\(ModelGracza nazwagr) -> not ( s == nazwagr.nazwaGracza )) lmg

{-kartyGracza : List ModelGracza -> List (List Card)
kartyGracza lmg = List.map (\(ModelGracza kargr) -> kargr.kartyWReku) lmg-}




