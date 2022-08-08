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
import List.Extra exposing (..)

main = Browser.sandbox { init = init2, view = view2, update = update2 }

type Model2 = Model2 {listaKart : List Card, listaGraczy : List ModelGracza, graczDoDodania : String , kartyWGrze : List Card}

init2 =
    Model2 {listaKart = deckOfCards, listaGraczy = listOfPlayers2, graczDoDodania = "", kartyWGrze = []}

type Msg
  = NoOp | Dodaj Card | Usun Card | Reset | Wybierz Card | NowyPlayer | UpdateNewPlayerName String | UsunGracza String | WyrzucKartyNaStol

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
    UsunGracza s -> Model2 { m | listaGraczy = deletePlayer s m.listaGraczy }
    WyrzucKartyNaStol -> Model2 { m | kartyWGrze = kartyNaStol m.listaGraczy
                                        --, listaGraczy = usunWyrzucone (m.listaGraczy)



                                      }



view2 : Model2 -> Html Msg
view2 (Model2 m) =
     div []
     [ div [style "height" "400px"
            ]
        [div [style "display" "inline-block"
              , style "width" "40%"
              , style "height" "300px"
              , style "vertical-align" "middle"
              , style "margin" "10px"
              ]
         [ text "Wszyscy gracze", (gracze m.listaGraczy)]
        ,div [style "display" "inline-block"
            , style "vertical-align" "middle"
            , style "width" "40%"
            , style "height" "300px"
            , style "margin" "10px"
            , style "background-color" "lightgreen"
            ] [ div [] [text "Stół"]
                , div [] [text " Lista wyrzuconych kart"]
                , div [] [ kartyNaStolListHtml m.kartyWGrze]
                , div [] []
            ]
        ]
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
     ,div [] [ listaKart m.listaKart ]
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
gracze lmg = ul [style "margin-top" "10px"] ( (List.map (\(ModelGracza x ) -> ( li [] [text x.nazwaGracza, button [onClick (UsunGracza x.nazwaGracza) ] [text "X"] , div [] [cardsOfColorListHtml x.kartyWReku ] ])) lmg)
      ++  [ input [ onInput (\x -> UpdateNewPlayerName x )
                  , style "display" "inline-block"] []
           , button [onClick (NowyPlayer)
                    , style "display" "inline-block"] [text "Dodaj"] ]
      ++ [ button [onClick WyrzucKartyNaStol ] [text "WYRZUC KARTY"]] )

deletePlayer : String -> List ModelGracza -> List ModelGracza
deletePlayer s lmg=  List.filter (\(ModelGracza x ) -> not ( s == x.nazwaGracza )) lmg




kartyNaStol : List ModelGracza -> List Card
kartyNaStol lmgr = List.concat (List.map (\(ModelGracza x) -> (take 1 x.kartyWReku) )  lmgr)

kartyNaStolListHtml : List Card -> Html Msg
kartyNaStolListHtml lc = div [] (List.map(\x -> cardsOfColorHtml x) lc )



lista : List Card
lista = [FaceCard King Spades, FaceCard Jack Clubs,FaceCard Ace Clubs]


--usunWyrzucone : List ModelGracza -> List ModelGracza
--usunWyrzucone lmg=  List.map(\y ->  List.filter (\(ModelGracza x) -> not (y == x.kartyWReku) )) lc


