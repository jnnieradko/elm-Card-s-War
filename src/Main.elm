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

main = Browser.sandbox { init = initNew, view = viewNew, update = updateNew }

type  Kolej = KolejA | KolejB

type ModelNew = GraRozpoczecie { nazwaGraczaA : String , nazwaGraczaB : String  }
                | GraPrzebieg { nazwaGraczaA : String
                              , nazwaGraczaB : String
                              , rekaGraczA : List Card
                              , rekaGraczB : List Card
                              , kartyNaStole : List Card
                              , kolej : Kolej
                              }
                | GraZakonczenie { nazwaGraczaWygranego : String }

type MRozpoczecie = UpdateNameA String    --  z pola Input zapamietuje wpisany String i przekazuje do nazwy gracza A
                  | UpdateNameB String -- z pola Input zapamietuje wpisany String i przekazuje do nazwy gracza B
                  | StartGry     -- wysyła msg w celu zmiany modelu na GraPrzebieg

type MPrzebieg = ZagrajA (List Card)      -- rekaGraczA : List Card - Card , kartyNaStole : List Card ++ [Card] , kolej : KolejA
               | ZagrajB (List Card)     -- rekaGraczB : List Card - Card , kartyNaStole : List Card ++ [Card] , kolej : KolejB
               | ZbierzKartyA -- compareCardsWar zwróci Order GT , rekaGraczA : lc + kartyNaStole [lc]
               | ZbierzKartyB -- compareCardsWar zwróci Order LT , rekaGraczA : lc + kartyNaStole [lc]

type MZakonczenie = KontynuujGre        -- zmień model na GraPrzebieg z aktualnymi graczami
                  | RozpocznijNowaGre   -- zmień model na GraRozpoczecie

type Msg = NoOp
         | MsgRozpoczecie MRozpoczecie
         | MsgPrzebieg MPrzebieg
         | MsgZakonczenie MZakonczenie

initNew = GraRozpoczecie {nazwaGraczaA = "" , nazwaGraczaB = ""}

updateNew : Msg -> ModelNew -> ModelNew
updateNew msg m =
    case (msg , m) of
        (NoOp , _ ) -> m
        (MsgRozpoczecie mr , GraRozpoczecie gr ) ->
                   case mr of
                        UpdateNameA s -> GraRozpoczecie { gr | nazwaGraczaA = s}
                        UpdateNameB s -> GraRozpoczecie { gr | nazwaGraczaB = s}
                        StartGry -> GraPrzebieg { nazwaGraczaA = gr.nazwaGraczaA
                                                , nazwaGraczaB = gr.nazwaGraczaB
                                                , rekaGraczA = Tuple.first(rozdanieKart deckOfCards)
                                                , rekaGraczB = Tuple.second(rozdanieKart deckOfCards)
                                                , kartyNaStole = []
                                                , kolej = KolejA
                                                }
        (MsgPrzebieg mp , GraPrzebieg gp ) ->
                   case mp of
                        ZagrajA lc1 -> GraPrzebieg { gp | rekaGraczA = wyrzucKarte lc1 gp.rekaGraczA
                                                      , kartyNaStole = lc1
                                                      , kolej = KolejB }
                        ZagrajB lc2 -> GraPrzebieg { gp | rekaGraczB = wyrzucKarte lc2 gp.rekaGraczB
                                                      , kartyNaStole = gp.kartyNaStole ++ lc2
                                                      , kolej = KolejA } -- pomocnicza funkcja po Zbierz karty , funkcja która sprawdza czy jest koniec gry i czy listy kart da puste i wtedy zwraca GraZakonczenie albo gra Przebie (funkcja przyjmuje GraPrzebieg a zwraca model)
                        ZbierzKartyA -> GraPrzebieg { gp | rekaGraczA = gp.rekaGraczA ++ gp.kartyNaStole}
                        ZbierzKartyB -> GraPrzebieg { gp | rekaGraczB = gp.rekaGraczB ++ gp.kartyNaStole}
                        -- co jak którys z graczy nie bedzie miał kart  rekaGraczaA = [] ??
        (MsgZakonczenie mz , GraZakonczenie gz) ->
                   case mz of
                        KontynuujGre -> todo ""  --GraPrzebieg
                        RozpocznijNowaGre -> m
        _ -> m


viewNew : ModelNew -> Html Msg
viewNew ( x ) =
    case x of
        GraRozpoczecie a  ->
                            div [] [
                              div [style "display" "block" , style "margin-top" "10px", style "text-align" "center"] [ text "Ekran 1"]
                            , div [style "display" "inline-block" , style "margin-top" "40px" , style "width" "50%" , style "text-align" "center"] (graczA )   -- tak powinno być , czy tak jak poniżej ????
                            , div [style "display" "inline-block" , style "margin-top" "40px" , style "width" "50%" , style "text-align" "center"] (graczB a.nazwaGraczaB)
                            , div [style "display" "flex" , style "margin-top" "50px", style "align-items" "center" ,style "justify-content" "center"] start
                            ]
        GraPrzebieg a ->    div [] [
                            div [style "display" "block" , style "margin-top" "10px", style "text-align" "center"] [ text "Ekran 2"]
                            , div [style "display" "inline-block" , style "margin-top" "40px" , style "width" "50%" , style "text-align" "center"] [text a.nazwaGraczaA
                                                                                                                                                   , div [style "height" "600px",  style "border" "1px solid black" , style "margin" "0px 70px"] [kartyGracza a.rekaGraczA]
                                                                                                                                                   , grajA (take 1 a.rekaGraczA) x
                                                                                                                                                   ]
                            , div [style "display" "inline-block" , style "margin-top" "40px" , style "width" "50%" , style "text-align" "center"] [text a.nazwaGraczaB
                                                                                                                                                   , div [style "height" "600px",  style "border" "1px solid black" , style "margin" "0px 70px"] [kartyGracza a.rekaGraczB]
                                                                                                                                                   , grajB (take 1 a.rekaGraczB) x
                                                                                                                                                   ]
                            ,div [style "display" "block" , style "margin-top" "10px", style "text-align" "center"] [div [style "height" "300px", style "border" "1px solid black", style "margin-top" "40px"] [ kartyNaStole a.kartyNaStole
                                                                                                                                                                                                               ]
                                                                                                                                                                                                               , button [] [text "Zbierz karty A"], button [] [text "Zbierz karty B"]]
                            ]
        GraZakonczenie a -> div [] [text "Ekran 3"]



graczA : List(Html Msg)
graczA  = [input [onInput (\x -> MsgRozpoczecie(UpdateNameA x))] [text ""] ]

graczB : String -> List(Html Msg)
graczB s = [input [onInput (\x -> MsgRozpoczecie(UpdateNameB x))] [text s] ]

--talia Kart - rozdanie na dwie części
rozdanieKart :  List Card -> (List Card, List Card)
rozdanieKart lc = splitAt 26 lc

start : List (Html Msg)
start =  [button [onClick (MsgRozpoczecie( StartGry))] [text "Start Gry"]]

cardHtml : Card -> Html Msg
cardHtml = \x ->
    case (cardName x ) of
       ValidName n -> div [
                           style "border" "1px solid black"
                          , style "display" "flex"
                          , style "flex-direction" "row"
                          , style "gap" "3px"
                          , style "margin" "3px"
                          , style "background-color" "pink"
                          , style "width" "30px"
                          , style "height" "40px"
                                ] [text n ]
       InvalidCard -> div [style "border" "1px solid red"
                          , style "margin" "3px"] [text "bald!"]


kartyGracza : List Card -> Html Msg
kartyGracza lc = div [style "display" "flex"] (List.map cardHtml lc)

kartyNaStole : List Card -> Html Msg
kartyNaStole lc = div [style "background-color" "yellow"] (List.map cardHtml lc)

grajA : List Card -> ModelNew -> Html Msg
grajA lc mn = case mn of
                   GraPrzebieg { kolej } -> case kolej of
                       KolejA -> button [onClick (MsgPrzebieg(ZagrajA lc))] [text "Zagraj graczu A !"]
                       KolejB -> button [disabled True ] [text "Zagraj graczu A !"]
                   _ -> todo ""

grajB : List Card -> ModelNew -> Html Msg
grajB lc mn = case mn of
                   GraPrzebieg { kolej } -> case kolej of
                       KolejA -> button [ disabled True ] [text "Zagraj graczu B !"]
                       KolejB -> button [onClick (MsgPrzebieg(ZagrajB lc))] [text "Zagraj graczu B !"]
                   _ -> todo ""






