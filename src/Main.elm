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
type Gracz = GraczA | GraczB
type alias ModelPrzebieg = { nazwaGraczaA : String
                           , nazwaGraczaB : String
                           , rekaGraczA : List Card
                           , rekaGraczB : List Card
                           , kartyNaStole : List (Card,Gracz)
                           , kolej : Kolej
                           }

type ModelNew = GraRozpoczecie { nazwaGraczaA : String , nazwaGraczaB : String  }
                | GraPrzebieg ModelPrzebieg
                | GraZakonczenie { nazwaGraczaWygranego : String , nazwaGraczaPrzegranego : String}

type MRozpoczecie = UpdateNameA String    --  z pola Input zapamietuje wpisany String i przekazuje do nazwy gracza A
                  | UpdateNameB String -- z pola Input zapamietuje wpisany String i przekazuje do nazwy gracza B
                  | StartGry     -- wysyła msg w celu zmiany modelu na GraPrzebieg

type MPrzebieg = ZagrajA Card      -- rekaGraczA : List Card - Card , kartyNaStole : List Card ++ [Card] , kolej : KolejA
               | ZagrajB Card     -- rekaGraczB : List Card - Card , kartyNaStole : List Card ++ [Card] , kolej : KolejB
               | ZbierzKartyA -- compareCardsWar zwróci Order GT , rekaGraczA : lc + kartyNaStole [lc]
               | ZbierzKartyB -- compareCardsWar zwróci Order LT , rekaGraczA : lc + kartyNaStole [lc]
               | ZakonczGre
               --| KartySaRowne

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
                                                , rekaGraczA = {-Tuple.first(rozdanieKart deckOfCards)-} [FaceCard Ace Spades, FaceCard Jack Spades {-, FaceCard Queen Hearts, FaceCard King Clubs-} ]
                                                , rekaGraczB = {-Tuple.second(rozdanieKart deckOfCards)-} [FaceCard King Spades,FaceCard Queen Spades {-, FaceCard Queen Diamonds , FaceCard Jack Clubs , Numeral 10 Hearts-} ]
                                                , kartyNaStole = []
                                                , kolej = KolejA
                                                }
        (MsgPrzebieg mp , GraPrzebieg gp ) ->
                   case mp of
                        ZagrajA c1 -> GraPrzebieg { gp | rekaGraczA = wyrzucKarte c1 gp.rekaGraczA
                                                      , kartyNaStole = (c1,GraczA) :: gp.kartyNaStole
                                                      , kolej = KolejB }
                        ZagrajB c2 -> GraPrzebieg { gp | rekaGraczB = wyrzucKarte c2 gp.rekaGraczB
                                                      , kartyNaStole = (c2,GraczB) :: gp.kartyNaStole
                                                      , kolej = KolejA } -- pomocnicza funkcja po Zbierz karty , funkcja która sprawdza czy jest koniec gry i czy listy kart da puste i wtedy zwraca GraZakonczenie albo gra Przebie (funkcja przyjmuje GraPrzebieg a zwraca model)
                        ZbierzKartyA -> GraPrzebieg { gp | rekaGraczA = gp.rekaGraczA ++ zTupli(gp.kartyNaStole)
                                                      , kolej = KolejA
                                                      , kartyNaStole = []
                                                    }
                        ZbierzKartyB -> GraPrzebieg { gp | rekaGraczB = gp.rekaGraczB ++ zTupli(gp.kartyNaStole)
                                                      , kolej = KolejB
                                                      , kartyNaStole = []
                                                    }
                        ZakonczGre -> GraZakonczenie {nazwaGraczaWygranego = "WyGRANY" , nazwaGraczaPrzegranego = "Przegrany"}

        (MsgZakonczenie mz , GraZakonczenie gz) ->
                   case mz of
                        KontynuujGre -> GraPrzebieg { nazwaGraczaA = gz.nazwaGraczaWygranego
                                                    , nazwaGraczaB = gz.nazwaGraczaPrzegranego
                                                    , rekaGraczA = {-Tuple.first(rozdanieKart deckOfCards)-} [FaceCard Ace Spades, FaceCard Jack Spades , FaceCard Queen Hearts, FaceCard King Clubs ]
                                                    , rekaGraczB = {-Tuple.second(rozdanieKart deckOfCards)-} [FaceCard King Spades,FaceCard Queen Spades , FaceCard Queen Diamonds , FaceCard Jack Clubs , Numeral 10 Hearts ]
                                                    , kartyNaStole = []
                                                    , kolej = KolejA
                                                    }
                        RozpocznijNowaGre -> GraRozpoczecie {nazwaGraczaA = "" , nazwaGraczaB = ""}
        _ -> m


viewNew : ModelNew -> Html Msg
viewNew ( x ) =
    case x of
        GraRozpoczecie a  ->
                            div [] [
                              div [style "display" "block" , style "margin-top" "10px", style "text-align" "center"] [ text "Ekran 1"]
                            , div [style "display" "inline-block" , style "margin-top" "40px" , style "width" "50%" , style "text-align" "center"] (graczA )   -- tak powinno być , czy tak jak poniżej ????
                            , div [style "display" "inline-block" , style "margin-top" "40px" , style "width" "50%" , style "text-align" "center"] (graczB a.nazwaGraczaB)
                            , div [style "display" "block" , style "margin-top" "50px", style "text-align" "center"] start
                            ]
        GraPrzebieg a ->    div [] [
                            div [ style "display" "block"
                                , style "margin-top" "10px"
                                , style "text-align" "center"] [ text "Ekran 2"]
                            , div [ style "display" "inline-block"
                                  , style "margin-top" "40px"
                                  , style "width" "50%" , style "text-align" "center"] [text a.nazwaGraczaA
                                                                                       , div [style "height" "300px",  style "border" "1px solid black" , style "margin" "0px 70px"] [kartyGracza a.rekaGraczA]
                                                                                       , grajA (head a.rekaGraczA) a
                                                                                       ]
                            , div [ style "display" "inline-block"
                                  , style "margin-top" "40px"
                                  , style "width" "50%"
                                  , style "text-align" "center"] [text a.nazwaGraczaB
                                                                 , div [style "height" "300px",  style "border" "1px solid black" , style "margin" "0px 70px"] [kartyGracza a.rekaGraczB]
                                                                 , grajB (head a.rekaGraczB) a
                                                                 ]
                            ,div [style "display" "block", style "text-align" "center" , style "height" "200px", style "border" "1px solid red", style "margin-top" "40px"]
                                                                [ kartyNaStoleWGrze (zTupli a.kartyNaStole)  ]
                                                                , div [style "display" "block" , style "margin-top" "10px", style "text-align" "center"] [przyciskZebraniaKart a]

                                   ]
        GraZakonczenie a -> div [] [
                            div [] [text "Ekran 3"]
                            ,div [] [text "Wygral Gracz" , text a.nazwaGraczaWygranego]
                            ]



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
                          , style "background-color" "lightgrey"
                          , style "display" "inline-block"
                          , style "margin" "5px"
                          , style "width" "30px"
                          , style "height" "40px"
                                ] [text n ]
       InvalidCard -> div [] [text "bald!"]


kartyGracza : List Card -> Html Msg
kartyGracza lc = div [style "display" "flex"] (List.map cardHtml lc)

kartyNaStoleWGrze : List Card -> Html Msg
kartyNaStoleWGrze lc = div [ style "background-color" "green", style "display" "block" , style "text-align" "center", style "margin" "10px"] (List.map cardHtml lc)


czyJegoKolej : Gracz -> ModelPrzebieg -> Bool
czyJegoKolej g mp =
            case (g , mp.kolej) of
                (GraczA , KolejA ) -> True
                (GraczB , KolejB ) -> True
                _ -> False

isNothing : Maybe a -> Bool
isNothing ma = case ma of
                    Nothing -> True
                    Just _ -> False

czyMozeZagracKarte : Gracz -> ModelPrzebieg -> Bool
czyMozeZagracKarte g mp = czyJegoKolej g mp && isNothing (czyMoznaZebrac mp)

czyMoznaZebrac : ModelPrzebieg -> Maybe Gracz
czyMoznaZebrac mp =
      if (modBy 2 (List.length mp.kartyNaStole) == 1) then Nothing else
       case mp.kartyNaStole of
          (cB , GraczB)  :: (cA , GraczA) :: _ ->
              case (compareCardsWar cA cB) of
                 EQ -> Nothing
                 LT -> Just GraczB
                 GT -> Just GraczA
          (cB , GraczA)  :: (cA , GraczB) :: _ ->
              case (compareCardsWar cA cB) of
                 EQ -> Nothing
                 LT -> Just GraczA
                 GT -> Just GraczB
          _ -> Nothing

-- List.length mp.kartyNaStole > 2 && List.length mp.kartyNaStole modBy 2
-- dl > 2 , dł parzysta , Order != EQ , nie mozna zebrac - > Nothing

--if List.length     --if List.length mp.kartyNaStole >= 2 then wygranyGracz (porownajKarty mp.kartyNaStole) mp else False


grajA : Maybe Card -> ModelPrzebieg -> Html Msg
grajA mbc mp =
      case mbc of
        Nothing -> div [] []
        Just c -> button [onClick (MsgPrzebieg(ZagrajA c )), disabled (not (czyMozeZagracKarte GraczA mp))] [text "Zagraj graczu A !"]


grajB : Maybe Card -> ModelPrzebieg -> Html Msg
grajB mbc mp =
   case mbc of
     Nothing -> if mp.rekaGraczB == [] && mp.kartyNaStole == [] then div [onClick (MsgPrzebieg(ZakonczGre))] [text "Przegrałeś"] else div [] []  -- todo "po pojawieniu się diva ma się wysłać MSG ? ? "
     Just c -> button [onClick (MsgPrzebieg(ZagrajB c )), disabled (not (czyMozeZagracKarte GraczB mp))] [text "Zagraj graczu B !"]



zTupli : List (Card,Gracz) -> List Card
zTupli = List.map (\x -> Tuple.first x)


--porownajKarty : List (Card,Gracz) -> Order
--porownajKarty lCG =  compareCardsWar (zTupli lCG)

przyciskZebraniaKart : ModelPrzebieg -> Html Msg
przyciskZebraniaKart mp = case czyMoznaZebrac mp of
                            Nothing -> div [] []
                            Just GraczA -> button [onClick (MsgPrzebieg(ZbierzKartyA)) ] [text "Zbierz graczu A"]
                            Just GraczB -> button [onClick (MsgPrzebieg(ZbierzKartyB)) ] [text "Zbierz graczu B"]





{-wygranyGracz : Order -> ModelNew -> Html Msg
wygranyGracz ord mn = case ord of
                            GT -> case mn of
                                GraPrzebieg {kolej} -> case kolej of
                                    KolejA -> button [onClick (MsgPrzebieg(ZbierzKartyA ))] [text "Zbierz graczu A !"]
                                    KolejB -> button [onClick (MsgPrzebieg(ZbierzKartyB ))] [text "Zbierz graczu B !"]
                                _ -> div [] [text "to 1 "]
                            LT -> case mn of
                                 GraPrzebieg {kolej} -> case kolej of
                                    KolejA -> button [onClick (MsgPrzebieg(ZbierzKartyB ))] [text "Zbierz graczu B !"]
                                    KolejB -> button [onClick (MsgPrzebieg(ZbierzKartyA ))] [text "Zbierz graczu A !"]
                                 _ -> div [] [text "to 1 "]
                            EQ -> div [] [text " karty są równe"]-}

-- co robic kiedy Nothing w wyrzuc kartę
-- line 109 NIC NIC NIC
-- czy do modelu dodać button Zagraj graczu A i zagraj graczu B ?
-- funkcja wygranyGracz _ - jakos dziwnie ?