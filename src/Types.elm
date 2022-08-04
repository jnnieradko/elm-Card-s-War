module Types exposing (..)
import List exposing (..)
import Debug exposing (..)
import String

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

para : (Int, String)
para = (2,"avx")


listaPar : List (Int,String)
listaPar = [(2,"a"),(3,"b"),(4,"c")]


zTupli : (Int,String) -> String
zTupli (_,s) = s

sumy : List ( List Int ) -> List Int
sumy = map (sum)



cw1a : List ( List Int ) -> List Int
cw1a  = map (\x -> product x)

cw1b : List Int -> List String
cw1b  = map (String.fromInt)



cw1 : List (List Int) -> String
cw1 l = String.concat ( map String.fromInt ( map product l ) )

cw11 : List (List Int) -> String
cw11 = map (product >> String.fromInt) >> String.concat


{-
cw1 l = String.concat ( cw1b ( cw1a l ) )
-}

{- cw1b - List Int = cw1a - List (List Int) // cw1b w String-}

{-
- przyklady z uzyciem funkcji compose ??
- łączenie funkcji filter, map, concat, coś tam
- pisanie nie zawsze z \x
- funkcja fold
- case
- pattern matching
-}

zad1 : List String-> List Int
zad1 l = map (\x -> String.length x ) l

zad2a : List Int -> List Int
zad2a l = map (\x -> modBy 2 x) l

zad2b :  List Int -> List Bool
zad2b  = map (\x ->
                    case x of
                        0 -> True
                        1 -> False
                        _ -> False )


{-zad2 : List Int -> List Bool
zad2 l = map (\x ->
               case x of
                   0 -> True
                   1 -> False
                   _ -> False) (map (\x -> modBy 2 x) l)-}

zad2 l = zad2b (zad2a l)

swap : (a,b) -> (b,a)
swap (a,b) = (b,a)

zad3 : List(String,Int) -> List(Int,String)
{-
zad3 l = swap l
-}



zad3 l = map (\x -> swap x) l

{-
zad3 (s,i)  = (i,s)-}

