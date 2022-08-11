module Cards exposing (..)
import List exposing (..)
import Debug exposing (..)
test = "abc"

-- wrzucić tu wszystki rzeczy związane z kartami ale niezwiązane z Html


type CardColor = Spades | Hearts | Clubs | Diamonds
type Face = Ace | King | Queen | Jack
type Card = FaceCard Face CardColor | Numeral Int CardColor

polishName : CardColor -> String
polishName x =
    case x of
      Spades -> String.fromChar '\u{2660}'
      Hearts -> String.fromChar '\u{2665}'
      Clubs -> String.fromChar '\u{2663}'
      Diamonds -> String.fromChar '\u{2666}'




polishNameFace : Face -> String
polishNameFace x =
    case x of
      Ace -> "As"
      King -> "Król"
      Queen -> "Królowa"
      Jack -> "Walet"

type CardNameResult = ValidName String | InvalidCard

cardName : Card -> CardNameResult
cardName = \x ->
    case x of
        FaceCard z y ->
            let cN = polishName y
            in ValidName (case z of
                    Ace -> ("As " ++ cN)
                    King ->  ("K " ++ cN)
                    Queen -> ("Q " ++ cN)
                    Jack -> ("J " ++ cN)
                    )
        Numeral y z ->
            let cN = polishName z
            in case y of
                2 -> ValidName ("2" ++ cN)
                3 -> ValidName ("3 " ++ cN)
                4 -> ValidName ("4 " ++ cN)
                5 -> ValidName ("5 " ++ cN)
                6 -> ValidName ("6 " ++ cN)
                7 -> ValidName ("7 " ++ cN)
                8 -> ValidName ("8 " ++ cN)
                9 -> ValidName ("9 " ++ cN)
                10 -> ValidName ("10 " ++ cN)
                _ -> InvalidCard


numbers : List Int
numbers = range 2 10

deckOfCards : List Card
deckOfCards =
  List.concat (List.map allCardsOfColor allColors)

allColors : List CardColor
allColors = [Spades,Hearts,Clubs,Diamonds]

allCardsOfColor : CardColor -> List Card
allCardsOfColor cc =
    allFaceCardsOfColor cc ++ allRegularCardsOfColor cc


allFaces : List Face
allFaces = [Ace,King,Queen,Jack]

allFaceCardsOfColor : CardColor -> List Card
allFaceCardsOfColor cc =
    List.map (\x -> FaceCard x cc ) allFaces

allRegularCardsOfColor: CardColor -> List Card
allRegularCardsOfColor cc =
    List.map (\x -> Numeral x cc) numbers

wyrzucKarte : List Card -> List Card -> List Card
wyrzucKarte lc1 lc = case head lc1 of
                            Nothing -> todo ""
                            Just x -> (List.filter (\z -> not ( x == z )) lc)

--List.concat (List.filter (\x -> not ( lc1 == x )) [lc])



changeFaceToInt : Face -> Int
changeFaceToInt f = case f of
    Jack -> 11
    Queen -> 12
    King -> 13
    Ace -> 14

compare1 : Face -> Face -> Order
compare1 f1 f2 = compare (changeFaceToInt f1) (changeFaceToInt f2)
{-    case (f1, f2) of
        (Ace , Ace ) -> EQ
        (Ace , _ ) -> GT
        (King , King ) -> EQ
        (King , _ ) -> GT
        (Queen , Queen ) -> EQ
        (Queen , _ ) -> GT
        (Jack , Jack ) -> EQ
        (Jack , _ ) -> GT-}

compareCardsWar : List Card -> List Card -> Order
compareCardsWar c1 c2 =
    case (c1, c2) of
      ( [FaceCard x _] , [FaceCard a _]) -> compare1 x a
      ( [FaceCard y _] , [Numeral x _] ) -> GT
      ( [Numeral x _] , [Numeral z _] ) -> compare x z
      _ -> LT


-- czy funkcja compare powinnaprzyjmować : Tuple (nazwaGraczaA , wyrzuconaKartagraczaA) , (nazwaGraczaB , wyrzuconaKartagraczaB)

{-
- jak działaja rekordy w elmie, poczytac o rekordach
- przerobic kod zeby Model był rekordem
-

- zrobic przycisk Usuń gracza X

-
-}