module Cards exposing (..)
import List exposing (..)
import Debug exposing (..)
test = "abc"

-- wrzucić tu wszystki rzeczy związane z kartami ale niezwiązane z Html


type CardColor = Spades | Hearts | Clubs | Diamonds
type Face = King | Queen | Jack | Ace
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
      King -> "Król"
      Queen -> "Królowa"
      Jack -> "Walet"
      Ace -> "As"

type CardNameResult = ValidName String | InvalidCard

cardName : Card -> CardNameResult
cardName = \x ->
    case x of
        FaceCard z y ->
            let cN = polishName y
            in ValidName (case z of
                    King ->  ("Król " ++ cN)
                    Queen -> ("Królowa " ++ cN)
                    Ace -> ("As " ++ cN)
                    Jack -> ("Walet " ++ cN)
                    )
        Numeral y z ->
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
allFaces = [Queen,King,Jack,Ace]

allFaceCardsOfColor : CardColor -> List Card
allFaceCardsOfColor cc =
    List.map (\x -> FaceCard x cc ) allFaces

allRegularCardsOfColor: CardColor -> List Card
allRegularCardsOfColor cc =
    List.map (\x -> Numeral x cc) numbers

wyrzucKarte : Card -> List Card -> List Card
wyrzucKarte c lc = List.filter (\x -> not ( c == x )) lc

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

compareCardsWar : Card -> Card -> Order
compareCardsWar c1 c2 =
    case (c1, c2) of
      ( FaceCard x _ , FaceCard a _) -> compare1 x a
      ( FaceCard y _ , Numeral x _ ) -> GT
      ( Numeral x _ , Numeral z _ ) -> compare x z
      _ -> LT


{-
- jak działaja rekordy w elmie, poczytac o rekordach
- przerobic kod zeby Model był rekordem
-

- zrobic przycisk Usuń gracza X

-
-}