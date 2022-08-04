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






