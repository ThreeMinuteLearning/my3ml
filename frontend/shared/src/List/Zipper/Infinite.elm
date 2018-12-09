module List.Zipper.Infinite exposing (Zipper, current, findFirst, fromList, mapCurrent, next, previous)

import List.Zipper as Z


type alias Zipper a =
    Z.Zipper a


fromList : List a -> Maybe (Zipper a)
fromList =
    Z.fromList


current : Zipper a -> a
current =
    Z.current


next : Zipper a -> Zipper a
next z =
    Maybe.withDefault (Z.first z) (Z.next z)


previous : Zipper a -> Zipper a
previous z =
    Maybe.withDefault (Z.last z) (Z.previous z)


findFirst : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
findFirst =
    Z.findFirst


mapCurrent : (a -> a) -> Zipper a -> Zipper a
mapCurrent =
    Z.mapCurrent
