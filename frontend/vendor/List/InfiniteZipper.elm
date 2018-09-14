module List.InfiniteZipper
    exposing
        ( singleton
        , fromList
        , fromListWithDefault
        , first
        , last
        , previous
        , next
        , findNext
        , findFirst
        , current
        , map
        , mapCurrent
        , beginning
        , end
        , toList
        , append
        , push
        , InfiniteZipper
        )

{-|
A circular zipper list

This module provides a form of a focused list that allows moving around and does not terminate.

When the cursor is at the end of the list, right will return focus to the front of the list.
When the cursor is at the beginning of the list, previous will return focus to the end of the list.

As an example:

    InfiniteZipper.fromListWithDefault 4 [1, 2, 3]
      |> InfiniteZipper.next
      |> InfiniteZipper.next
      |> InfiniteZipper.next
      |> InfiniteZipper.current
      -- 1

    InfiniteZipper.fromListWithDefault 4 [1, 2, 3]
      |> InfiniteZipper.push 5
      |> InfiniteZipper.first
      |> InfiniteZipper.current
      -- 5

    InfiniteZipper.fromListWithDefault 4 [1, 2, 3]
      |> InfiniteZipper.next
      |> InfiniteZipper.mapCurrent ((+) 1)
      |> InfiniteZIpper.current
      -- 3

# Types
@docs InfiniteZipper

# Constructing an InfiniteZipper
@docs singleton, fromList, fromListWithDefault

# Moving focus
@docs first, last, previous, next, findNext, findFirst

# Getting and changing values
@docs current, map, mapCurrent

# Adding values
@docs append, push

# Checking where the cursor is
@docs beginning, end

# Returning the underlying list
@docs toList
-}

import List exposing (reverse)


{-| The InfiniteZipper type
-}
type InfiniteZipper a
    = InfiniteZipper (List a) a (List a)


{-| Tries to construct an InfiniteZipper from a List which may be empty
-}
fromList : List a -> Maybe (InfiniteZipper a)
fromList xs =
    case xs of
        [] ->
            Nothing

        x :: xs_ ->
            Just (InfiniteZipper [] x xs_)


{-| Creates an InfiniteZipper with a single element
-}
singleton : a -> InfiniteZipper a
singleton x =
    InfiniteZipper [] x []


{-| Returns the underlying List of elements
-}
toList : InfiniteZipper a -> List a
toList (InfiniteZipper left curr right) =
    (reverse left) ++ [ curr ] ++ right


{-| Constructs an InfiniteZipper from a List. If the List is empty creates an InfiniteZipper with the default value as the only value
-}
fromListWithDefault : a -> List a -> InfiniteZipper a
fromListWithDefault default =
    Maybe.withDefault (singleton default) << fromList


{-| Moves the focus to the first element of the List
-}
first : InfiniteZipper a -> InfiniteZipper a
first (InfiniteZipper left curr right) =
    case reverse left of
        [] ->
            (InfiniteZipper left curr right)

        x :: xs ->
            InfiniteZipper [] x (xs ++ [ curr ] ++ right)


{-| Moves the focus to the last element of the List
-}
last : InfiniteZipper a -> InfiniteZipper a
last (InfiniteZipper left curr right) =
    case reverse right of
        [] ->
            InfiniteZipper left curr right

        x :: xs ->
            InfiniteZipper (xs ++ [ curr ] ++ left) x []


{-| Moves the focus to the element after the current element, or returns focus to the first element of the List if focus is at the end
-}
next : InfiniteZipper a -> InfiniteZipper a
next ((InfiniteZipper left curr right) as z) =
    case right of
        [] ->
            first z

        x :: xs ->
            InfiniteZipper (curr :: left) x xs


{-| Moves the focus to the element previous of the current element, or returns focus to the last element of the List if focus is at the begining
-}
previous : InfiniteZipper a -> InfiniteZipper a
previous ((InfiniteZipper left curr right) as z) =
    case left of
        [] ->
            last z

        x :: xs ->
            InfiniteZipper xs x (curr :: right)


{-| Gets the element that the InfiniteZipper is currently focused on
-}
current : InfiniteZipper a -> a
current (InfiniteZipper _ curr _) =
    curr


{-| Applies a transformation function to every element in the InfiniteZipper
-}
map : (a -> b) -> InfiniteZipper a -> InfiniteZipper b
map f (InfiniteZipper left curr right) =
    InfiniteZipper (List.map f left) (f curr) (List.map f right)


{-| Applies a function to the element the InfiniteZipper is currently focused on
-}
mapCurrent : (a -> a) -> InfiniteZipper a -> InfiniteZipper a
mapCurrent f (InfiniteZipper left curr right) =
    InfiniteZipper left (f curr) right


{-| Returns True if the InfiniteZipper is currently focused on the first element of the List
-}
beginning : InfiniteZipper a -> Bool
beginning (InfiniteZipper left _ _) =
    List.isEmpty left


{-| Returns True if the InfiniteZipper is currently focused on the last element of the List
-}
end : InfiniteZipper a -> Bool
end (InfiniteZipper _ _ right) =
    List.isEmpty right


{-| Returns an InfiniteZipper focused on an element that matches a given predicate if such element can be found. Starts at the beginning of the InfiniteZipper and searches forwards until the end
-}
findFirst : (a -> Bool) -> InfiniteZipper a -> Maybe (InfiniteZipper a)
findFirst pred zipper =
    find pred <| first zipper


{-| Returns an InfiniteZipper focused on an element that matches a given predicate if such element can be found. Starts at the next element from the current element and loop around (excluding the current element)
-}
findNext : (a -> Bool) -> InfiniteZipper a -> Maybe (InfiniteZipper a)
findNext pred zipper =
    let
        search currentZipper =
            if equals currentZipper zipper then
                Nothing
            else if pred (current currentZipper) then
                Just currentZipper
            else
                search <| next currentZipper
    in
        search <| next zipper


{-| Inserts an element to the end of the InfiniteZipper, maintaining focus on the current element
-}
append : a -> InfiniteZipper a -> InfiniteZipper a
append newElem (InfiniteZipper left curr right) =
    InfiniteZipper left curr (right ++ [ newElem ])


{-| Inserts an element to the beginning of the InfiniteZipper, maintaining focus on the current element
-}
push : a -> InfiniteZipper a -> InfiniteZipper a
push newElem (InfiniteZipper left curr right) =
    InfiniteZipper (left ++ [ newElem ]) curr right


find : (a -> Bool) -> InfiniteZipper a -> Maybe (InfiniteZipper a)
find pred zipper =
    if pred (current zipper) then
        Just zipper
    else if end zipper then
        Nothing
    else
        find pred <| next zipper


equals : InfiniteZipper a -> InfiniteZipper a -> Bool
equals (InfiniteZipper left1 current1 right1) (InfiniteZipper left2 current2 right2) =
    left1 == left2 && current1 == current2 && right1 == right2
