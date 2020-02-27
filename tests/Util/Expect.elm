module Util.Expect exposing (commutative, constant, exactly, like, like2)

import Expect exposing (Expectation)


exactly : Float -> Float -> Expectation
exactly actual expected =
    actual |> Expect.within (Expect.Absolute 0.0) expected


isFloat n =
    let
        isJust x =
            case x of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    isJust (String.toFloat n) && not (isJust (String.toInt n))



-- Helper


identical : a -> a -> Expectation
identical x y =
    if x == y then
        Expect.pass

    else
        Expect.fail <| "Expected " ++ Debug.toString x ++ " but got " ++ Debug.toString y



-- Function properties


like : (a -> b) -> (a -> b) -> a -> Expectation
like f g x =
    identical (f x) (g x)


like2 : (a -> b -> c) -> (a -> b -> c) -> a -> b -> Expectation
like2 f g x y =
    identical (f x y) (g x y)


constant : b -> (a -> b) -> a -> Expectation
constant o f x =
    like f (always o) x


commutative : (a -> a -> b) -> a -> a -> Expectation
commutative f x y =
    identical (f x y) (f y x)
