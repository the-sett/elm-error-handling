module ResultME exposing
    ( ResultME, error, errors, fromResult
    , map, map2, map3, map4, map5, map6, map7, andMap
    , mapError
    , combineList, combineDict, combineNonempty
    , andThen, flatten
    )

{-| ResultME is a variation on Result, where the `err` is a non-empty list of
errors. This is useful in situations where multiple errors can be detected in a
single pass, and it is preferable to report all errors detected, and not to fail
only on the first error.

Some examples; when parsing a form with multiple inputs and possibly multiple
errors to report to the user; when parsing some source code which may contain
multiple syntax errors.


# Type and Constructors

@docs ResultME, error, errors, fromResult


# Mapping

@docs map, map2, map3, map4, map5, map6, map7, andMap
@docs mapError


# Combining errors over collections

@docs combineList, combineDict, combineNonempty


# Chaining

@docs andThen, flatten

-}

import Dict exposing (Dict)
import List.Nonempty exposing (Nonempty(..))


{-| The result-possible-with-multiple-errors type.
-}
type alias ResultME err a =
    Result (Nonempty err) a


{-| Produces a `ResultME` from a single error.
-}
error : err -> ResultME err a
error err =
    err
        |> List.Nonempty.singleton
        |> Err


{-| Produces a `ResultME` from a list of errors.
-}
errors : err -> List err -> ResultME err a
errors err errs =
    List.Nonempty.Nonempty err errs |> Err


{-| Turns a `Result` into `ResultME` with one error.
-}
fromResult : Result err a -> ResultME err a
fromResult result =
    case result of
        Err err ->
            error err

        Ok val ->
            Ok val


{-| Combines 2 `ResultME`s together. If any of them have errors all the errors
will be gathered. Otherwise the function supplied in the second one will be
applied to the first, to produce the result value as `Ok`.
-}
andMap : ResultME e a -> ResultME e (a -> b) -> ResultME e b
andMap ra rfn =
    case ( ra, rfn ) of
        ( Ok a, Ok fun ) ->
            fun a |> Ok

        ( Err err, Ok _ ) ->
            Err err

        ( Ok _, Err err ) ->
            Err err

        ( Err err1, Err err2 ) ->
            List.Nonempty.append err1 err2
                |> Err


{-| Combines 2 `ResultME`s together. If any of them have errors all the errors
will be gathered. Otherwise the supplied function will be used to combine the
result values as `Ok`.
-}
map2 : (a -> b -> c) -> ResultME err a -> ResultME err b -> ResultME err c
map2 fun first second =
    case ( first, second ) of
        ( Ok checkedArg, Ok checkedRes ) ->
            fun checkedArg checkedRes |> Ok

        ( Err err, Ok _ ) ->
            Err err

        ( Ok _, Err err ) ->
            Err err

        ( Err err1, Err err2 ) ->
            List.Nonempty.append err1 err2
                |> Err


{-| Combines 3 `ResultME`s together. If any of them have errors all the errors
will be gathered. Otherwise the supplied function will be used to combine the
result values as `Ok`.
-}
map3 :
    (a -> b -> c -> d)
    -> ResultME err a
    -> ResultME err b
    -> ResultME err c
    -> ResultME err d
map3 fun first second third =
    case first of
        Ok checkedFirst ->
            map2 (fun checkedFirst) second third

        Err errFirst ->
            andMap third
                (map2 (flip fun) second first)


{-| Combines 4 `ResultME`s together. If any of them have errors all the errors
will be gathered. Otherwise the supplied function will be used to combine the
result values as `Ok`.
-}
map4 :
    (a -> b -> c -> d -> e)
    -> ResultME err a
    -> ResultME err b
    -> ResultME err c
    -> ResultME err d
    -> ResultME err e
map4 fun first second third fourth =
    case first of
        Ok checkedFirst ->
            map3 (fun checkedFirst) second third fourth

        Err errFirst ->
            andMap fourth
                (map3 (flip fun) second first third)


{-| Combines 5 `ResultME`s together. If any of them have errors all the errors
will be gathered. Otherwise the supplied function will be used to combine the
result values as `Ok`.
-}
map5 :
    (a -> b -> c -> d -> e -> f)
    -> ResultME err a
    -> ResultME err b
    -> ResultME err c
    -> ResultME err d
    -> ResultME err e
    -> ResultME err f
map5 fun first second third fourth fifth =
    case first of
        Ok checkedFirst ->
            map4 (fun checkedFirst) second third fourth fifth

        Err errFirst ->
            andMap fifth
                (map4 (flip fun) second first third fourth)


{-| Combines 6 `ResultME`s together. If any of them have errors all the errors
will be gathered. Otherwise the supplied function will be used to combine the
result values as `Ok`.
-}
map6 :
    (a -> b -> c -> d -> e -> f -> g)
    -> ResultME err a
    -> ResultME err b
    -> ResultME err c
    -> ResultME err d
    -> ResultME err e
    -> ResultME err f
    -> ResultME err g
map6 fun first second third fourth fifth sixth =
    case first of
        Ok checkedFirst ->
            map5 (fun checkedFirst) second third fourth fifth sixth

        Err errFirst ->
            andMap sixth
                (map5 (flip fun) second first third fourth fifth)


{-| Combines 7 `ResultME`s together. If any of them have errors all the errors
will be gathered. Otherwise the supplied function will be used to combine the
result values as `Ok`.
-}
map7 :
    (a -> b -> c -> d -> e -> f -> g -> h)
    -> ResultME err a
    -> ResultME err b
    -> ResultME err c
    -> ResultME err d
    -> ResultME err e
    -> ResultME err f
    -> ResultME err g
    -> ResultME err h
map7 fun first second third fourth fifth sixth seventh =
    case first of
        Ok checkedFirst ->
            map6 (fun checkedFirst) second third fourth fifth sixth seventh

        Err errFirst ->
            andMap seventh
                (map6 (flip fun) second first third fourth fifth sixth)


{-| Combines all errors in a `List` of `ResultME`s. All errors will
be gathered in the case where there are any errors, otherwise a `List`
of the result values will be returned as `Ok`.

    import ResultME

    ResultME.combineList
        [ Ok "this is fine"
        , ResultME.errors "failure" [ "problem" ]
        , ResultME.error "error"
        , Ok "done"
        ]
    --> ResultME.errors "failure" [ "problem", "error" ]

    ResultME.combineList
        [ Ok "ok"
        , Ok "result"
        , Ok "value"
        ]
    --> Ok [ "ok", "result", "value" ]

-}
combineList : List (ResultME err a) -> ResultME err (List a)
combineList results =
    List.foldr
        (\result accumRes ->
            case ( result, accumRes ) of
                ( Ok val, Ok accum ) ->
                    val :: accum |> Ok

                ( Err err, Ok _ ) ->
                    Err err

                ( Ok _, Err errAccum ) ->
                    Err errAccum

                ( Err err, Err errAccum ) ->
                    List.Nonempty.append err errAccum |> Err
        )
        (Ok [])
        results


{-| Combines all errors in a `Dict` of `ResultME`s. All errors will
be gathered in the case where there are any errors, otherwise a `Dict`
of the result values will be returned as `Ok`.

    import ResultME
    import Dict

    ResultME.combineDict
        (Dict.fromList
            [ ( "3", Ok 3 )
            , ( "-4", Ok -4 )
            , ( "20xFF", Ok 255 )

            ]
        )
    --> Ok
    -->     (Dict.fromList
    -->         [ ( "3", 3 )
    -->         , ( "2", 2 )
    -->         , ( "1", 1 )
    -->         ]
    -->     )

    ResultME.combineDict
        (Dict.fromList
            [ ( "3", Ok 3 )
            , ( "1.2", ResultME.error "can't contain '.'" )
            , ( "-4", Ok -4 )
            , ( "#:-3"
              , ResultME.errors
                  "can't contain '#'"
                  [ "can't contain ':'" ]
              )
            ]
        )
    --> ResultME.errors
    -->     "can't contain '.'"
    -->     [ "can't contain '#'"
    -->     , "can't contain ':'"
    -->     ]

-}
combineDict : Dict comparable (ResultME err v) -> ResultME err (Dict comparable v)
combineDict results =
    Dict.foldl
        (\key result accumRes ->
            case ( result, accumRes ) of
                ( Ok val, Ok accum ) ->
                    Dict.insert key val accum |> Ok

                ( Err err, Ok _ ) ->
                    Err err

                ( Ok _, Err errAccum ) ->
                    Err errAccum

                ( Err err, Err errAccum ) ->
                    List.Nonempty.append errAccum err |> Err
        )
        (Ok Dict.empty)
        results


{-| Combines all errors in a `Nonempty` list of `ResultME`s. All errors will
be gathered in the case where there are any errors, otherwise a `Nonempty` list
of the result values will be returned as `Ok`.

    import ResultME
    import List.Nonempty

    ResultME.combineNonempty
        (List.Nonempty.Nonempty
            (Ok "this is fine")
            [ ResultME.errors "failure" [ "problem" ]
            , ResultME.error "error"
            , Ok "done"
            ]
        )
    --> ResultME.errors "failure" [ "bad", "problem" ]

    ResultME.combineNonempty
        (List.Nonempty.Nonempty
            (Ok "ok")
            [ Ok "result"
            , Ok "value"
            ]
        )
    --> Ok
    -->     (List.Nonempty.Nonempty
    -->         "ok"
    -->         [ "result", "value" ]
    -->     )

-}
combineNonempty : Nonempty (ResultME err a) -> ResultME err (Nonempty a)
combineNonempty (Nonempty head tail) =
    List.foldl
        (\result accumRes ->
            case ( result, accumRes ) of
                ( Ok val, Ok accum ) ->
                    List.Nonempty.cons val accum |> Ok

                ( Err err, Ok _ ) ->
                    Err err

                ( Ok _, Err errAccum ) ->
                    Err errAccum

                ( Err err, Err errAccum ) ->
                    List.Nonempty.append errAccum err |> Err
        )
        (Result.map List.Nonempty.singleton head)
        tail
        |> map List.Nonempty.reverse


{-| Applies a function to the result value in a `ResultME`, if there is one.
-}
map : (a -> b) -> ResultME err a -> ResultME err b
map =
    Result.map


{-| Applies a function to the errors in a `ResultME`, if there are any.
-}
mapError : (x -> y) -> ResultME x a -> ResultME y a
mapError fun result =
    Result.mapError (List.Nonempty.map fun) result


{-| Chain together a sequence of computations that may fail. This is identical
to `Result.andThen`.
-}
andThen : (a -> ResultME err b) -> ResultME err a -> ResultME err b
andThen =
    Result.andThen


{-| Flattens the structure if one `ResultME` is nested inside another.
-}
flatten : ResultME err (ResultME err a) -> ResultME err a
flatten res =
    case res of
        Err err ->
            Err err

        Ok (Err err) ->
            Err err

        Ok (Ok val) ->
            Ok val


flip : (a -> b -> c) -> (b -> a -> c)
flip f b a =
    f a b
