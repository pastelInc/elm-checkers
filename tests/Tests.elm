module Tests exposing (..)

import Checkers
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, conditional, int, list, string)
import Regex
import Test exposing (..)


validateString : (Checkers.ValidatableInput String -> Checkers.ValidatableInput String) -> String -> Bool
validateString validator =
    Checkers.validate validator << Checkers.from


validString : Fuzzer String
validString =
    string
        |> conditional
            { retries = 100
            , fallback = (++) "val@idsT1n" << String.right 90
            , condition =
                Regex.escape "^(?=.*?[A-Z])(?=.*?[!\"#$%&'()*+,-./\\:;?@[]^_`{|}~]).{7,100}"
                    |> Regex.regex
                    |> Regex.contains
            }


suite : Test
suite =
    describe "The Checkers module"
        [ describe "Checkers.gte"
            [ test "returns valid validator if it's greater than given string" <|
                \_ ->
                    let
                        policy =
                            Checkers.gte -1
                    in
                    Expect.equal
                        True
                        (validateString policy "y")
            , test "returns valid validator if it's equal to given string" <|
                \_ ->
                    let
                        policy =
                            Checkers.gte 1
                    in
                    Expect.equal
                        True
                        (validateString policy "x")
            , test "returns invalid validator if it's less than given string" <|
                \_ ->
                    let
                        policy =
                            Checkers.gte 7
                    in
                    Expect.equal
                        False
                        (validateString policy "x")
            ]
        , describe "Checkers.lte"
            [ test "returns valid validator if it's less than given string" <|
                \_ ->
                    let
                        policy =
                            Checkers.lte 5
                    in
                    Expect.equal
                        True
                        (validateString policy "x")
            , test "returns valid validator if it's equal to given string" <|
                \_ ->
                    let
                        policy =
                            Checkers.lte 1
                    in
                    Expect.equal
                        True
                        (validateString policy "x")
            , test "returns invalid validator if it's greater than given string" <|
                \_ ->
                    let
                        policy =
                            Checkers.lte -1
                    in
                    Expect.equal
                        False
                        (validateString policy "y")
            ]
        , describe "Checkers.mixAlpha"
            [ test "returns valid validator if it contains alphabet" <|
                \_ ->
                    let
                        policy =
                            Checkers.mixAlpha
                    in
                    Expect.equal
                        True
                        (validateString policy "a1b2C")
            , test "returns invalid validator if it does not contain alphabet" <|
                \_ ->
                    let
                        policy =
                            Checkers.mixAlpha
                    in
                    Expect.equal
                        False
                        (validateString policy "765")
            ]
        , describe "Checkers.mixLowercase"
            [ test "returns valid validator if it contains lowercase" <|
                \_ ->
                    let
                        policy =
                            Checkers.mixLowercase
                    in
                    Expect.equal
                        True
                        (validateString policy "a1b2C")
            , test "returns invalid validator if it does not contain lowercase" <|
                \_ ->
                    let
                        policy =
                            Checkers.mixLowercase
                    in
                    Expect.equal
                        False
                        (validateString policy "A1B2C")
            ]
        , describe "Checkers.mixUppercase"
            [ test "returns valid validator if it contains uppercase" <|
                \_ ->
                    let
                        policy =
                            Checkers.mixUppercase
                    in
                    Expect.equal
                        True
                        (validateString policy "a1b2C")
            , test "returns invalid validator if it does not contain uppercase" <|
                \_ ->
                    let
                        policy =
                            Checkers.mixUppercase
                    in
                    Expect.equal
                        False
                        (validateString policy "a1b2c")
            ]
        , describe "Checkers.mixNumeric"
            [ test "returns valid validator if it contains numeric" <|
                \_ ->
                    let
                        policy =
                            Checkers.mixNumeric
                    in
                    Expect.equal
                        True
                        (validateString policy "a1b2C")
            , test "returns invalid validator if it does not contain numeric" <|
                \_ ->
                    let
                        policy =
                            Checkers.mixNumeric
                    in
                    Expect.equal
                        False
                        (validateString policy "abc")
            ]
        , describe "Checkers.mixSpecial"
            [ test "returns valid validator if it contains special characters" <|
                \_ ->
                    let
                        policy =
                            Checkers.mixSpecial
                    in
                    Expect.equal
                        True
                        (validateString policy "a[1@b2\"C")
            , test "returns invalid validator if it does not contain special characters" <|
                \_ ->
                    let
                        policy =
                            Checkers.mixSpecial
                    in
                    Expect.equal
                        False
                        (validateString policy "a b c")
            ]
        , describe "Checkers.required"
            [ test "returns valid validator if it contains some characters" <|
                \_ ->
                    let
                        policy =
                            Checkers.required
                    in
                    Expect.equal
                        True
                        (validateString policy "a")
            , test "returns invalid validator if it does not contain some characters" <|
                \_ ->
                    let
                        policy =
                            Checkers.required
                    in
                    Expect.equal
                        False
                        (validateString policy " ")
            ]
        , describe "combine validators"
            [ fuzz validString "return valid" <|
                \str ->
                    let
                        policy =
                            Checkers.required
                                << Checkers.gte 7
                                << Checkers.lte 100
                                << Checkers.mixSpecial
                                << Checkers.mixUppercase
                    in
                    Expect.equal
                        True
                        (validateString policy str)
            , test "return invalid" <|
                \_ ->
                    let
                        policy =
                            Checkers.required
                                << Checkers.gte 7
                                << Checkers.lte 100
                                << Checkers.mixSpecial
                                << Checkers.mixUppercase
                    in
                    Expect.equal
                        False
                        (validateString policy "validsT1n")
            ]
        ]
