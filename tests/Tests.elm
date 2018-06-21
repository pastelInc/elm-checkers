module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, conditional, int, list, string)
import Regex
import Test exposing (..)
import Validator


validateString : (Validator.Validator -> Validator.Validator) -> String -> Bool
validateString validator =
    Validator.validate validator << Validator.string


validateInt : (Validator.Validator -> Validator.Validator) -> Int -> Bool
validateInt validator =
    Validator.validate validator << Validator.int


validString : Fuzzer String
validString =
    string
        |> conditional
            { retries = 100
            , fallback = (++) "val@idsT1n" << String.right 90
            , condition = Regex.contains <| Regex.regex <| Regex.escape "^(?=.*?[A-Z])(?=.*?[!\"#$%&'()*+,-./\\:;?@[]^_`{|}~]).{7,100}"
            }


suite : Test
suite =
    describe "The Validator module"
        [ describe "Validator.gte"
            [ test "returns valid validator if it's greater than given number" <|
                \_ ->
                    let
                        policy =
                            Validator.gte "7"
                    in
                    Expect.true "Expect to be greater than 7" (validateInt policy 8)
            , test "returns valid validator if it's greater than given string" <|
                \_ ->
                    let
                        policy =
                            Validator.gte "-1"
                    in
                    Expect.true "Expect to be greater than -1" (validateString policy "y")
            , test "returns valid validator if it's equal to given string" <|
                \_ ->
                    let
                        policy =
                            Validator.gte "1"
                    in
                    Expect.true "Expect to be equal to 1" (validateString policy "x")
            , test "returns invalid validator if it's less than given number" <|
                \_ ->
                    let
                        policy =
                            Validator.gte "7"
                    in
                    Expect.false "Expect to be less than 7" (validateInt policy 5)
            , test "returns invalid validator if it's less than given string" <|
                \_ ->
                    let
                        policy =
                            Validator.gte "7"
                    in
                    Expect.false "Expect to be less than 7" (validateString policy "x")
            ]
        , describe "Validator.lte"
            [ test "returns valid validator if it's less than given number" <|
                \_ ->
                    let
                        policy =
                            Validator.lte "7"
                    in
                    Expect.true "Expect to be less than 7" (validateInt policy 6)
            , test "returns valid validator if it's less than given string" <|
                \_ ->
                    let
                        policy =
                            Validator.lte "5"
                    in
                    Expect.true "Expect to be less than 5" (validateString policy "x")
            , test "returns valid validator if it's equal to given string" <|
                \_ ->
                    let
                        policy =
                            Validator.lte "1"
                    in
                    Expect.true "Expect to be equal to 1" (validateString policy "x")
            , test "returns invalid validator if it's greater than given number" <|
                \_ ->
                    let
                        policy =
                            Validator.lte "-1"
                    in
                    Expect.false "Expect to be greater than -1" (validateInt policy 8)
            , test "returns invalid validator if it's greater than given string" <|
                \_ ->
                    let
                        policy =
                            Validator.lte "-1"
                    in
                    Expect.false "Expect to be greater than -1" (validateString policy "y")
            ]
        , describe "Validator.mixAlpha"
            [ test "returns valid validator if it contains alphabet" <|
                \_ ->
                    let
                        policy =
                            Validator.mixAlpha
                    in
                    Expect.true "Expect to contain alphabet" (validateString policy "a1b2C")
            , test "returns invalid validator if it does not contain alphabet" <|
                \_ ->
                    let
                        policy =
                            Validator.mixAlpha
                    in
                    Expect.false "Expect not to contain alphabet" (validateString policy "765")
            ]
        , describe "Validator.mixLowercase"
            [ test "returns valid validator if it contains lowercase" <|
                \_ ->
                    let
                        policy =
                            Validator.mixLowercase
                    in
                    Expect.true "Expect to contain lowercase" (validateString policy "a1b2C")
            , test "returns invalid validator if it does not contain lowercase" <|
                \_ ->
                    let
                        policy =
                            Validator.mixLowercase
                    in
                    Expect.false "Expect not to contain lowercase" (validateString policy "A1B2C")
            ]
        , describe "Validator.mixUppercase"
            [ test "returns valid validator if it contains uppercase" <|
                \_ ->
                    let
                        policy =
                            Validator.mixUppercase
                    in
                    Expect.true "Expect to contain uppercase" (validateString policy "a1b2C")
            , test "returns invalid validator if it does not contain uppercase" <|
                \_ ->
                    let
                        policy =
                            Validator.mixUppercase
                    in
                    Expect.false "Expect not to contain uppercase" (validateString policy "a1b2c")
            ]
        , describe "Validator.mixNumeric"
            [ test "returns valid validator if it contains numeric" <|
                \_ ->
                    let
                        policy =
                            Validator.mixNumeric
                    in
                    Expect.true "Expect to contain numeric" (validateString policy "a1b2C")
            , test "returns invalid validator if it does not contain numeric" <|
                \_ ->
                    let
                        policy =
                            Validator.mixNumeric
                    in
                    Expect.false "Expect not to contain numeric" (validateString policy "abc")
            ]
        , describe "Validator.mixSpecial"
            [ test "returns valid validator if it contains special characters" <|
                \_ ->
                    let
                        policy =
                            Validator.mixSpecial
                    in
                    Expect.true "Expect to contain special characters" (validateString policy "a[1@b2\"C")
            , test "returns invalid validator if it does not contain special characters" <|
                \_ ->
                    let
                        policy =
                            Validator.mixSpecial
                    in
                    Expect.false "Expect not to contain special characters" (validateString policy "a b c")
            ]
        , describe "Validator.required"
            [ test "returns valid validator if it contains some characters" <|
                \_ ->
                    let
                        policy =
                            Validator.required
                    in
                    Expect.true "Expect to contain some characters" (validateString policy "a")
            , test "returns invalid validator if it does not contain some characters" <|
                \_ ->
                    let
                        policy =
                            Validator.required
                    in
                    Expect.false "Expect not to contain some characters" (validateString policy " ")
            ]
        , describe "combine validators"
            [ fuzz validString "return valid" <|
                \str ->
                    let
                        policy =
                            Validator.required << Validator.gte "7" << Validator.lte "100" << Validator.mixSpecial << Validator.mixUppercase
                    in
                    Expect.true "Expect to valid" (validateString policy str)
            , test "return invalid" <|
                \_ ->
                    let
                        policy =
                            Validator.required << Validator.gte "7" << Validator.lte "100" << Validator.mixSpecial << Validator.mixUppercase
                    in
                    Expect.false "Expect to invalid" (validateString policy "validsT1n")
            ]
        ]
