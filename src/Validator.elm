module Validator
    exposing
        ( ValidatableInput
        , gte
        , int
        , lte
        , mixAlpha
        , mixLowercase
        , mixNumeric
        , mixSpecial
        , mixUppercase
        , required
        , string
        , validate
        )

{-| This module can be combined some validations.


# Create

@docs ValidatableInput, int, string


# Use

@docs validate


# Common validations

@docs gte, lte, mixAlpha, mixLowercase, mixNumeric, mixSpecial, mixUppercase, required

-}

import Regex


{-| A validatable input
-}
type ValidatableInput
    = ValidatableString Bool String
    | ValidatableInt Bool Int


{-| Create a valid input from string
-}
string : String -> ValidatableInput
string input =
    ValidatableString True input


{-| Create a valid input from integer
-}
int : Int -> ValidatableInput
int input =
    ValidatableInt True input


isValid : ValidatableInput -> Bool
isValid input =
    case input of
        ValidatableString isValid _ ->
            isValid

        ValidatableInt isValid _ ->
            isValid


{-| Validate the input.

    input = string "Tom"

    validate required input == True
    validate mixNumeric input == False
    validate (required << mixNumeric) input == False

-}
validate : (ValidatableInput -> ValidatableInput) -> ValidatableInput -> Bool
validate operator =
    isValid << operator


{-| Validate whether there is input.
-}
required : ValidatableInput -> ValidatableInput
required input =
    update requiredValidator input


requiredValidator : ValidatableInput -> Bool
requiredValidator input =
    case input of
        ValidatableString _ str ->
            (not << String.isEmpty << String.trim) str

        _ ->
            True


update : (ValidatableInput -> Bool) -> ValidatableInput -> ValidatableInput
update op input =
    case input of
        ValidatableString isValid val ->
            if isValid then
                ValidatableString (op input) val
            else
                input

        ValidatableInt isValid val ->
            if isValid then
                ValidatableInt (op input) val
            else
                input


{-| Validate if input is greater than or equal arbitrary value.
-}
gte : String -> ValidatableInput -> ValidatableInput
gte str validation =
    case String.toInt str of
        Ok n ->
            update (gteValidator n) validation

        Err _ ->
            update (\_ -> False) validation


gteValidator : Int -> ValidatableInput -> Bool
gteValidator x y =
    case y of
        ValidatableString _ strY ->
            x <= String.length strY

        ValidatableInt _ intY ->
            x <= intY


{-| Validate if input is less than or equal arbitrary value.
-}
lte : String -> ValidatableInput -> ValidatableInput
lte str validation =
    case String.toInt str of
        Ok n ->
            update (lteValidator n) validation

        Err _ ->
            update (\_ -> False) validation


lteValidator : Int -> ValidatableInput -> Bool
lteValidator x y =
    case y of
        ValidatableString _ strY ->
            x >= String.length strY

        ValidatableInt _ intY ->
            x >= intY


{-| -}
mixAlpha : ValidatableInput -> ValidatableInput
mixAlpha validation =
    mix (Regex.regex "[a-zA-Z]+") validation


{-| -}
mixNumeric : ValidatableInput -> ValidatableInput
mixNumeric validation =
    mix (Regex.regex "\\d+") validation


{-| -}
mixSpecial : ValidatableInput -> ValidatableInput
mixSpecial validation =
    mix (Regex.regex <| "[" ++ Regex.escape "!\"#$%&'()*+,-./\\:;?@[]^_`{|}~" ++ "]+") validation


{-| -}
mixLowercase : ValidatableInput -> ValidatableInput
mixLowercase validation =
    mix (Regex.regex "[a-z]+") validation


{-| -}
mixUppercase : ValidatableInput -> ValidatableInput
mixUppercase validation =
    mix (Regex.regex "[A-Z]+") validation


{-| -}
mix : Regex.Regex -> ValidatableInput -> ValidatableInput
mix regex validation =
    update (mixValidator regex) validation


mixValidator : Regex.Regex -> ValidatableInput -> Bool
mixValidator regex input =
    case input of
        ValidatableString _ str ->
            Regex.contains regex str

        _ ->
            False
