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

@docs required, gte, lte, mixAlpha, mixNumeric, mixSpecial, mixUppercase, mixLowercase

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

    input = string "Haruka"

    validate required input == True
    validate mixNumeric input == False
    validate (required << mixNumeric) input == False

-}
validate : (ValidatableInput -> ValidatableInput) -> ValidatableInput -> Bool
validate operator =
    isValid << operator


{-| Validate whether there is input.

    name = string "Haruka"
    age = int 17
    empty = string ""

    validate required name == True
    validate required age == True
    validate required empty == False
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

    name = string "Haruka"
    age = int 17

    validate (gte "3") name == True
    validate (gte "7") name == False
    validate (gte "17") 17 == True
    validate (gte "20") 17 == False
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

    name = string "Haruka"
    age = int 17

    validate (lte "3") name == True
    validate (lte "-7") name == False
    validate (lte "17") 17 == True
    validate (lte "-20") 17 == False
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


{-| Validate if input contains alphabets.

    email = string "haruka17@example.com"
    age = string "17"

    validate mixAlpha email == True
    validate mixAlpha age == False
-}
mixAlpha : ValidatableInput -> ValidatableInput
mixAlpha validation =
    mix (Regex.regex "[a-zA-Z]+") validation


{-| Validate if input contains numbers.

    validEmail = string "haruka17@example.com"
    invalidEmail = string "haruka@exaple.com"

    validate mixNumeric validEmail == True
    validate mixNumeric invalidEmail == False
-}
mixNumeric : ValidatableInput -> ValidatableInput
mixNumeric validation =
    mix (Regex.regex "\\d+") validation


{-| Validate if input contains special characters.

    email = string "haruka17@example.com"
    age = string "17"

    validate mixSpecial email == True
    validate mixSpecial age == False
-}
mixSpecial : ValidatableInput -> ValidatableInput
mixSpecial validation =
    mix (Regex.regex <| "[" ++ Regex.escape "!\"#$%&'()*+,-./\\:;?@[]^_`{|}~" ++ "]+") validation


{-| Validate if input contains lowercase.

    validEmail = string "haruka17@example.com"
    invalidEmail = string "HARUKA17@EXAMPLE.COM"

    validate mixLowercase validEmail == True
    validate mixLowercase invalidEmail == False
-}
mixLowercase : ValidatableInput -> ValidatableInput
mixLowercase validation =
    mix (Regex.regex "[a-z]+") validation


{-| Validate if input contains uppercase.

    validEmail = string "HARUKA17@EXAMPLE.COM"
    invalidEmail = string "haruka17@example.com"

    validate mixUppercase validEmail == True
    validate mixUppercase invalidEmail == False
-}
mixUppercase : ValidatableInput -> ValidatableInput
mixUppercase validation =
    mix (Regex.regex "[A-Z]+") validation


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
