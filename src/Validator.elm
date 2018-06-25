module Validator
    exposing
        ( ValidatableInput
        , from
        , gte
        , lte
        , mixAlpha
        , mixLowercase
        , mixNumeric
        , mixSpecial
        , mixUppercase
        , required
        , validate
        )

{-| This module can be combined some validations.


# Create

@docs ValidatableInput, from


# Use

@docs validate


# Common validations

@docs required, gte, lte, mixAlpha, mixNumeric, mixSpecial, mixUppercase, mixLowercase

-}

import Regex


{-| A validatable input
-}
type ValidatableInput a
    = ValidatableInput Bool a


{-| Create a valid input from val
-}
from : a -> ValidatableInput a
from val =
    ValidatableInput True val


isValid : ValidatableInput a -> Bool
isValid (ValidatableInput isValid val) =
    isValid


{-| Validate the input.

    input = from "Haruka"

    validate required input == True
    validate mixNumeric input == False
    validate (required << mixNumeric) input == False

-}
validate : (ValidatableInput a -> ValidatableInput a) -> ValidatableInput a -> Bool
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
required : ValidatableInput String -> ValidatableInput String
required input =
    update requiredValidator input


requiredValidator : String -> Bool
requiredValidator input =
    (not << String.isEmpty << String.trim) input


update : (a -> Bool) -> ValidatableInput a -> ValidatableInput a
update op ((ValidatableInput isValid val) as input) =
    if isValid then
        ValidatableInput (op val) val
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
gte : Int -> ValidatableInput String -> ValidatableInput String
gte n input =
    update (gteValidator n) input


gteValidator : Int -> String -> Bool
gteValidator x input =
    x <= String.length input


{-| Validate if input is less than or equal arbitrary value.

    name = string "Haruka"
    age = int 17

    validate (lte "3") name == True
    validate (lte "-7") name == False
    validate (lte "17") 17 == True
    validate (lte "-20") 17 == False

-}
lte : Int -> ValidatableInput String -> ValidatableInput String
lte n input =
    update (lteValidator n) input


lteValidator : Int -> String -> Bool
lteValidator x input =
    x >= String.length input


{-| Validate if input contains alphabets.

    email = string "haruka17@example.com"
    age = string "17"

    validate mixAlpha email == True
    validate mixAlpha age == False

-}
mixAlpha : ValidatableInput String -> ValidatableInput String
mixAlpha input =
    mix (Regex.regex "[a-zA-Z]+") input


{-| Validate if input contains numbers.

    validEmail = string "haruka17@example.com"
    invalidEmail = string "haruka@exaple.com"

    validate mixNumeric validEmail == True
    validate mixNumeric invalidEmail == False

-}
mixNumeric : ValidatableInput String -> ValidatableInput String
mixNumeric input =
    mix (Regex.regex "\\d+") input


{-| Validate if input contains special characters.

    email = string "haruka17@example.com"
    age = string "17"

    validate mixSpecial email == True
    validate mixSpecial age == False

-}
mixSpecial : ValidatableInput String -> ValidatableInput String
mixSpecial input =
    mix (Regex.regex <| "[" ++ Regex.escape "!\"#$%&'()*+,-./\\:;?@[]^_`{|}~" ++ "]+") input


{-| Validate if input contains lowercase.

    validEmail = string "haruka17@example.com"
    invalidEmail = string "HARUKA17@EXAMPLE.COM"

    validate mixLowercase validEmail == True
    validate mixLowercase invalidEmail == False

-}
mixLowercase : ValidatableInput String -> ValidatableInput String
mixLowercase input =
    mix (Regex.regex "[a-z]+") input


{-| Validate if input contains uppercase.

    validEmail = string "HARUKA17@EXAMPLE.COM"
    invalidEmail = string "haruka17@example.com"

    validate mixUppercase validEmail == True
    validate mixUppercase invalidEmail == False

-}
mixUppercase : ValidatableInput String -> ValidatableInput String
mixUppercase input =
    mix (Regex.regex "[A-Z]+") input


mix : Regex.Regex -> ValidatableInput String -> ValidatableInput String
mix regex input =
    update (mixValidator regex) input


mixValidator : Regex.Regex -> String -> Bool
mixValidator regex input =
    Regex.contains regex input
