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
    = ValidatableInput
        { isValid : Bool
        , attr : Attribute
        }


type Attribute
    = String_ String
    | Integer Int


{-| Create a valid input from string
-}
string : String -> ValidatableInput
string attr =
    ValidatableInput
        { isValid = True
        , attr = String_ attr
        }


{-| Create a valid input from integer
-}
int : Int -> ValidatableInput
int attr =
    ValidatableInput
        { isValid = True
        , attr = Integer attr
        }


isValid : ValidatableInput -> Bool
isValid (ValidatableInput { isValid }) =
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


requiredValidator : Attribute -> Bool
requiredValidator attr =
    case attr of
        String_ str ->
            (not << String.isEmpty << String.trim) str

        _ ->
            True


update : (Attribute -> Bool) -> ValidatableInput -> ValidatableInput
update op ((ValidatableInput { isValid, attr }) as validation) =
    if isValid then
        ValidatableInput
            { isValid = op attr
            , attr = attr
            }
    else
        validation


{-| Validate if input is greater than or equal arbitrary value.
-}
gte : String -> ValidatableInput -> ValidatableInput
gte str validation =
    case String.toInt str of
        Ok n ->
            update (gteValidator n) validation

        Err _ ->
            update (\_ -> False) validation


gteValidator : Int -> Attribute -> Bool
gteValidator x y =
    case y of
        String_ strY ->
            x <= String.length strY

        Integer intY ->
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


lteValidator : Int -> Attribute -> Bool
lteValidator x y =
    case y of
        String_ strY ->
            x >= String.length strY

        Integer intY ->
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


mixValidator : Regex.Regex -> Attribute -> Bool
mixValidator regex attr =
    case attr of
        String_ str ->
            Regex.contains regex str

        _ ->
            False
