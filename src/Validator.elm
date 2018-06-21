module Validator
    exposing
        ( Validator
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

{-| This module can be combined some validators.

@docs Validator
@docs int
@docs string
@docs validate


# Common validators

@docs gte
@docs lte
@docs mixAlpha
@docs mixLowercase
@docs mixNumeric
@docs mixSpecial
@docs mixUppercase
@docs required

-}

import Regex


{-| -}
type Validator
    = Validator
        { isValid : Bool
        , input : Attribute
        }


type Attribute
    = String_ String
    | Integer Int


{-| -}
string : String -> Validator
string input =
    Validator
        { isValid = True
        , input = String_ input
        }


{-| -}
int : Int -> Validator
int input =
    Validator
        { isValid = True
        , input = Integer input
        }


isValid : Validator -> Bool
isValid (Validator { isValid }) =
    isValid


{-| -}
validate : (Validator -> Validator) -> Validator -> Bool
validate operator =
    isValid << operator


{-| -}
required : Validator -> Validator
required ((Validator { input }) as validator) =
    update requiredOp validator


requiredOp : Attribute -> Bool
requiredOp attr =
    case attr of
        String_ str ->
            (not << String.isEmpty << String.trim) str

        _ ->
            True


update : (Attribute -> Bool) -> Validator -> Validator
update op ((Validator { isValid, input }) as validator) =
    if isValid then
        Validator
            { isValid = op input
            , input = input
            }
    else
        validator


{-| -}
gte : String -> Validator -> Validator
gte str validator =
    case String.toInt str of
        Ok n ->
            update (gteOp n) validator

        Err _ ->
            update (\_ -> False) validator


gteOp : Int -> Attribute -> Bool
gteOp x y =
    case y of
        String_ strY ->
            x <= String.length strY

        Integer intY ->
            x <= intY


{-| -}
lte : String -> Validator -> Validator
lte str validator =
    case String.toInt str of
        Ok n ->
            update (lteOp n) validator

        Err _ ->
            update (\_ -> False) validator


lteOp : Int -> Attribute -> Bool
lteOp x y =
    case y of
        String_ strY ->
            x >= String.length strY

        Integer intY ->
            x >= intY


{-| -}
mixAlpha : Validator -> Validator
mixAlpha validator =
    mix (Regex.regex "[a-zA-Z]+") validator


{-| -}
mixNumeric : Validator -> Validator
mixNumeric validator =
    mix (Regex.regex "\\d+") validator


{-| -}
mixSpecial : Validator -> Validator
mixSpecial validator =
    mix (Regex.regex <| "[" ++ Regex.escape "!\"#$%&'()*+,-./\\:;?@[]^_`{|}~" ++ "]+") validator


{-| -}
mixLowercase : Validator -> Validator
mixLowercase validator =
    mix (Regex.regex "[a-z]+") validator


{-| -}
mixUppercase : Validator -> Validator
mixUppercase validator =
    mix (Regex.regex "[A-Z]+") validator


{-| -}
mix : Regex.Regex -> Validator -> Validator
mix regex validator =
    update (mixOp regex) validator


mixOp : Regex.Regex -> Attribute -> Bool
mixOp regex attr =
    case attr of
        String_ str ->
            Regex.contains regex str

        _ ->
            False
