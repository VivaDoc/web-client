module Language exposing (Language(..), decodeLanguage, toString)

import Json.Decode as Decode


{-| This file matches the backend languages heree: /node-services/src/github-app/languages/languages.ts
-}
type Language
    = JavaScript
    | TypeScript
    | Java
    | C
    | CPlusPlus
    | CSharp
    | Go


toString : Language -> String
toString language =
    case language of
        JavaScript ->
            "JavaScript"

        TypeScript ->
            "TypeScript"

        Java ->
            "Java"

        C ->
            "C"

        CPlusPlus ->
            "C++"

        CSharp ->
            "C#"

        Go ->
            "Go"


decodeLanguage : Decode.Decoder Language
decodeLanguage =
    Decode.string
        |> Decode.andThen
            (\strLang ->
                case strLang of
                    "JavaScript" ->
                        Decode.succeed JavaScript

                    "TypeScript" ->
                        Decode.succeed TypeScript

                    "Java" ->
                        Decode.succeed Java

                    "C" ->
                        Decode.succeed C

                    "C++" ->
                        Decode.succeed CPlusPlus

                    "C#" ->
                        Decode.succeed CSharp

                    "Go" ->
                        Decode.succeed Go

                    _ ->
                        Decode.fail <| "Unsupported language: " ++ strLang
            )
