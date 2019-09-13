module FetchData exposing (FetchData(..))

{-| `FetchData` is like `RemoteData` but does not contain a `NotAsked` state for data that is requested immediately.
-}


type FetchData err val
    = Loading
    | Success val
    | Failure err
