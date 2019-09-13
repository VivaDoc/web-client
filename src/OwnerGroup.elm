module OwnerGroup exposing (OwnerGroup, isUserInAnyGroup, isUserInGroup)


type alias OwnerGroup =
    List String


isUserInGroup : String -> OwnerGroup -> Bool
isUserInGroup username =
    List.any ((==) username)


isUserInAnyGroup : String -> List OwnerGroup -> Bool
isUserInAnyGroup username =
    List.any (isUserInGroup username)
