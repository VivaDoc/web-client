module Asset exposing (Image, approveChange, githubLogo, prFailed, src, vdLogo, vdTitle)

{-| Assets, such as images, videos, and audio.

Don't expose asset URLs directly; this module should be in charge of all of them. Better to have
a single source of truth.

-}

import Html exposing (..)
import Html.Attributes as Attr


type Image
    = Image String



-- IMAGES


githubLogo : Image
githubLogo =
    image "github-logo.svg"


vdLogo : Image
vdLogo =
    image "vd-logo.svg"


vdTitle : Image
vdTitle =
    image "vd-title.svg"


prFailed : Image
prFailed =
    image "pr-failed.jpg"


approveChange : Image
approveChange =
    image "vivadoc-approve-change.jpg"


image : String -> Image
image filename =
    Image ("/assets/images/" ++ filename)



-- USING IMAGES


src : Image -> Attribute msg
src (Image url) =
    Attr.src url
