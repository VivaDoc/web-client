module Page exposing (HighlightableTab(..), viewWithHeader)

{-| This allows you to insert a page under a common header. The header is usually a navbar but not always.
-}

import Asset
import Browser exposing (Document)
import Github
import Html exposing (Html, a, button, div, h1, i, iframe, img, li, nav, p, section, span, strong, text, ul)
import Html.Attributes exposing (attribute, class, classList, height, href, src, style, width)
import Html.Events exposing (onClick)
import Route exposing (Route)
import Session exposing (Session)
import Viewer exposing (Viewer)


{-| Frame a page under a header.
-}
viewWithHeader :
    RenderNavbarConfig msg
    -> Maybe Viewer
    -> { title : String, content : Html pageMsg }
    -> (pageMsg -> msg)
    -> Document msg
viewWithHeader renderNavbarConfig maybeViewer { title, content } toMsg =
    { title = title
    , body =
        [ renderNavbar renderNavbarConfig maybeViewer
        , Html.map toMsg <| div [ class "color-bg-grey" ] [ content ]
        ]
    }


type HighlightableTab
    = NoTab
    | HomeTab
    | DocumentationTab


type alias RenderNavbarConfig msg =
    { mobileNavbarOpen : Bool
    , toggleMobileNavbar : msg
    , logout : msg
    , loginWithGithub : msg
    , isLoggingIn : Bool
    , isLoggingOut : Bool
    , selectedTab : HighlightableTab
    }


{-| Render the navbar.

Will have log-in/sign-up or logout buttons according to whether there is a `Viewer`.

-}
renderNavbar : RenderNavbarConfig msg -> Maybe Viewer -> Html msg
renderNavbar config maybeViewer =
    nav [ class "navbar is-spaced has-shadow" ]
        [ div
            [ class "navbar-brand" ]
            [ div
                [ class "navbar-item"
                , style "padding" "5px"
                , style "width" "50px"
                , style "margin-left" "3px"
                ]
                [ img
                    [ style "height" "45px !important"
                    , style "max-height" "45px"
                    , Asset.src Asset.vdLogo
                    ]
                    []
                ]
            , div
                [ classList
                    [ ( "navbar-burger burger", True )
                    , ( "is-active", config.mobileNavbarOpen )
                    ]
                , onClick config.toggleMobileNavbar
                ]
                [ span [] [], span [] [], span [] [] ]
            ]
        , div
            [ classList
                [ ( "navbar-menu", True )
                , ( "is-active", config.mobileNavbarOpen )
                ]
            ]
            [ div
                [ class "navbar-start" ]
                [ a
                    [ class "navbar-item"
                    , Route.href Route.Home
                    ]
                    [ text "Home" ]
                , a
                    [ class "navbar-item"
                    , Route.href <| Route.Documentation Route.OverviewTab
                    ]
                    [ text "Documentation" ]
                ]
            , div
                [ class "navbar-end" ]
                [ div
                    [ class "navbar-item" ]
                    [ case maybeViewer of
                        Nothing ->
                            div
                                [ class "buttons" ]
                                [ a
                                    [ class "button is-dark"
                                    , href "https://github.com/vivadoc"
                                    ]
                                    [ text "View code on GitHub" ]
                                , button
                                    [ classList
                                        [ ( "button is-info", True )
                                        , ( "is-loading", config.isLoggingIn )
                                        ]
                                    , onClick config.loginWithGithub
                                    ]
                                    [ text "Sign in with GitHub" ]
                                ]

                        Just viewer ->
                            button
                                [ classList
                                    [ ( "button is-light is-medium", True )
                                    , ( "is-loading", config.isLoggingOut )
                                    ]
                                , onClick config.logout
                                ]
                                [ text "Log out" ]
                    ]
                ]
            ]
        ]
