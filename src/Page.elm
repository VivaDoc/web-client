module Page exposing (DisplayHeroOption(..), HighlightableTab(..), viewWithHeader)

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


type DisplayHeroOption msg
    = NoHero
    | LandingHero (LandingHeroConfig msg)


type alias LandingHeroConfig msg =
    { scrollMsg : msg
    , videoModalOpen : Bool
    , setVideoModalOpenValue : Bool -> msg
    }


type alias RenderHeaderConfig msg =
    { showHero : DisplayHeroOption msg
    , renderNavbarConfig : RenderNavbarConfig msg
    }


{-| Frame a page under a header.
-}
viewWithHeader :
    RenderHeaderConfig msg
    -> Maybe Viewer
    -> { title : String, content : Html pageMsg }
    -> (pageMsg -> msg)
    -> Document msg
viewWithHeader { showHero, renderNavbarConfig } maybeViewer { title, content } toMsg =
    { title = title
    , body =
        [ case showHero of
            NoHero ->
                renderNavbar renderNavbarConfig maybeViewer

            LandingHero landingHeroConfig ->
                renderLandingHero
                    landingHeroConfig
                    (renderNavbar renderNavbarConfig maybeViewer)
        , Html.map toMsg content
        ]
    }


type HighlightableTab
    = NoTab
    | HomeTab
    | DocumentationTab
    | AboutUsTab
    | PricingTab


type alias RenderNavbarConfig msg =
    { mobileNavbarOpen : Bool
    , toggleMobileNavbar : msg
    , logout : msg
    , loginWithGithub : msg
    , isLoggingIn : Bool
    , isLoggingOut : Bool
    , showHomeButton : Bool
    , selectedTab : HighlightableTab
    }


renderLandingHero : LandingHeroConfig msg -> Html msg -> Html msg
renderLandingHero { scrollMsg, videoModalOpen, setVideoModalOpenValue } navbar =
    section
        [ class "hero is-fullheight is-primary is-bold" ]
        [ navbar
        , if videoModalOpen then
            renderVideoModal { closeVideoModal = setVideoModalOpenValue False }

          else
            div [ class "is-hidden" ] []
        , div
            [ class "hero-title" ]
            [ div
                [ class "has-text-centered"
                , style "padding-top" "3rem"
                ]
                [ img
                    [ Asset.src Asset.playVid
                    , class "play-vid-svg is-hidden-mobile"
                    , style "width" "64px"
                    , style "height" "64px"
                    , onClick <| setVideoModalOpenValue True
                    ]
                    []
                ]
            ]
        , div
            [ class "hero-body"
            , style "padding-top" "0"
            ]
            [ div
                [ class "container has-text-centered" ]
                [ img
                    [ Asset.src Asset.vdTitle
                    , style "width" "400px"
                    , style "height" "100px"
                    ]
                    []
                , p
                    [ class "subtitle is-4 has-text-vd-base-light" ]
                    [ text "Documentation that lives" ]
                ]
            ]
        , div
            [ class "hero-foot" ]
            [ div
                [ class "has-text-centered" ]
                [ span
                    [ class "icon is-large"
                    , style "cursor" "pointer"
                    , onClick scrollMsg
                    ]
                    [ i
                        [ class "material-icons has-text-vd-spark-dark"
                        , style "font-size" "48px"
                        ]
                        [ text "expand_more" ]
                    ]
                ]
            , p
                [ class "content is-small has-text-right"
                , style "margin" "0 10px 10px 0"
                ]
                [ text "Alpha Version 1" ]
            ]
        ]


renderVideoModal : { closeVideoModal : msg } -> Html.Html msg
renderVideoModal { closeVideoModal } =
    div
        [ class "modal is-active" ]
        [ div
            [ class "modal-background"
            , style "opacity" "0.7"
            , onClick closeVideoModal
            ]
            []
        , div
            [ class "modal-content"
            , style "width" "768px"
            , style "height" "480px"
            , style "overflow" "hidden"
            ]
            [ iframe
                [ class "vd-landing-vid"
                , attribute "frameborder" "0"
                , attribute "allow" "accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture"
                , attribute "allowFullscreen" "true"
                , src "https://www.youtube.com/embed/CCB04j7kr3M?autoplay=1"
                ]
                []
            ]
        ]


{-| Render the navbar.

Will have log-in/sign-up or logout buttons according to whether there is a `Viewer`.

-}
renderNavbar : RenderNavbarConfig msg -> Maybe Viewer -> Html msg
renderNavbar config maybeViewer =
    nav [ class "navbar is-primary" ]
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
                    [ ( "navbar-burger burger has-text-vd-spark-bright", True )
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
                [ classList
                    [ ( "navbar-start", True )
                    , ( "is-hidden", not config.showHomeButton )
                    ]
                ]
                [ a
                    [ classList
                        [ ( "navbar-item", True )
                        , ( "is-border-bottom-underlined"
                          , case config.selectedTab of
                                HomeTab ->
                                    True

                                _ ->
                                    False
                          )
                        ]
                    , Route.href Route.Home
                    ]
                    [ text "Home" ]
                ]
            , div
                [ class "navbar-end" ]
                [ a
                    [ classList
                        [ ( "navbar-item", True )
                        , ( "is-border-bottom-underlined"
                          , case config.selectedTab of
                                PricingTab ->
                                    True

                                _ ->
                                    False
                          )
                        ]
                    , Route.href Route.Pricing
                    ]
                    [ text "Pricing" ]
                , a
                    [ classList
                        [ ( "navbar-item", True )
                        , ( "is-border-bottom-underlined"
                          , case config.selectedTab of
                                DocumentationTab ->
                                    True

                                _ ->
                                    False
                          )
                        ]
                    , Route.href <| Route.Documentation Route.OverviewTab
                    ]
                    [ text "Docs" ]
                , a
                    [ classList
                        [ ( "navbar-item", True )
                        , ( "is-border-bottom-underlined"
                          , case config.selectedTab of
                                AboutUsTab ->
                                    True

                                _ ->
                                    False
                          )
                        ]
                    , Route.href <| Route.AboutUs
                    ]
                    [ text "About" ]
                , div [ class "navbar-item" ]
                    (case maybeViewer of
                        Nothing ->
                            [ button
                                [ classList
                                    [ ( "button is-vd-box-link is-medium", True )
                                    , ( "is-loading", config.isLoggingIn )
                                    ]
                                , onClick config.loginWithGithub
                                ]
                                [ text "Sign in with github" ]
                            ]

                        Just viewer ->
                            [ button
                                [ classList
                                    [ ( "button is-vd-box-link is-medium", True )
                                    , ( "is-loading", config.isLoggingOut )
                                    ]
                                , onClick config.logout
                                ]
                                [ text "Log out" ]
                            ]
                    )
                ]
            ]
        ]
