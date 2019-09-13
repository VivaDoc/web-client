module Main exposing (main)

{-| The entry-point to the application. This module should remain minimal.
-}

import Api.Api as Api
import Api.Core as Core
import Browser exposing (Document)
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Ease
import Github
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import Json.Decode as Decode
import LocalStorage
import Page
import Page.AboutUs as AboutUs
import Page.CommitReview as CommitReview
import Page.Documentation as Documentation
import Page.Home as Home
import Page.NotFound as NotFound
import Page.OAuthRedirect as OAuthRedirect
import Page.Pricing as Pricing
import Page.Redirect as Redirect
import Page.Repo as Repo
import Ports
import Route exposing (Route)
import Session exposing (Session)
import SmoothScroll
import Task
import Url exposing (Url)
import Viewer exposing (Viewer)



-- MODEL


type alias Model =
    { mobileNavbarOpen : Bool
    , isLoggingIn : Bool
    , isLoggingOut : Bool
    , pageModel : PageModel
    , videoModalOpen : Bool
    }


type PageModel
    = Redirect Session Redirect.RedirectDisplay
    | NotFound Session
    | Home Home.Model
    | OAuthRedirect OAuthRedirect.Model
    | CommitReview CommitReview.Model
    | Documentation Documentation.Model
    | Repo Repo.Model
    | AboutUs AboutUs.Model
    | Pricing Pricing.Model


{-| On init we have 2 cases:

    1. Github redirects the user with an access code (not a token yet)
    2. The user goes to the website (the common case)

-}
init : Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    case Route.fromUrl url of
        -- Sent here by github, completing the process to get access token.
        (Just (Route.OAuthRedirect _)) as oauthRedirectRoute ->
            changeRouteTo
                oauthRedirectRoute
                { mobileNavbarOpen = False
                , pageModel = Redirect (Session.Guest navKey) Redirect.FetchingUser
                , isLoggingIn = True
                , isLoggingOut = False
                , videoModalOpen = False
                }

        -- Otherwise we've hit the website and should try to get the user
        maybeRoute ->
            ( { mobileNavbarOpen = False
              , pageModel =
                    Redirect (Session.Guest navKey) <|
                        -- To prevent the flash of the loading user spinner on the landing page we don't display
                        -- the fetching user spinner on those pages. On the other pages it looks nice, especially the
                        -- stacked spinners ("loading user" -> "loading commit review").
                        case maybeRoute of
                            Nothing ->
                                Redirect.Blank

                            Just Route.Home ->
                                Redirect.Blank

                            _ ->
                                Redirect.FetchingUser
              , isLoggingIn = True
              , isLoggingOut = False
              , videoModalOpen = False
              }
            , Api.getUser (CompletedGetUser maybeRoute)
            )



-- VIEW


view : Model -> Document Msg
view model =
    let
        viewer =
            Session.getViewer (toSession model)

        viewPageWithNavbar { showHomeButton, showHero, selectedTab } toMsg pageView =
            let
                { title, body } =
                    Page.viewWithHeader
                        { showHero = showHero
                        , renderNavbarConfig =
                            { mobileNavbarOpen = model.mobileNavbarOpen
                            , toggleMobileNavbar = ToggledMobileNavbar
                            , logout = Logout
                            , loginWithGithub = OnClickLoginWithGithub
                            , isLoggingIn = model.isLoggingIn
                            , isLoggingOut = model.isLoggingOut
                            , showHomeButton = showHomeButton
                            , selectedTab = selectedTab
                            }
                        }
                        viewer
                        pageView
                        toMsg
            in
            { title = title
            , body = body
            }
    in
    case model.pageModel of
        Redirect _ redirectReason ->
            viewPageWithNavbar
                { showHomeButton = False, showHero = Page.NoHero, selectedTab = Page.NoTab }
                (\_ -> Ignored)
                (Redirect.view redirectReason)

        NotFound _ ->
            viewPageWithNavbar
                { showHomeButton = True, showHero = Page.NoHero, selectedTab = Page.NoTab }
                (\_ -> Ignored)
                NotFound.view

        Home homeModel ->
            viewPageWithNavbar
                (case viewer of
                    Nothing ->
                        { showHomeButton = False
                        , showHero =
                            Page.LandingHero
                                { scrollMsg = LandingPageScrollDown
                                , videoModalOpen = model.videoModalOpen
                                , setVideoModalOpenValue = ToggleVideoOpenModal
                                }
                        , selectedTab = Page.NoTab
                        }

                    Just _ ->
                        { showHomeButton = True, showHero = Page.NoHero, selectedTab = Page.HomeTab }
                )
                GotHomeMsg
                (Home.view homeModel)

        OAuthRedirect oauthRedirect ->
            viewPageWithNavbar
                { showHomeButton = False, showHero = Page.NoHero, selectedTab = Page.NoTab }
                GotOAuthRedirectMsg
                (OAuthRedirect.view oauthRedirect)

        CommitReview commitReviewModel ->
            viewPageWithNavbar
                { showHomeButton = True, showHero = Page.NoHero, selectedTab = Page.NoTab }
                GotCommitReviewMsg
                (CommitReview.view commitReviewModel)

        Documentation documentationModel ->
            viewPageWithNavbar
                { showHomeButton = True, showHero = Page.NoHero, selectedTab = Page.DocumentationTab }
                GotDocumentationMsg
                (Documentation.view documentationModel)

        Repo repoModel ->
            viewPageWithNavbar
                { showHomeButton = True, showHero = Page.NoHero, selectedTab = Page.NoTab }
                GotRepoMsg
                (Repo.view repoModel)

        AboutUs aboutUsModel ->
            viewPageWithNavbar
                { showHomeButton = True, showHero = Page.NoHero, selectedTab = Page.AboutUsTab }
                GotAboutUsMsg
                (AboutUs.view aboutUsModel)

        Pricing pricingModel ->
            viewPageWithNavbar
                { showHomeButton = True, showHero = Page.NoHero, selectedTab = Page.PricingTab }
                GotPricingMsg
                (Pricing.view pricingModel)



-- UPDATE


type Msg
    = Ignored
    | OnLoadLocalStorage String
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | OnClickLoginWithGithub
    | ToggledMobileNavbar
    | CompletedGetUser (Maybe Route.Route) (Result (Core.HttpError ()) Viewer.Viewer)
    | Logout
    | CompletedLogout (Result (Core.HttpError ()) ())
    | GotHomeMsg Home.Msg
    | GotCommitReviewMsg CommitReview.Msg
    | GotOAuthRedirectMsg OAuthRedirect.Msg
    | GotDocumentationMsg Documentation.Msg
    | GotRepoMsg Repo.Msg
    | GotAboutUsMsg AboutUs.Msg
    | GotPricingMsg Pricing.Msg
    | LandingPageScrollDown
    | ToggleVideoOpenModal Bool


toSession : Model -> Session
toSession { pageModel } =
    case pageModel of
        Redirect session _ ->
            session

        NotFound session ->
            session

        Home homeModel ->
            Home.toSession homeModel

        OAuthRedirect oauthRedirect ->
            OAuthRedirect.toSession oauthRedirect

        CommitReview commitReviewModel ->
            CommitReview.toSession commitReviewModel

        Documentation { session } ->
            session

        Repo { session } ->
            session

        AboutUs { session } ->
            session

        Pricing { session } ->
            session


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model

        closeMobileNavbar =
            { model | mobileNavbarOpen = False }
    in
    case maybeRoute of
        Nothing ->
            ( { model
                | mobileNavbarOpen = False
                , pageModel = NotFound session
              }
            , Cmd.none
            )

        Just Route.Root ->
            ( closeMobileNavbar
            , Route.replaceUrl (Session.getNavKey session) Route.Home
            )

        Just Route.Home ->
            Home.init session
                |> updatePageModel Home GotHomeMsg model

        Just (Route.OAuthRedirect maybeCode) ->
            OAuthRedirect.init session maybeCode
                |> updatePageModel OAuthRedirect GotOAuthRedirectMsg model

        Just (Route.CommitReview repoId prNumber commitId) ->
            CommitReview.init session repoId prNumber commitId
                |> updatePageModel CommitReview GotCommitReviewMsg model

        Just (Route.Documentation docTab) ->
            Documentation.init session docTab
                |> updatePageModel Documentation GotDocumentationMsg model

        Just (Route.Repo repoId) ->
            Repo.init session repoId
                |> updatePageModel Repo GotRepoMsg model

        Just Route.AboutUs ->
            AboutUs.init session
                |> updatePageModel AboutUs GotAboutUsMsg model

        Just Route.Pricing ->
            Pricing.init session
                |> updatePageModel Pricing GotPricingMsg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentSession =
            toSession model

        currentNavKey =
            Session.getNavKey currentSession
    in
    case ( msg, model.pageModel ) of
        ( Ignored, _ ) ->
            ( model, Cmd.none )

        -- NOTE: Currently local storage is strictly for temporarily saving a url
        ( OnLoadLocalStorage str, _ ) ->
            ( model
            , case Decode.decodeString LocalStorage.decodeLocalStorage str of
                Ok { relativeUrl } ->
                    Cmd.batch
                        [ Nav.pushUrl currentNavKey relativeUrl
                        , LocalStorage.clearModel
                        ]

                Err err ->
                    Route.replaceUrl currentNavKey Route.Home
            )

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl currentNavKey (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( OnClickLoginWithGithub, _ ) ->
            ( model
            , Cmd.batch
                [ -- Save a url to jump back to after auth if needed
                  case model.pageModel of
                    CommitReview { repoId, prNumber, commitId } ->
                        LocalStorage.saveModel
                            { relativeUrl =
                                Route.CommitReview repoId prNumber commitId
                                    |> Route.routeToString
                            }

                    Redirect _ _ ->
                        Cmd.none

                    NotFound _ ->
                        Cmd.none

                    Home _ ->
                        Cmd.none

                    OAuthRedirect _ ->
                        Cmd.none

                    Documentation _ ->
                        Cmd.none

                    Repo { repoId } ->
                        LocalStorage.saveModel
                            { relativeUrl =
                                Route.Repo repoId |> Route.routeToString
                            }

                    AboutUs _ ->
                        Cmd.none

                    Pricing _ ->
                        Cmd.none
                , Nav.load Github.oAuthSignInLink
                ]
            )

        ( ToggledMobileNavbar, _ ) ->
            ( { model | mobileNavbarOpen = not model.mobileNavbarOpen }
            , Cmd.none
            )

        ( CompletedGetUser maybeRoute (Ok viewer), _ ) ->
            let
                goToRoute =
                    Maybe.withDefault Route.Home maybeRoute
            in
            ( { model
                | mobileNavbarOpen = False
                , isLoggingIn = False
                , pageModel = Redirect (Session.LoggedIn currentNavKey viewer) Redirect.Blank
              }
            , Route.replaceUrl currentNavKey goToRoute
            )

        -- TODO handle error better (eg. network error)
        ( CompletedGetUser maybeRoute (Err err), _ ) ->
            changeRouteTo maybeRoute { model | isLoggingIn = False }

        ( Logout, _ ) ->
            ( { model | isLoggingOut = True }, Api.getLogout CompletedLogout )

        ( CompletedLogout (Ok _), _ ) ->
            changeRouteTo
                (Just Route.Home)
                { model
                    | pageModel = Redirect (Session.Guest currentNavKey) Redirect.Blank
                    , isLoggingOut = False
                }

        -- TODO
        ( CompletedLogout (Err _), _ ) ->
            ( { model | isLoggingOut = False }, Cmd.none )

        ( LandingPageScrollDown, Home _ ) ->
            let
                scrollMilliseconds =
                    750

                scrollEase =
                    Ease.inOutSine

                scrollTask =
                    Dom.getViewport
                        |> Task.andThen
                            (\{ viewport } ->
                                SmoothScroll.scrollTo
                                    (SmoothScroll.createConfig scrollEase scrollMilliseconds)
                                    viewport.height
                            )
            in
            ( model
            , Task.perform (always Ignored) scrollTask
            )

        ( LandingPageScrollDown, _ ) ->
            ( model, Cmd.none )

        ( ToggleVideoOpenModal newVideoOpenValue, Home _ ) ->
            ( { model | videoModalOpen = newVideoOpenValue }, Cmd.none )

        ( ToggleVideoOpenModal newVideoOpenValue, _ ) ->
            ( model, Cmd.none )

        ( GotHomeMsg pageMsg, Home homeModel ) ->
            Home.update pageMsg homeModel
                |> updatePageModel Home GotHomeMsg model

        ( GotHomeMsg _, _ ) ->
            ( model, Cmd.none )

        ( GotCommitReviewMsg pageMsg, CommitReview commitReviewModel ) ->
            CommitReview.update pageMsg commitReviewModel
                |> updatePageModel CommitReview GotCommitReviewMsg model

        ( GotCommitReviewMsg _, _ ) ->
            ( model, Cmd.none )

        ( GotOAuthRedirectMsg pageMsg, OAuthRedirect oauthRedirectModel ) ->
            let
                ( newOauthRedirectModel, newOauthRedirectMsg ) =
                    OAuthRedirect.update pageMsg oauthRedirectModel
            in
            updatePageModel
                OAuthRedirect
                GotOAuthRedirectMsg
                { model | isLoggingIn = newOauthRedirectModel.isLoggingIn }
                ( newOauthRedirectModel, newOauthRedirectMsg )

        ( GotOAuthRedirectMsg _, _ ) ->
            ( model, Cmd.none )

        ( GotDocumentationMsg pageMsg, Documentation docModel ) ->
            Documentation.update pageMsg docModel
                |> updatePageModel Documentation GotDocumentationMsg model

        ( GotDocumentationMsg _, _ ) ->
            ( model, Cmd.none )

        ( GotRepoMsg pageMsg, Repo repoModel ) ->
            Repo.update pageMsg repoModel
                |> updatePageModel Repo GotRepoMsg model

        ( GotAboutUsMsg pageMsg, AboutUs aboutUsModel ) ->
            AboutUs.update pageMsg aboutUsModel
                |> updatePageModel AboutUs GotAboutUsMsg model

        ( GotAboutUsMsg _, _ ) ->
            ( model, Cmd.none )

        ( GotPricingMsg pageMsg, Pricing pricingModel ) ->
            Pricing.update pageMsg pricingModel
                |> updatePageModel Pricing GotPricingMsg model

        ( GotPricingMsg _, _ ) ->
            ( model, Cmd.none )

        ( GotRepoMsg _, _ ) ->
            ( model, Cmd.none )


{-| For updating the model given a page model and page msg.

This update will close the mobileNavbar.

-}
updatePageModel :
    (pageModel -> PageModel)
    -> (pageMsg -> Msg)
    -> Model
    -> ( pageModel, Cmd pageMsg )
    -> ( Model, Cmd Msg )
updatePageModel toPageModel toMsg model ( pageModel, pageCmd ) =
    ( { model
        | mobileNavbarOpen = False
        , pageModel = toPageModel pageModel
      }
    , Cmd.map toMsg pageCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Ports.onLoadFromLocalStorage OnLoadLocalStorage
        , case model.pageModel of
            NotFound _ ->
                Sub.none

            Redirect _ _ ->
                Sub.none

            Home homeModel ->
                Sub.map GotHomeMsg <| Home.subscriptions homeModel

            OAuthRedirect oauthRedirect ->
                Sub.map GotOAuthRedirectMsg <| OAuthRedirect.subscriptions oauthRedirect

            CommitReview commitReviewModel ->
                Sub.map GotCommitReviewMsg <| CommitReview.subscriptions commitReviewModel

            Documentation _ ->
                Sub.none

            Repo _ ->
                Sub.none

            AboutUs _ ->
                Sub.none

            Pricing _ ->
                Sub.none
        ]



-- MAIN


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
