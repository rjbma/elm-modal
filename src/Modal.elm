module Modal
    exposing
        ( view
        , maybeView
        , Config
        , PullDirection(..)
        , maintainableCssConfig
        )

{-| This library exposes functions for conditionally viewing HTML elements
inside a modal dialog.

Please note that this library only makes sure that all modals have similar
markup with consistent CSS classes; it's still up to you to provide the CSS
to actually make the modal appear and disappear. However, you can use
[this](https://github.com/rjbma/elm-modal/blob/master/src/modal.css) as a starting point!

# View
@docs view
@docs maybeView

# Config
@docs Config
@docs PullDirection
@docs maintainableCssConfig
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


{-| Represents a modal dialog with the given configuration and content. The
   modal's visibility is determined by the `isOpen` flag. The Html attributes
   are applied to the modal's top level element.

For example, a confirmation dialog that depends on a field in the model:

    view : Model -> Html Msg
    view model =
        let
            cfg =
                Modal.maintainableCssConfig "confirmationDialog" Top CancelConfirmation

            confirmationDialog =
                Modal.view cfg model.showConfirmation
        in
            div []
                [ confirmationDialog
                    []
                    [ p [] [ text "Destroy the world?" ]
                    , button [ onClick CallTheNutHaus ] [ text "Sure, we're on borrowed time anyways" ]
                    , button [ onClick CancelConfirmation ] [ text "Not until GoT is over!" ]
                    ]
                ]
-}
view : Config msg -> Bool -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
view cfg isOpen attributes children =
    let
        isOpenClass =
            if isOpen then
                class cfg.openClass
            else
                class ""

        allAttrs =
            class cfg.mainClass
                :: isOpenClass
                :: attributes
    in
        aside allAttrs
            [ div [ class cfg.containerClass ] children
            , div [ class cfg.backdropClass, onClick cfg.closeMsg ] []
            ]


{-| Represents a modal that is visible when the given `Maybe` is a `Just`, and
hidden if it's a `Nothing`. the `childrenFn` function is invoked with the
`Just`'s value, and will determine the modal's content.

For example, a confirmation dialog for viewing detailed information on a user:

    view : Model -> Html Msg
    view model =
        let
            cfg =
                Modal.maintainableCssConfig "viewUserThingy" Bottom CloseUserDetail

            userDetailDialog =
                Modal.maybeView cfg model.selectedUser

            viewUser user =
                [ dl []
                    [ dt [] [ text "Name" ]
                    , dd [] [ text user.name ]
                    , dt [] [ text "Last login" ]
                    , dd [] [ text user.lastLoginDate ]
                    ]
                ]
        in
            div []
                [ userDetailDialog
                    []
                    viewUser
                ]
-}
maybeView : Config msg -> Maybe a -> List (Html.Attribute msg) -> (a -> List (Html msg)) -> Html msg
maybeView cfg val attrs childrenFn =
    case val of
        Nothing ->
            view cfg False attrs [ text "" ]

        Just a ->
            view cfg True attrs (childrenFn a)


{-| The direction from where the modal dialog will be transitioned. Notice
that this just puts a particular CSS class in the main HTML element so
that it can be easily targeted.
-}
type PullDirection
    = Top
    | Right
    | Bottom
    | Left


{-| Particular configuration for the modal dialog, including the CSS classes,
direction and the message triggrered when closing the modal.
The CSS classes are applied as follows:

    <div class="mainClass openClass mainClass+direction">
        <div class="containerClass">
            <!-- stuff goes here -->
        </div>
        <div class="backdropClass"></div>
    </div>

-}
type alias Config msg =
    { mainClass : String
    , openClass : String
    , containerClass : String
    , backdropClass : String
    , direction : PullDirection
    , closeMsg : msg
    }


{-| Generates a configuration function whose CSS classes are named according
to [Maintainable CSS](https://maintainablecss.com/).
-}
maintainableCssConfig : String -> PullDirection -> msg -> Config msg
maintainableCssConfig moduleName pullDirection closeMsg =
    let
        direction =
            case pullDirection of
                Top ->
                    "--top"

                Right ->
                    "--right"

                Bottom ->
                    "--bottom"

                Left ->
                    "--left"
    in
        { mainClass = moduleName ++ " " ++ moduleName ++ direction
        , direction = pullDirection
        , openClass = moduleName ++ "-isOpen"
        , containerClass = moduleName ++ "-container"
        , backdropClass = moduleName ++ "-backdrop"
        , closeMsg = closeMsg
        }
