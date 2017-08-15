module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import Modal


type Msg
    = CloseMe


all : Test
all =
    describe "Modal dialogs"
        [ test "maintainableCssConfig should correctly initialize a configuration" <|
            \() ->
                Expect.equal
                    (Modal.maintainableCssConfig "m" Modal.Top CloseMe)
                    { mainClass = "m m--top"
                    , direction = Modal.Top
                    , openClass = "m-isOpen"
                    , containerClass = "m-container"
                    , backdropClass = "m-backdrop"
                    , closeMsg = CloseMe
                    }
        ]
