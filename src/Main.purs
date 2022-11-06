module Main where

import Prelude

import Data.Foldable (oneOf)
import Data.Tuple.Nested ((/\))
import Deku.Attribute ((!:=), (:=))
import Deku.Control (text_)
import Deku.DOM as D
import Deku.Do (useState')
import Deku.Do as Deku
import Deku.Listeners (click_)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import HalogenParser as HalogenParser
import Web.HTML.HTMLTextAreaElement (value)

main :: Effect Unit
main = runInBody Deku.do
  doCompile /\ compile <- useState'
  setPurs /\ purs <- useState'
  D.div (oneOf [ pure (D.Class := "w-full"), click_ (doCompile unit) ])
    [ D.span (oneOf [ D.Class !:= "text-xl" ]) [ text_ "html2deku" ]
    , D.button
        ( oneOf
            [ D.Class !:=
                "ml-2 inline-flex items-center rounded border border-transparent bg-indigo-600 px-2.5 py-1.5 text-xs font-medium text-white shadow-sm hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:ring-offset-2"
            ]
        )
        [ text_ "Convert >" ]
    , D.div
        (oneOf [ D.Class !:= "w-full grid grid-cols-1 md:grid-cols-2 gap-4" ])
        [ D.textarea
            ( oneOf
                [ D.Rows !:= "20"
                , D.Class !:= "border-2"
                , compile <#> \_ -> D.SelfT := \i -> do
                  v <- value i
                  setPurs (show (HalogenParser.parse v))
                ]
            )
            []
        , D.textarea
            ( oneOf
                [ D.Rows !:= "20"
                , D.Class !:= "border-2"
                , purs <#> (D.Value := _)
                ]
            )
            []
        ]
    ]
