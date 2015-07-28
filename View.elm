module View where

import Html exposing (..)
import Common exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, on, targetValue)
import Array exposing (Array)
import String

showInstrs : BlockState -> Html
showInstrs s =
  ul [class "instructions"] (List.indexedMap
               (\i x -> li [ style (if i == s.cur then [("color","white"), ("background", "black")] else [])
                           , class (cssClassForInstr x.instr ++  (if i == s.cur then " current" else ""))
                           ] [text (showInstr x.instr), text "|" ,text (toString x.line)]) (Array.toList s.instrs))


showSource: List String -> BlockState-> Html
showSource list s =
  ul [class "instructions"] (List.indexedMap
               (\i x -> pre [ style (if i == getSourcePos s then [("color","white"), ("background", "black")] else [])
                            , class (if i == getSourcePos s then " current" else "")
                           ] [text (if x == "" then " "else x)]) list)

cssClassForInstr x =
  case x of
    JMP    label -> "jump"
    JEZ    label -> "jump"
    JNZ    label -> "jump"
    JGZ    label -> "jump"
    JLZ    label -> "jump"
    Label  _ -> "label"
    ILabel _  _ -> "label"
    _ -> ""


showInstr : Instr -> String
showInstr x =
  case x of
    MOVINT n Acc -> "MOV " ++ toString n ++ ", ACC"
    MOV    Acc (Port dir) -> "MOV ACC, " ++ toString dir
    SWP          -> "SWP"
    ADD    Acc   -> "ADD ACC"
    ADDINT n     -> (if n < 0 then "SUB" else "ADD") ++ " " ++ toString (abs n)
    Label  str   -> str ++ ":"
    ILabel str instr -> str ++ ": " ++ showInstr instr
    JMP    label -> "JMP " ++ label
    JEZ    label -> "JEZ " ++ label
    JNZ    label -> "JNZ " ++ label
    JGZ    label -> "JGZ " ++ label
    JLZ    label -> "JLZ " ++ label
    NewLine      -> " "
    _            -> toString x


showBlock isRunning address idx (src,s) =
  div [ style [("display", "inline-block"), ("width", "200px"), ("vertical-align","top")]
      , class "block"]
      [
        div [class "registers"] [
                 h4 [] [text ("ACC " ++ toString s.acc)]
              , h4 [] [text ("BAK " ++ toString s.bak)]
              , showOutPort s
              , showWaitPort s
              ]

--      , h4 [] [text ("OUT: " ++ toString s.outPort)]
--      , h4 [] [text ("IN : " ++ toString s.inPort)]
--      , h4 [] [text ("WAIT : " ++ toString s.waitPort)]
      , div [style [("display","none")]] [text (toString s.instrs)]
      , div [style [("display","none")]] [text (toString s.labels)]
      , if not isRunning
        then textarea [ rows 10
                      , cols 20
                      , on "input" targetValue (Signal.message address << (UpdateSource idx))] [text (String.join "\n" src)]
        else showSource src s
      ]

showOutPort s =
  let list = case s.outPort of
               Just (dir, value)  ->
                 [ text (toString s.mode)
                 , span [class "arrow"] [text (directionToArrow dir)]
                 , text (toString value)]
               Nothing -> []
  in div [class "out-port"] list

showWaitPort s =
  let list = case s.waitPort of
               Just dir  ->
                 [ text (toString s.mode)
                 , text "?"
                 , span [class "arrow"] [text (directionToArrow dir)]
                 ]
               Nothing -> []
  in div [class "wait-port"] list

directionToArrow dir =
  case dir of
    Up    -> "↑"
    Down  -> "↓"
    Right -> "→"
    Left  -> "←"
    _  -> "ANY"

help = div [class "help"]
       [ code [] [text "MOV 5, ACC"]
       ]

view address model =  div [style [("display", "block")]]
                      [ div [class "controls"]
                              [ button [ onClick address Step ] [ text "Step" ]
                              , button [ onClick address Stop ] [ text "Stop" ]
                              , button [ onClick address ActionRun ] [ text "Run" ]
                              ]
                      , div [] (List.indexedMap (showBlock model.isRunning address)
                                       (List.map2 (,) (Array.toList model.sources)
                                                 (Array.toList model.blocks)))
                      ]


type Action = Step
            | ActionRun
            | Timed
            | Stop
            | UpdateSource Int String
