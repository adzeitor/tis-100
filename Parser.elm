module Parser where

import Html exposing (..)
import Common exposing (..)
import String
import Char
import Result
import Maybe

trimSplit s = List.filter (not << String.isEmpty )
           << List.map String.trim
           << String.split s



-- FIXME: get line numbers here
parseWithLabels line =
  case String.indexes ":" line of
    [n] -> case trimSplit ":" line of
             [label] -> Just (Label (String.toUpper label))
             [label, instrs] -> Maybe.map (ILabel (String.toUpper label)) (parseInstr instrs)
    _ -> parseInstr line



parseInstr line =
  case trimSplit " " <| String.toUpper line of
    "ADD"::xs -> parseADD xs
    "SUB"::xs -> parseSUB xs
    "MOV"::xs -> parseMOV (String.concat xs)
    "SWP"::_  -> Just SWP
    "SAV"::_  -> Just SAV
    ["JMP",label] -> Just (JMP label)
    ["JNZ",label] -> Just (JNZ label)
    ["JEZ",label] -> Just (JEZ label)
    ["JGZ",label] -> Just (JGZ label)
    ["JLZ",label] -> Just (JLZ label)
    []            -> Just NewLine
    _ -> Nothing

parseADD list =
  case list of
    [x] -> if String.all Char.isDigit x
           then Result.toMaybe (String.toInt x) |> Maybe.map ADDINT
           else Maybe.map ADD  (parseSource x)
    _ -> Nothing

parseSUB list =
  case list of
    [x] -> if String.all Char.isDigit x
           then Result.toMaybe (String.toInt x) |> Maybe.map SUBINT
           else Maybe.map SUB  (parseSource x)
    _ -> Nothing

parseSource x =
  case x of
    "UP"   -> Just (Port Up)
    "DOWN" -> Just (Port Down)
    "RIGHT"-> Just (Port Right)
    "LEFT" -> Just (Port Left)
    "ACC"  -> Just Acc
    _      -> Nothing

parseMOV line =
  case List.map String.trim <| String.split "," line of
    [arg1, arg2] -> if String.all Char.isDigit arg1
                    then Result.toMaybe (String.toInt arg1) `Maybe.andThen`  (\x -> Maybe.map (MOVINT x) (parseSource arg2) )
                    else  (parseSource arg1) `Maybe.andThen` (\x -> Maybe.map (MOV x) (parseSource arg2))
    _            -> Nothing

test1 = List.map parseWithLabels [ "SAV"
                                 , "ADD 2"
                                 , "ADD STR"
                                 , " SWP"
                                 , "add acc"
                                 , "mov 5, acc"
                                 , "mov left, acc"
                                 , "sub 5"
                                 , "sub acc"
                                 , "jmp test"
                                 , "jmp    loop1   "
                                 , "   label   :   "
                                 , "  loop_label  :  ADD acc  "
                                 , "label2:abwr"
                                 , "label3:"
                                 , "label4:sav"]


main = text (toString test1)
