module Common where

import Array exposing (Array)
import Dict  exposing (Dict)

type alias DebugInstr = { instr: Instr
                        , line : Int
                        }

type Instr = MOV Source Source
           -- for type safing (this is mov)
           | MOVINT Int Source
           | NEG
           | ADD Source
           | ADDINT Int
           | SUB Source
           | SUBINT Int
           | SAV
           | SWP
           | JMP String
           -- conditional jumps
           | JGZ String
           | JLZ String
           | JEZ String
           | JNZ String
           | ILabel String Instr
           | Label String
           | NewLine

type Direction = Up | Down | Right | Left | Any

type Source    = Port Direction
               | Acc

--type SignalValue = SignalValue Direction Int

type alias BlockState =
  { inPort    : Maybe (Direction, Int)
  , outPort   : Maybe (Direction, Int)
  , waitPort  : Maybe Direction
  , acc    : Int
  , bak    : Int
  , instrs : Array DebugInstr
  , source : List String
  -- ^ instruction pointers to all labels
  , labels : Dict String Int
  -- ^ current instruction pointer
  , cur    : Int
  , mode   : Mode
  }


type Mode =  Run | PortWait

emptyBlock : BlockState
emptyBlock =
  { inPort   = Nothing
  , waitPort = Nothing
  , outPort  = Nothing
  , acc = 0
  , bak = 0
  , instrs = Array.empty
  , labels = Dict.empty
  , cur    = 0
  , source = []
  , mode   = Run
  }


type alias Connection = { from      : Int
                        , to        : Int
                        , direction : Direction
                        }



inverseDirection : Direction -> Direction
inverseDirection dir =
  case dir of
    Right -> Left
    Left  -> Right
    Up    -> Down
    Down  -> Up
    Any   -> Any

getSourcePos s =
  case Array.get s.cur s.instrs of
    Just x  -> x.line
    Nothing -> 0
