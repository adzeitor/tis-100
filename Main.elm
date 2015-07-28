-- TODO:
--  - make synchro impulse (time to execute one step 1ms or 10ms), timer
--      one block with loop and another in wait state

import Json.Encode exposing (string)
import Array exposing (Array)
import Dict exposing (Dict)
import StartApp
import Maybe
import Parser exposing (..)
import View exposing (..)
import Common exposing (..)
import String
import Debug
import Html exposing (Html)
import Signal exposing (Address)
import Time

isLabel x =
  case x of
    Label _ -> True
    ILabel _ _ -> True
    _ -> False

getLabel x =
  case x of
    Label x -> x
    ILabel x _ -> x
    _ -> ""

loadInstructions : List DebugInstr -> BlockState
loadInstructions list'  =
  let list = List.filter (not << (==) NewLine << .instr) list'
      go x (pos, mem, labels) =
        if isLabel x.instr
        then (pos  , mem     , labels++[(getLabel x.instr, pos)])
        else (pos+1, mem++[x], labels)
      (_,mem,labels) =  List.foldl go (0,[],[]) list
  in { emptyBlock | instrs <- Array.fromList mem
                  , labels <- Dict.fromList labels
     }

-- loadInstructions list block =
--   { emptyBlock | instrs <- Array.fromList list
--                , labels <- Dict.fromList
--                            <| List.map(\(x,y) -> (getLabel y, x))
--                            <| List.filter(\x -> isLabel (snd x))
--                            <| Array.toIndexedList
--                            <| Array.fromList list
--   }




eval s =
  case Array.get s.cur s.instrs of
    Nothing -> s
    Just x  -> evalInstr x.instr s

evalInstr : Instr -> BlockState -> BlockState
evalInstr x s =
  case x of
    MOVINT val Acc        -> nextInstruction {s | acc <- val}
    MOVINT val (Port dir) -> setPortOrWait val dir s
    MOV src dest -> evalMOV src dest s
    ADD src      -> evalADD src      s
    ADDINT     n -> nextInstruction { s | acc <- s.acc + n}
    SUB src      -> evalADD src      s
    SUBINT     n -> nextInstruction { s | acc <- s.acc - n}
    SWP          -> nextInstruction {s | acc <- s.bak
                                       , bak <- s.acc
                                    }
    SAV          -> nextInstruction {s | bak <- s.acc
                                    }
    NEG          -> nextInstruction {s | acc <- negate s.acc}
    JMP label    -> jumpToLabel label s
    JEZ label    -> conditionalJump (==) label s
    JNZ label    -> conditionalJump (/=) label s
    JGZ label    -> conditionalJump (>)  label s
    ILabel _ l   -> evalInstr l s
    Label _      -> nextInstruction s
    _ ->            nextInstruction s


evalADD src s =
  case src of
    Port dir -> getPortOrWait (\value s -> {s | acc <- s.acc + value}) dir s
    Acc      -> nextInstruction {s | acc <- s.acc + s.acc}

evalMOV src dest s =
  case dest of
    -- TODO: PORT TO PORT (case (src,dest)
    Port dir  -> setPortOrWait s.acc dir s
    Acc       -> case src of
                   Port dir -> getPortOrWait (\value s -> {s | acc <- value}) dir s
                   _        -> s

setPortOrWait value dir s =
  if s.outPort == Nothing
  then {s | outPort <- Just (dir, value)
          , mode    <- PortWait}
  else {s | mode    <- PortWait}


getPortOrWait func dir s =
  let wait = { s | waitPort <- Just dir
                 , mode     <- PortWait}
  in case s.inPort of
    Just (from,val) -> if dir ==  from
                       then nextInstruction (func val { s | inPort <- Nothing})
                       else wait
    Nothing  -> wait


conditionalJump cond label s =
  if cond s.acc 0
  then jumpToLabel label s
  else nextInstruction   s

jumpToLabel label s =
  Maybe.withDefault (nextInstruction s) (Maybe.map (\pos -> { s | cur <- pos}) (Dict.get label s.labels))

nextInstruction s =
  { s | cur <- if s.cur+1 >=  Array.length s.instrs
               then 0
               else s.cur+1
  }



resetBlockState s = {s | acc <- 0
                    , cur <- 0
                    , bak <- 0
                    , inPort <- Nothing
                    , outPort <- Nothing
                    , waitPort <- Nothing}

type alias Model = { isRunning   : Bool
                   , isPaused    : Bool
                   , blocks      : Array BlockState
                   , sources     : Array (List String)
                   , connections : List Connection
                   , positions : List (List Int)
                   }


parseAndLoad lines =
  let addPosition n x = DebugInstr x n
      parsed = List.foldl (\x z -> x `Maybe.andThen` (\val -> Maybe.map (\r -> r++[val]) z ) )
                 (Just []) <| List.indexedMap (\n x -> Maybe.map (addPosition n) <| parseWithLabels x) lines
  in case parsed of
       Just list -> Just (loadInstructions list)
       Nothing   -> Nothing


concatMaybe : List (Maybe a) -> List a
concatMaybe list = List.foldr (\x z -> Maybe.withDefault [] (Maybe.map (\a -> [a]) x) ++ z) [] list

--test1 : BlockState
model : Model
model =  let prog1 = "mov 1, acc\nswp\nmov 0,acc\n\nloop:\n mov acc,right\n swp\n add right\n jmp loop\n"
             prog2 = "mov left,acc\nmov acc,left\nmov 1,right\n"
             prog3 = "add left\n"
             prog4 = "mov up, acc\nadd left\nmov acc,left\n"
             progs = List.map String.lines [prog1,prog2,prog3]
         in { isRunning = False
            , isPaused  = True
            , blocks = (Array.fromList (concatMaybe <| List.map parseAndLoad progs))
            , sources = Array.fromList progs
            , connections = [ Connection 0 1 Right
                            , Connection 1 0 Left
                            , Connection 1 2 Right
                            , Connection 2 1 Left
                            ]
            , positions = [ [0, 1]
                          , [2, 3]]
            }



transferPort : BlockState  -- ^ from
             -> BlockState -- ^ to
             -> Direction  -- ^ direction
             -> (BlockState,BlockState)
transferPort blockA blockB dir =
  let noAction = (blockA,blockB)
  in case (blockA.outPort,blockB.waitPort) of
      (Just (fromDir,value), Just toDir) ->
          if fromDir == inverseDirection toDir
               && blockA.mode == PortWait
               && blockB.mode == PortWait
          then (  nextInstruction { blockA  | outPort <- Nothing}
               ,   eval { blockB | waitPort <- Nothing
                                 , inPort   <- Just (toDir, value)})
          else noAction
      _                    -> noAction

updateConnections :  List Connection -> Array BlockState -> Array BlockState
updateConnections connections blocks =
  let go c bs = case (Array.get c.from bs, Array.get c.to bs) of
                  (Just a, Just b) -> let (a',b') = transferPort a b c.direction
                                      in Array.set c.to b' <| Array.set c.from a' bs
                  _                -> bs
  in List.foldl go blocks connections


update : Action -> Model -> Model
update action model =
  let next = let blocks' =  Array.map eval (updateConnections model.connections model.blocks)
             in {model | blocks <- if model.isRunning == False
                                   then model.blocks
                                   else  blocks'
                , isRunning <- True}
  in case action of
    Step -> { next | isPaused <- True }
    UpdateSource idx str -> let lines = String.lines str
                            in case parseAndLoad lines of
                              Just u -> {model | blocks <- Array.set idx u model.blocks
                                               , sources <- Array.set idx lines model.sources}
                              _      -> { model | sources <- Array.set idx lines  model.sources}
    Stop -> { model | isRunning <- False
                    , blocks    <- Array.map resetBlockState model.blocks
                    , isPaused  <- True
            }
    ActionRun -> { model | isRunning <- True
                   , isPaused  <- False
           }
    Timed -> if model.isRunning && not model.isPaused
             then next
             else model
    _ -> model


main =
  start { model = model, view = view, update = update }



--start : StartApp.App model action -> Signal Html
start app =
  let
    actions =
      Signal.mailbox Nothing

    address =
      Signal.forwardTo actions.address Just

    model =
      Signal.foldp
        (\(Just action) model -> app.update action model)
        app.model
        (Signal.merge actions.signal (Signal.map (\_ -> Just Timed) (Time.fps 10)))
  in
    Signal.map (app.view address) model
