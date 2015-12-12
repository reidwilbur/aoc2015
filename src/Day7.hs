module Day7 (Gate(Wire, Not, And, Or, LShift, RShift), parseGate, initGates, initSignals, runGateSim) where
import Debug.Trace
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Word as Word
import qualified Data.Bits as Bits

type SignalMap = Map.Map String Word.Word16

type Signal = (String, Word.Word16)

data Gate = Not String String |
            And String String String |
            Or String String String |
            LShift String Int String |
            RShift String Int String |
            Wire String String
            deriving (Show, Eq)

initGates :: [String] -> [Gate]
initGates ss = List.foldr (\mg l -> if Maybe.isJust mg then (Maybe.fromJust mg):l else l) [] $ List.map parseGate ss

mapSignal :: SignalMap -> Maybe.Maybe Signal -> SignalMap
mapSignal signals signal = case signal of
                             Just (sig, val) -> Map.insert sig val signals 
                             _ -> signals

initSignals :: [String] -> SignalMap
initSignals gs = let constants = Map.fromList [("1",1),("0",0)] 
                 -- above makes things like "1 AND x -> y" work with my shitty parser
                 in Map.union constants $ List.foldl' mapSignal Map.empty $ List.map parseSignal gs

getSignal :: SignalMap -> Gate -> Maybe Signal
getSignal signals gate = case gate of
                             Not in1 out -> do
                               val1 <- Map.lookup in1 signals
                               Just (out, Bits.complement val1)
                             And in1 in2 out -> do
                               val1 <- Map.lookup in1 signals
                               val2 <- Map.lookup in2 signals
                               Just (out, val1 Bits..&. val2)
                             Or in1 in2 out -> do
                               val1 <- Map.lookup in1 signals
                               val2 <- Map.lookup in2 signals
                               Just (out, val1 Bits..|. val2)
                             LShift in1 sl out -> do
                               val1 <- Map.lookup in1 signals
                               Just (out, Bits.shiftL val1 sl)
                             RShift in1 sr out -> do
                               val1 <- Map.lookup in1 signals
                               Just (out, Bits.shiftR val1 sr)
                             Wire in1 out -> do
                               val1 <- Map.lookup in1 signals
                               Just (out, val1)

applySignals :: SignalMap -> [Gate] -> SignalMap
applySignals sigs gates = let newSigs = List.map (getSignal sigs) gates
                          in List.foldl' (\sigmap ms -> case ms of 
                                                          Just (sig, val) -> Map.insert sig val sigmap
                                                          _ -> sigmap
                                         ) sigs newSigs

runGateSim :: SignalMap -> SignalMap -> [Gate] -> SignalMap
runGateSim lastSigs sigs gates = if lastSigs == sigs then sigs
                                 else let newSigs = applySignals sigs gates 
                                      in runGateSim sigs newSigs gates

parseSignal :: String -> Maybe.Maybe Signal
parseSignal s = case List.words s of
                  val:"->":sig:[] -> Just $ (sig, (read val))
                  _ -> Nothing

parseGate :: String -> Maybe.Maybe Gate
parseGate s = case List.words s of
                "WIRE":in1      :"->":out:[] -> Just $ Wire in1 out
                "NOT":in1       :"->":out:[] -> Just $ Not in1 out
                in1:"AND"   :in2:"->":out:[] -> Just $ And in1 in2 out
                in1:"OR"    :in2:"->":out:[] -> Just $ Or in1 in2 out
                in1:"LSHIFT":in2:"->":out:[] -> Just $ LShift in1 (read in2) out
                in1:"RSHIFT":in2:"->":out:[] -> Just $ RShift in1 (read in2) out
                _ -> Nothing

