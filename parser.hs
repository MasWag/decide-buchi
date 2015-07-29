import qualified Data.List as L
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Text.Lazy
import qualified Data.Text.Lazy.IO as TI
import Data.Map.Strict

type LabelType = String
type State = DotNode LabelType
type Transition = DotEdge LabelType
type AutoMata = DotGraph LabelType

acceptingAttribute :: Attribute
acceptingAttribute = shape DoubleCircle

isAccepting :: State -> Bool
isAccepting node = or $ Prelude.map (sameAttribute acceptingAttribute) attributes
    where
      attributes = nodeAttributes node 

isStart :: State -> Bool
isStart node = nodeID node == "start"
           
textToGraph :: Data.Text.Lazy.Text -> AutoMata
textToGraph txt = parseDotGraph txt

findLasso :: AutoMata -> [LabelType]
findLasso automata = Prelude.map nodeID $ L.foldl' (\ li root -> if li == [] then 
                                                                     searchLasso root root []
                                                                 else li) 
                     [] acceptingStates
    where
      -- Depth First Search
      searchLasso :: State -> State -> [State] -> [State]
      searchLasso root now hist 
          | root == now && hist /= [] =  L.reverse $ now:hist
          | elem now hist  =  []
          | otherwise      = L.foldl' (\ l s -> 
                                           if l /= [] then
                                               l
                                           else
                                               searchLasso root s (now:hist)
                                      ) [] $ nextStates ! now
      traceStates :: [State] -> State -> [State]
      traceStates hist now 
          | searchNext == [] = hist
          | otherwise = L.foldl' traceStates (now:hist) searchNext
          where
            nexts = nextStates ! now
            searchNext = Prelude.filter (\ s -> notElem s hist) nexts
      nextStates :: Map State [State]
      nextStates = fromList nextStatesList
      nextStatesList = [(from,[to |
                               to<-states,
                               t<-transitions,
                               fromNode t == nodeID from,
                               nodeID to == toNode t])|
                        from <- states]
      acceptingStates = Prelude.filter isAccepting reachableStates
      startState = L.head $ Prelude.filter isStart states
      reachableStates = traceStates [startState] startState
      states = graphNodes automata
      transitions = graphEdges automata

main :: IO ()
main = do
    lasso <- findLasso <$> textToGraph  <$> TI.getContents
    if lasso == [] then
        putStrLn "There is no accepting path."
    else
        putStrLn $ "Found accepting path:" ++ (show lasso) 
