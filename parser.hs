module Parser where
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.Text.Lazy
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

textToGraph :: Data.Text.Lazy.Text -> AutoMata
textToGraph txt = parseDotGraph txt

findLasso :: AutoMata -> [LabelType]
findLasso automata = Prelude.map nodeID $ nextStates ! Prelude.head states -- Stub
    where
      nextStates :: Map State [State]
      nextStates = fromList nextStatesList
      nextStatesList = [(from,[to |
                               to<-states,
                               t<-transitions,
                               fromNode t == nodeID from,
                               nodeID to == toNode t])|
                        from <- states]
      acceptingStates = Prelude.filter isAccepting states
      states = graphNodes automata
      transitions = graphEdges automata

finite_state_machine = "\
\digraph finite_state_machine {\
\    rankdir=LR;\
\    size=\"8,5\"\
\    LR_0 [shape = doublecircle];\
\    LR_3 [shape = doublecircle];\
\    LR_4 [shape = doublecircle];\
\    LR_8 [shape = doublecircle];\
\    LR_0 -> LR_2 [ label = \"SS(B)\" ];\
\    LR_0 -> LR_1 [ label = \"SS(S)\" ];\
\    LR_1 -> LR_3 [ label = \"S($end)\" ];\
\    LR_2 -> LR_6 [ label = \"SS(b)\" ];\
\    LR_2 -> LR_5 [ label = \"SS(a)\" ];\
\    LR_2 -> LR_4 [ label = \"S(A)\" ];\
\    LR_5 -> LR_7 [ label = \"S(b)\" ];\
\    LR_5 -> LR_5 [ label = \"S(a)\" ];\
\    LR_6 -> LR_6 [ label = \"S(b)\" ];\
\    LR_6 -> LR_5 [ label = \"S(a)\" ];\
\    LR_7 -> LR_8 [ label = \"S(b)\" ];\
\    LR_7 -> LR_5 [ label = \"S(a)\" ];\
\    LR_8 -> LR_6 [ label = \"S(b)\" ];\
\    LR_8 -> LR_5 [ label = \"S(a)\" ];\
\}\
\"
