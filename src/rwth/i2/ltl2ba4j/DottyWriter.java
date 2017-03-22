package rwth.i2.ltl2ba4j;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import rwth.i2.ltl2ba4j.model.IState;
import rwth.i2.ltl2ba4j.model.ITransition;

public class DottyWriter {

    public static String automatonToDot(Collection<ITransition> automaton) {
        Map<IState,Integer> stateToNumber = new HashMap<IState,Integer>();
        int i=0;
        for(ITransition t: automaton) {
            IState state = t.getSourceState();
            if(!stateToNumber.containsKey(state)) {
                stateToNumber.put(state,i++);
            }
            state = t.getTargetState();
            if(!stateToNumber.containsKey(state)) {
                stateToNumber.put(state,i++);
            }
        }

        String dot = "";
        dot += "digraph G {\n" + 
               "size =\"4,4\";\n";

        for(IState state: stateToNumber.keySet()) {
            boolean isInitial = false, isFinal = false;
            if(state.isInitial()) {
                isInitial = true;
            }
            if(state.isFinal()) {
                isFinal = true;
            }
            if(isInitial||isFinal) {
                dot += "s" + stateToNumber.get(state)
                + " ["
                + (isInitial?"shape=box":"")
                + ((isInitial&&isFinal)?",":"")
                + (isFinal?"style=dotted":"")
                + "];\n";                
            }
        }
        
        for(ITransition transition: automaton) {
            dot += "s" + stateToNumber.get(transition.getSourceState())
                 + " -> "
                 + "s" + stateToNumber.get(transition.getTargetState())
                 + " [label=\""
                 + transition.getLabels()
                 + "\"]"
                 + ";\n";
        }
        dot +="}\n";
        return dot;        
    }
    
}
