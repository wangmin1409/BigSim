package rwth.i2.ltl2ba4j.model;

import java.util.Set;

import rwth.i2.ltl2ba4j.model.IGraphProposition;

/**
 * @author amy
 *
 * Factory to instantiate graph components of the
 * finite state machine.
 */
public interface IGraphFactory {

    /**
     * Constructs a state with the given label. 
     * @param label label of the state
     * @param isInitial <code>true</code> if the state is the initial state
     * @param isFinal <code>true</code> if the state is final
     * @return the state
     */
    public IState State(String label, boolean isInitial, boolean isFinal);
    
    /**
     * A transition with the given label, source and target state
     * @param labels label
     * @param sourceState source state of the transition
     * @param targetState target state of the transition
     * @return the transition
     */
    public ITransition Transition(Set<IGraphProposition> labels, IState sourceState, IState targetState);
    
    /**
     * A proposition with the given label. It might be negated.
     * @param label label
     * @param isNegated it <code>true</code>, the proposition is negated
     * @return the proposition
     */
    public IGraphProposition Proposition(String label, boolean isNegated);

    /**
     * The unique SIGMA proposition, meaning <i>any symbol</i>.
     * @return the unique SIGMA proposition
     */
    public IGraphProposition SigmaProposition();
}
