package rwth.i2.ltl2ba4j.model.impl;

import java.util.Set;

import rwth.i2.ltl2ba4j.model.IGraphProposition;
import rwth.i2.ltl2ba4j.model.IState;
import rwth.i2.ltl2ba4j.model.IGraphFactory;
import rwth.i2.ltl2ba4j.model.ITransition;

/**
 * @author amy
 *
 * GraphFactory
 */
public class GraphFactory implements IGraphFactory {

    /** the unique proposition meaning <i>any letter</i> */ 
    private static final IGraphProposition SIGMA_PROPOSITION = new SigmaProposition();

    /**
     * {@inheritDoc}
     */
    public IState State(String label, boolean isInitial, boolean isFinal) {
        return new State(label, isInitial, isFinal);
    }

    /**
     * {@inheritDoc}
     */
    public ITransition Transition(Set<IGraphProposition> labels, IState sourceState,
            IState targetState) {
        return new Transition(labels, sourceState, targetState);
    }
    
    /**
     * {@inheritDoc}
     */
    public IGraphProposition Proposition(String label, boolean isNegated) {
        return new GraphProposition(label, isNegated);
    }

    /**
     * {@inheritDoc}
     */
    public IGraphProposition SigmaProposition() {
        return SIGMA_PROPOSITION;
    }

}
