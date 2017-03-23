package rwth.i2.ltl2ba4j.model;

import java.util.Set;

import rwth.i2.ltl2ba4j.model.IGraphProposition;

/**
 * @author amy
 *
 * IState
 */
public interface ITransition {

    /**
     * @return the labels attached to this transition
     */
    public Set<IGraphProposition> getLabels();
    
    /**
     * @return the source state of this transition
     */
    public IState getSourceState();
    
    /**
     * @return the target state of this transition
     */
    public IState getTargetState();
    
    /**
     * Clients should implement this method, since the
     * implementation uses HashSets, which require a proper
     * notion of equality.
     * @return True iff label and associated states are equal
     * @see java.lang.Object#equals(java.lang.Object)
     */
    public boolean equals(Object obj);
    
    /**
     * Clients should implement this method, since the
     * implementation uses HashSets, which require a proper
     * hash code.
     * @see java.lang.Object#hashCode()
     */
    public int hashCode();    
}
