package rwth.i2.ltl2ba4j.model;

/**
 * @author amy
 *
 * IState
 */
public interface IState {

    /**
     * @return Label of this state.
     */
    public String getLabel();
    
    /**
     * @return <code>true</code> iff this state is the initial state
     */
    public boolean isInitial();

    /**
     * @return <code>true</code> iff this state is final
     */
    public boolean isFinal();
    
    /**
     * Clients should implement this method, since the
     * implementation uses HashSets, which require a proper
     * notion of equality.
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
