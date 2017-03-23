package rwth.i2.ltl2ba4j.model;

/**
 * @author amy
 *
 * Represents a proposition attached to a transition. 
 */
public interface IGraphProposition {

    /**
     * @return the pure label, regardless of the negation 
     */
    public String getLabel();
    
    /**
     * @return the label, taking negation with into account
     */
    public String getFullLabel();
    
    /**
     * @return <code>true</code> iff the proposition is negated
     */
    public boolean isNegated();
	
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
