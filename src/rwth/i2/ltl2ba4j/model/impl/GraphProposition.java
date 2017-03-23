package rwth.i2.ltl2ba4j.model.impl;

import rwth.i2.ltl2ba4j.model.IGraphProposition;

/**
 * @author amy
 * 
 * And
 */
public class GraphProposition implements IGraphProposition {

    protected String label;

    protected boolean negated;

    public GraphProposition(boolean bool, boolean isNegated)
            throws IllegalArgumentException {
        this(bool ? "true" : "false", isNegated);
    }

    public GraphProposition(String label, boolean isNegated)
            throws IllegalArgumentException {
        // if(!label.matches("[a-z][a-z0-9]++")) {
        // throw new IllegalArgumentException("proposition must be of form
        // [a-z][a-z0-9]++ (got "+label+")");
        // }
        this.label = label;
        this.negated = isNegated;
    }

    /**
     * {@inheritDoc}
     */
    public String getLabel() {
        return this.label;
    }

    /**
     * {@inheritDoc}
     */
    public String getFullLabel() {
        return (this.negated ? "!" : "") + this.getLabel();
    }

    /**
     * {@inheritDoc}
     */
    public String toString() {
        return this.getFullLabel();
    }

    /**
     * {@inheritDoc}
     */
    public boolean isNegated() {
        return negated;
    }

    /**
     * {@inheritDoc}
     */
    public int hashCode() {
        final int PRIME = 1000003;
        int result = 0;
        if (label != null) {
            result = PRIME * result + label.hashCode();
        }
        result = PRIME * result + (negated ? 1 : 0);

        return result;
    }

    /**
     * {@inheritDoc}
     */
    public boolean equals(Object oth) {
        if (this == oth) {
            return true;
        }

        if (oth == null) {
            return false;
        }

        if (oth.getClass() != getClass()) {
            return false;
        }

        GraphProposition other = (GraphProposition) oth;
        if (this.label == null) {
            if (other.label != null) {
                return false;
            }
        } else {
            if (!this.label.equals(other.label)) {
                return false;
            }
        }

        if (this.negated != other.negated) {
            return false;
        }

        return true;
    }

}
