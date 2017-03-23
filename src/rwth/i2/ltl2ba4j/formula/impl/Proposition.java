package rwth.i2.ltl2ba4j.formula.impl;

import rwth.i2.ltl2ba4j.formula.IProposition;

/**
 * @author amy
 * 
 * And
 */
public class Proposition implements IProposition {

    protected String label;

    public Proposition(boolean bool)
            throws IllegalArgumentException {
        this(bool ? "true" : "false");
    }

    public Proposition(String label)
            throws IllegalArgumentException {
        this.label = label;
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
    public String toString() {
        return this.getLabel();
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

        Proposition other = (Proposition) oth;
        if (this.label == null) {
            if (other.label != null) {
                return false;
            }
        } else {
            if (!this.label.equals(other.label)) {
                return false;
            }
        }

        return true;
    }

}
