package rwth.i2.ltl2ba4j.model.impl;

import rwth.i2.ltl2ba4j.model.IState;

/**
 * @author amy
 *
 * State
 */
public class State implements IState {

    protected String label;
    
    protected boolean isInitial, isFinal;
    
    /**
     * @param label
     * @param isInitial
     * @param isFinal
     */
    public State(String label, boolean isInitial, boolean isFinal) {
        super();
        this.label = label;
        this.isInitial = isInitial;
        this.isFinal = isFinal;
    }
    
    /**
     * {@inheritDoc}
     */
    public String getLabel() {
        return label;
    }
 
    public boolean isFinal() {
        return isFinal;
    }
    
    public boolean isInitial() {
        return isInitial;
    }
    
    /**
     * {@inheritDoc}
     */
    public String toString() {
        return "<" +
        this.label +
        (this.isFinal()?"(final)":"") + 
        (this.isInitial()?"(initial)":"") + 
        ">";
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
        result = PRIME * result + (isInitial ? 1 : 0);
        result = PRIME * result + (isFinal ? 1 : 0);

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

        State other = (State) oth;
        if (this.label == null) {
            if (other.label != null) {
                return false;
            }
        } else {
            if (!this.label.equals(other.label)) {
                return false;
            }
        }

        if (this.isInitial != other.isInitial) {
            return false;
        }

        if (this.isFinal != other.isFinal) {
            return false;
        }

        return true;
    }
}
