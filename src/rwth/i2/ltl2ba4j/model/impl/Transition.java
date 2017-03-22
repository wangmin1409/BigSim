package rwth.i2.ltl2ba4j.model.impl;

import java.util.Set;

import rwth.i2.ltl2ba4j.model.IGraphProposition;
import rwth.i2.ltl2ba4j.model.IState;
import rwth.i2.ltl2ba4j.model.ITransition;

/**
 * @author amy
 *
 * Transition
 */
public class Transition implements ITransition {

    protected Set<IGraphProposition> labels;
    
    protected IState sourceState;
    
    protected IState targetState;    
    
    /**
     * @param labels labels 
     * @param sourceState source state
     * @param targetState target state
     */
    public Transition(Set<IGraphProposition> labels, IState sourceState, IState targetState) {
        super();
        this.labels = labels;
        this.sourceState = sourceState;
        this.targetState = targetState;
    }
    
    /**
     * {@inheritDoc}
     */
    public Set<IGraphProposition> getLabels() {
        return labels;
    }

    /**
     * {@inheritDoc}
     */
    public IState getSourceState() {
        return sourceState;
    }

    /**
     * {@inheritDoc}
     */
    public IState getTargetState() {
        return targetState;
    }

    /**
     * {@inheritDoc}
     */
    public int hashCode() {
        final int PRIME = 1000003;
        int result = 0;
        if (labels != null) {
            result = PRIME * result + labels.hashCode();
        }
        if (sourceState != null) {
            result = PRIME * result + sourceState.hashCode();
        }
        if (targetState != null) {
            result = PRIME * result + targetState.hashCode();
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

        Transition other = (Transition) oth;
        if (this.labels == null) {
            if (other.labels != null) {
                return false;
            }
        } else {
            if (!this.labels.equals(other.labels)) {
                return false;
            }
        }
        if (this.sourceState == null) {
            if (other.sourceState != null) {
                return false;
            }
        } else {
            if (!this.sourceState.equals(other.sourceState)) {
                return false;
            }
        }
        if (this.targetState == null) {
            if (other.targetState != null) {
                return false;
            }
        } else {
            if (!this.targetState.equals(other.targetState)) {
                return false;
            }
        }

        return true;
    }
    
    /**
     * {@inheritDoc}
     */
    public String toString() {
        return sourceState + " --" + labels + "--> " + targetState;
    }
}
