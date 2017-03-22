package rwth.i2.ltl2ba4j.formula.impl;

import rwth.i2.ltl2ba4j.formula.IFormula;
import rwth.i2.ltl2ba4j.formula.IUnaryFormula;

public abstract class UnaryFormula implements IUnaryFormula {

    protected IFormula subformula; 
    
    /**
     * @param subformula the subformula
     */
    public UnaryFormula(IFormula subformula) {
        this.subformula = subformula;
    }

    /**
     * {@inheritDoc}
     */
    public IFormula getSubformula() {
        return subformula;
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
    
        UnaryFormula other = (UnaryFormula) oth;
        if (this.subformula == null) {
            if (other.subformula != null) {
                return false;
            }
        } else {
            if (!this.subformula.equals(other.subformula)) {
                return false;
            }
        }
    
        return true;
    }

    /**
     * {@inheritDoc}
     */
    public int hashCode() {
        final int PRIME = 1000003;
        int result = 0;
        if (subformula != null) {
            result = PRIME * result + subformula.hashCode();
        }
    
        return result;
    }
    
    /**
     * {@inheritDoc}
     */
    public String toString() {
        return this.symbol() +
            "(" +
            this.subformula.toString() +
            ") ";
    }

    /**
     * @return the symbol for this formula constructor
     */
    abstract protected String symbol();


}
