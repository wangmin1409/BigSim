package rwth.i2.ltl2ba4j.formula.impl;


import rwth.i2.ltl2ba4j.formula.IBinaryFormula;
import rwth.i2.ltl2ba4j.formula.IFormula;

public abstract class BinaryFormula implements IBinaryFormula {

    protected IFormula subformula1, subformula2; 
    
    /**
     * @param subformula1 left subformula
     * @param subformula2 right subformula
     */
    public BinaryFormula(IFormula subformula1, IFormula subformula2) {
        this.subformula1 = subformula1;
        this.subformula2 = subformula2;
    }

    /**
     * {@inheritDoc}
     */
    public IFormula getSubformula1() {
        return subformula1;
    }

    /**
     * {@inheritDoc}
     */
    public IFormula getSubformula2() {
        return subformula2;
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
    
        BinaryFormula other = (BinaryFormula) oth;
        if (this.subformula1 == null) {
            if (other.subformula1 != null) {
                return false;
            }
        } else {
            if (!this.subformula1.equals(other.subformula1)) {
                return false;
            }
        }
        if (this.subformula2 == null) {
            if (other.subformula2 != null) {
                return false;
            }
        } else {
            if (!this.subformula2.equals(other.subformula2)) {
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
        if (subformula1 != null) {
            result = PRIME * result + subformula1.hashCode();
        }
        if (subformula2 != null) {
            result = PRIME * result + subformula2.hashCode();
        }
    
        return result;
    }
    
    /**
     * {@inheritDoc}
     */
    public String toString() {
        return "(" +
            this.subformula1.toString() +
            ") " + 
            this.symbol() +
            " (" +             
            this.subformula2.toString() +
            ")";
    }

    /**
     * @return the symbol for this formula constructor
     */
    abstract protected String symbol();
}
