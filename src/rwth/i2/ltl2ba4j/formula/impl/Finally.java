package rwth.i2.ltl2ba4j.formula.impl;

import rwth.i2.ltl2ba4j.formula.IFinally;
import rwth.i2.ltl2ba4j.formula.IFormula;

/**
 * @author amy
 *
 * And
 */
public class Finally extends UnaryFormula implements IFinally {

    /**
     * @param subFormula the subformula
     */
    public Finally(IFormula subFormula) {
        super(subFormula);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String symbol() {
        return "F";
    }
    
}
