package rwth.i2.ltl2ba4j.formula.impl;

import rwth.i2.ltl2ba4j.formula.IAnd; 
import rwth.i2.ltl2ba4j.formula.IFormula;

/**
 * @author amy
 *
 * And
 */
public class And extends BinaryFormula implements IAnd {

    /**
     * @param subFormula1 left subformula
     * @param subFormula2 right subformula
     */
    public And(IFormula subFormula1, IFormula subFormula2) {
        super(subFormula1,subFormula2);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String symbol() {
        return "&&";
    }

}
