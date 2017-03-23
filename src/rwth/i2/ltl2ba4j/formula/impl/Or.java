package rwth.i2.ltl2ba4j.formula.impl;

import rwth.i2.ltl2ba4j.formula.IFormula;
import rwth.i2.ltl2ba4j.formula.IOr;

/**
 * @author amy
 *
 * And
 */
public class Or extends BinaryFormula implements IOr {

    /**
     * @param subFormula1 left subformula
     * @param subFormula2 right subformula
     */
    public Or(IFormula subFormula1, IFormula subFormula2) {
        super(subFormula1,subFormula2);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String symbol() {
        return "||";
    } 
    
}
