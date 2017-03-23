package rwth.i2.ltl2ba4j.formula.impl;

import rwth.i2.ltl2ba4j.formula.IFormula;
import rwth.i2.ltl2ba4j.formula.IRelease;

/**
 * @author amy
 *
 * And
 */
public class Release extends BinaryFormula implements IRelease {

    /**
     * @param subFormula1 left subformula
     * @param subFormula2 right subformula
     */
    public Release(IFormula subFormula1, IFormula subFormula2) {
        super(subFormula1,subFormula2);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String symbol() {
        return "R";
    } 
    
}
