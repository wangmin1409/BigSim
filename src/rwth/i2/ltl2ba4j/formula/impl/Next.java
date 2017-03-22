package rwth.i2.ltl2ba4j.formula.impl;

import rwth.i2.ltl2ba4j.formula.IFormula;
import rwth.i2.ltl2ba4j.formula.INext;

/**
 * @author amy
 *
 * And
 */
public class Next extends UnaryFormula implements INext {

    /**
     * @param subFormula the subformula
     */
    public Next(IFormula subFormula) {
        super(subFormula);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String symbol() {
        return "X";
    }
}
