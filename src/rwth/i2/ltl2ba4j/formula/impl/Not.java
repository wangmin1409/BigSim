package rwth.i2.ltl2ba4j.formula.impl;

import rwth.i2.ltl2ba4j.formula.IFormula;
import rwth.i2.ltl2ba4j.formula.INot;

/**
 * @author amy
 *
 * And
 */
public class Not extends UnaryFormula implements INot {

    /**
     * @param subFormula the subformula
     */
    public Not(IFormula subFormula) {
        super(subFormula);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String symbol() {
        return "!";
    }
}
