package rwth.i2.ltl2ba4j.formula.impl;

import rwth.i2.ltl2ba4j.formula.IFormula;
import rwth.i2.ltl2ba4j.formula.IGlobally;

/**
 * @author amy
 *
 * And
 */
public class Globally extends UnaryFormula implements IGlobally {

    /**
     * @param subFormula the subformula
     */
    public Globally(IFormula subFormula) {
        super(subFormula);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String symbol() {
        return "G";
    }
}
