package rwth.i2.ltl2ba4j.formula;

/**
 * @author amy
 *
 * IBinaryFormula
 */
public interface IUnaryFormula extends IFormula {

    /**
     * @return the subformula or proposition
     */
    public IFormula getSubformula();

}
