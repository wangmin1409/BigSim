package rwth.i2.ltl2ba4j.formula;

/**
 * @author amy
 *
 * IBinaryFormula
 */
public interface IBinaryFormula extends IFormula {

    /**
     * @return the left subformula or proposition
     */
    public IFormula getSubformula1();

    /**
     * @return the right subformula or proposition
     */
    public IFormula getSubformula2();

}
