package rwth.i2.ltl2ba4j.formula;

/**
 * @author amy
 *
 * IFormulaFactory
 */
public interface IFormulaFactory {
    
    public IAnd And(IFormula formula1, IFormula formula2);

    public IEquivalent Eq(IFormula formula1, IFormula formula2);
    
    public IFinally F(IFormula formula);
    
    public IGlobally G(IFormula formula);

    public IImplies Impl(IFormula formula1, IFormula formula2);

    public INext X(IFormula formula);

    public INot Not(IFormula formula);
    
    public IOr Or(IFormula formula1, IFormula formula2);

    public IProposition Proposition(String label);

    public IRelease Release(IFormula formula1, IFormula formula2);

    public IUntil Until(IFormula formula1, IFormula formula2);

}
