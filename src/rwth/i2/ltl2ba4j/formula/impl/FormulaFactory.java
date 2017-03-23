/*
 * Created on 15.02.2005
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package rwth.i2.ltl2ba4j.formula.impl;

import rwth.i2.ltl2ba4j.formula.IAnd;
import rwth.i2.ltl2ba4j.formula.IEquivalent;
import rwth.i2.ltl2ba4j.formula.IFinally;
import rwth.i2.ltl2ba4j.formula.IFormula;
import rwth.i2.ltl2ba4j.formula.IFormulaFactory;
import rwth.i2.ltl2ba4j.formula.IGlobally;
import rwth.i2.ltl2ba4j.formula.IImplies;
import rwth.i2.ltl2ba4j.formula.INext;
import rwth.i2.ltl2ba4j.formula.INot;
import rwth.i2.ltl2ba4j.formula.IOr;
import rwth.i2.ltl2ba4j.formula.IProposition;
import rwth.i2.ltl2ba4j.formula.IRelease;
import rwth.i2.ltl2ba4j.formula.IUntil;

/**
 * @author amy
 *
 * FormulaFactory
 */
public class FormulaFactory implements IFormulaFactory {

    /**
     * {@inheritDoc}
     */
    public IAnd And(IFormula formula1, IFormula formula2) {
        return new And(formula1, formula2);
    }

    /**
     * {@inheritDoc}
     */
    public IEquivalent Eq(IFormula formula1, IFormula formula2) {
        return new Equivalent(formula1, formula2);
    }

    /**
     * {@inheritDoc}
     */
    public IFinally F(IFormula formula) {
        return new Finally(formula);
    }

    /**
     * {@inheritDoc}
     */
    public IGlobally G(IFormula formula) {
        return new Globally(formula);
    }

    /**
     * {@inheritDoc}
     */
    public IImplies Impl(IFormula formula1, IFormula formula2) {
        return new Implies(formula1, formula2);
    }

    /**
     * {@inheritDoc}
     */
    public INext X(IFormula formula) {
        return new Next(formula);
    }

    /**
     * {@inheritDoc}
     */
    public INot Not(IFormula formula) {
        return new Not(formula);
    }

    /**
     * {@inheritDoc}
     */
    public IOr Or(IFormula formula1, IFormula formula2) {
        return new Or(formula1,formula2);
    }

    /**
     * {@inheritDoc}
     */
    public IProposition Proposition(String label) {
        return new Proposition(label);
    }

    /**
     * {@inheritDoc}
     */
    public IRelease Release(IFormula formula1, IFormula formula2) {
        return new Release(formula1,formula2);
    }

    /**
     * {@inheritDoc}
     */
    public IUntil Until(IFormula formula1, IFormula formula2) {
        return new Until(formula1, formula2);
    }

}
