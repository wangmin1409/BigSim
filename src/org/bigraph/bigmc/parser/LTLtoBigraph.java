package org.bigraph.bigmc.parser;

import rwth.i2.ltl2ba4j.LTL2BA4J;
import rwth.i2.ltl2ba4j.formula.IFormula;
import rwth.i2.ltl2ba4j.formula.IFormulaFactory;
import rwth.i2.ltl2ba4j.formula.impl.FormulaFactory;
import rwth.i2.ltl2ba4j.model.ITransition;

public class LTLtoBigraph {
	
    static LTL2BA4J lTL2BA4J = new LTL2BA4J();
    
	public void entry(){
		IFormulaFactory factory = new FormulaFactory();
		
		IFormula formula = factory.G(
                factory.And(
                   factory.Proposition("prop1"),
                   factory.Not(
                           factory.Proposition("prop2")
                   )
                )
              );
		
		for(ITransition t: LTL2BA4J.formulaToBA("a U(b U c)")) {
		    System.out.println(t);
		}
		
		
		
		/*for(ITransition t: LTL2BA4J.formulaToBA(formula)) {
		    System.out.println(t);
		}*/
	}
	 	
}

