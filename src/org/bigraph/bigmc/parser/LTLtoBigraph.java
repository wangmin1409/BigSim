package org.bigraph.bigmc.parser;

import java.util.Collection;

import rwth.i2.ltl2ba4j.DottyWriter;
import rwth.i2.ltl2ba4j.LTL2BA4J;
import rwth.i2.ltl2ba4j.formula.IFormulaFactory;
import rwth.i2.ltl2ba4j.formula.impl.FormulaFactory;
import rwth.i2.ltl2ba4j.model.ITransition;

public class LTLtoBigraph {
	
    static LTL2BA4J lTL2BA4J = new LTL2BA4J();
    
	public void entry(){
		IFormulaFactory factory = new FormulaFactory();
		
		/*IFormula formula = factory.G(
                factory.And(
                   factory.Proposition("prop1"),
                   factory.Not(
                           factory.Proposition("prop2")
                   )
                )
              );*/
		
		String formula = "a U(b U c)";
		for(ITransition t: LTL2BA4J.formulaToBA(formula)) {
		    System.out.println(t);
		}
		
		Collection<ITransition> automaton = LTL2BA4J.formulaToBA(formula);
        System.out.println(DottyWriter.automatonToDot(automaton));
		
		
		
		/*for(ITransition t: LTL2BA4J.formulaToBA(formula)) {
		    System.out.println(t);
		}*/
	}
	 	
}

