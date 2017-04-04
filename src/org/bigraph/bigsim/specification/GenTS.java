package org.bigraph.bigsim.specification;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Set;


import rwth.i2.ltl2ba4j.LTL2BA4J;
import rwth.i2.ltl2ba4j.model.IGraphProposition;
import rwth.i2.ltl2ba4j.model.ITransition;
import org.bigraph.bigsim.model.Specification;

public class GenTS {
	//private static String formula = "<>a";
	
	//private static String formula = "a U (b U c)";
	
    static LTL2BA4J lTL2BA4J = new LTL2BA4J();
    
    /**
     *  G   (always) (Spin syntax : [])
        F   (eventually) (Spin syntax : <>)
        U   (until)
        R   (realease) (Spin syntax : V)
        X   (next)
     */
    public static void main(String[] args) {
    	genTS();
	}
	
	/**
	 * 调用formulaToBA生成迁移系统
	 * @return
	 */
	public static ArrayList<ITransition> genTS() {
		String formula = Specification.processSpec().formula();
		String negFormula = "!" + formula; 
		ArrayList<ITransition> res = new ArrayList<ITransition>();
		Collection<ITransition> automaton = LTL2BA4J.formulaToBA(negFormula);
		for (ITransition t : automaton) {
			System.out.println("Transition---" + t);
			System.out.println(t.getLabels());
			Set<IGraphProposition> igset = t.getLabels();
			for (IGraphProposition ig : igset) {
				System.out.println(ig.getFullLabel());
				System.out.println(ig.getLabel());
				System.out.println(ig.isNegated());
			}
			System.out.println(t.getSourceState());
			System.out.println(t.getSourceState().getLabel());
			res.add(t);
		}
		return  res;
	}
	
	public static String getLineInfo()
    {
        StackTraceElement ste = new Throwable().getStackTrace()[1];
        return ste.getFileName() + ": Line " + ste.getLineNumber();
    }
	 	
}

