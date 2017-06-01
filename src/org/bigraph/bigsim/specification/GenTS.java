package org.bigraph.bigsim.specification;

import java.lang.reflect.Array;
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
    	long start =  System.currentTimeMillis();
    	genTS();
    	long end =  System.currentTimeMillis();
    	System.out.println("total time is " + (end - start)+ " ms" );
	}
	
	/**
	 * 调用formulaToBA生成迁移系统
	 * @return
	 */
	public static ArrayList<ITransition> genTS() {
		String[] arr = {"a V b",
				"[]!(a&&b)->Xc",
				"(a&&b) U c U d",
				"[]!a",
				"<>b",
				"a U b",
				"Xa"};
		//Array[String] arr = new Array()[]
		String formula = arr[0];
		//String formula = Specification.processSpec().formula();
		String negFormula = "!" + formula; 
		ArrayList<ITransition> res = new ArrayList<ITransition>();
		Collection<ITransition> automaton = LTL2BA4J.formulaToBA(formula);
		System.out.println(automaton.size());
		for (ITransition t : automaton) {
			System.out.println("Transition---" + t);
			//System.out.println(t.getLabels());
			Set<IGraphProposition> igset = t.getLabels();
			for (IGraphProposition ig : igset) {
				/*System.out.println(ig.getFullLabel());
				System.out.println(ig.getLabel());
				System.out.println(ig.isNegated());*/
			}
			/*System.out.println(t.getSourceState());
			System.out.println(t.getSourceState().getLabel());*/
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

