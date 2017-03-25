package org.bigraph.bigmc.specification;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.bigraph.bigmc.model.BigraphPair;
import org.bigraph.bigsim.model.Bigraph;

import rwth.i2.ltl2ba4j.LTL2BA4J;
import rwth.i2.ltl2ba4j.model.IGraphProposition;
import rwth.i2.ltl2ba4j.model.ITransition;

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
		String formula = GenBigraph.getFormula();
		ArrayList<ITransition> res = new ArrayList<ITransition>();
		Collection<ITransition> automaton = LTL2BA4J.formulaToBA(formula);
		for (ITransition t : automaton) {
			System.out.println("Transition---" + t);
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

