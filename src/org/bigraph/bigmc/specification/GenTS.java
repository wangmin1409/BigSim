package org.bigraph.bigmc.specification;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.bigraph.bigsim.model.Bigraph;

import rwth.i2.ltl2ba4j.DottyWriter;
import rwth.i2.ltl2ba4j.LTL2BA4J;
import rwth.i2.ltl2ba4j.model.IGraphProposition;
import rwth.i2.ltl2ba4j.model.ITransition;

public class GenTS {
	Bigraph b = new Bigraph();
	private static String formula = "<>a";
	
    static LTL2BA4J lTL2BA4J = new LTL2BA4J();
    
    /**
     *  G   (always) (Spin syntax : [])
        F   (eventually) (Spin syntax : <>)
        U   (until)
        R   (realease) (Spin syntax : V)
        X   (next)
     */
    public static void main(String[] args) {
    	GenTS test = new GenTS();
    	test.genTS();
	}
    
	public void entry(){
		
		String formula = "a U(b U c)";
		for(ITransition t: LTL2BA4J.formulaToBA(formula)) {
		    System.out.println(t);
		}
		
		Collection<ITransition> automaton = LTL2BA4J.formulaToBA(formula);
        System.out.println(DottyWriter.automatonToDot(automaton));
	}
	
	/**
	 * 调用formulaToBA生成迁移系统
	 * @return
	 */
	private  Collection<ITransition> genTS() {
		Collection<ITransition> automaton = LTL2BA4J.formulaToBA(formula);
		for(ITransition t: automaton) {
			System.out.println("Transition---" + t);
			System.out.println("label-----" + t.getLabels());
			System.out.println("sourceState-----" + t.getSourceState());
			System.out.println("sourceState---label-----" + t.getSourceState().getLabel());
			System.out.println("sourceState---isInitial-----" + t.getSourceState().isInitial());
			System.out.println("sourceState---isFinal-----" + t.getSourceState().isFinal());
			System.out.println("targetState----" + t.getTargetState());
			System.out.println("targetState----label----" + t.getTargetState().getLabel());
			System.out.println("targetState----isInitial----" + t.getTargetState().isInitial());
			System.out.println("targetState----isFinal------" + t.getTargetState().isFinal());
		}
		return automaton;
	}
	
	/**
	 * 从迁移系统生成Bigraph键值对，key-value均为Bigraph
	 * 然后得到此结构的List
	 */
	private List<BigraphPair> getBigraphPair(Collection<ITransition> automaton) {
		List<BigraphPair> bigraphPairList = new ArrayList<BigraphPair>();
		for(ITransition t: automaton) {
			System.out.println(t);
			Set<IGraphProposition> label = t.getLabels();
		}
		return bigraphPairList;
		
	}
	
	 	
}

