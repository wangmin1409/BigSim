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
	private static String formula = "<>a";
	
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
		ArrayList<ITransition> res = new ArrayList<ITransition>();
		Collection<ITransition> automaton = LTL2BA4J.formulaToBA(formula);
		/*for(ITransition t: automaton) {
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
		}*/
		for (ITransition t : automaton) {
			System.out.println("Transition---" + t);
			res.add(t);
		}
		return  res;
	}
	
	/**
	 * 从迁移系统生成Bigraph键值对，key-value均为Bigraph
	 * 然后得到此结构的List
	 */
	private List<BigraphPair> getBigraphPair() {
		Collection<ITransition> automaton = genTS();
		List<BigraphPair> bigraphPairList = new ArrayList<BigraphPair>();
		BigraphPair bigraphPair = new BigraphPair(null, null);
		Bigraph trueB = new Bigraph();
		Bigraph init = new Bigraph();
		for(ITransition t: automaton) {
			System.out.println(t);
			Set<IGraphProposition> labelSet = t.getLabels();
			System.out.println(labelSet.size());
			if (labelSet.size() == 1) {
				if (isSIGMA(labelSet)) {
					System.out.println("label为siegma");
					if (t.getSourceState().isFinal()) {
						bigraphPairList.add(bigraphPair);
					}
				}
			}else {
				for (IGraphProposition graphProposition : labelSet) {
					System.out.println(graphProposition.getLabel());
				}
			}
		}
		return bigraphPairList;
		
	}
	
	/**
	 * 判断label是否为<SIGMA>
	 */
	
	private boolean isSIGMA(Set<IGraphProposition> labelSet) {
		boolean res = false; 
		for (IGraphProposition graphProposition : labelSet) {
			if (("<SIGMA>").equals(graphProposition.getLabel())) {
				res = true;
			}
		}
		return res;
	}
	 	
}

