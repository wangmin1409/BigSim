package rwth.i2.ltl2ba4j;

import java.util.Collection;

import rwth.i2.ltl2ba4j.formula.IFormula; 
import rwth.i2.ltl2ba4j.internal.jnibridge.BAJni;
import rwth.i2.ltl2ba4j.model.IGraphFactory;
import rwth.i2.ltl2ba4j.model.ITransition;
import rwth.i2.ltl2ba4j.model.impl.GraphFactory;

/**
 * @author amy
 *
 * LTL2BA4J - Facade giving access to ltl2ba.
 */
public class LTL2BA4J {

    /**
     * Factory used to create the states and transitions.
     */
    private static IGraphFactory graphFactory;

    /**
     * Converts the given formula into a finite state machine.
     * @param formula a valid ltl2ba formula
     * the formula must not be longer than 4095 characters
     * @return the resulting automaton as a list of transitions;
     * if the list is empty, that means that the automaton never matches
     * @throws IllegalArgumentException if formula has invalid syntax 
     */
    public static Collection<ITransition> formulaToBA(IFormula formula) {
        //call ltl2ba over JNI
        BAJni bajni = new BAJni(formula);

        return bajni.getTransitions();
    }

    /**
     * Converts the given formula into a finite state machine.
     * @param formulaAsString a valid ltl2ba formula;
     * the formula must not be longer than 4095 characters
     * @return the resulting automaton as a list of transitions;
     * if the list is empty, that means that the automaton never matches
     * @throws IllegalArgumentException if formula has invalid syntax 
     */
    public static Collection<ITransition> formulaToBA(String formulaAsString) {
        //call ltl2ba over JNI
        BAJni bajni = new BAJni(formulaAsString);

        return bajni.getTransitions();
    }

    /**
     * Sets a factory used to generate the resulting graph.
     * Default is GraphFactory.
     * @param customGraphFactory
     * @see GraphFactory
     */
    public static void setGraphFactory(IGraphFactory customGraphFactory) {
        graphFactory = customGraphFactory;
    }
    
    public static IGraphFactory getGraphFactory() {
        if(graphFactory==null) {
            graphFactory = new GraphFactory();
        }
        return graphFactory;
    }
    
    /**
     * @param args the formula
     */
    public static void main(String args[]) {
    	String str = "a U (b U c)";
        /*if(args.length!=1) {
            System.out.println("Enter the formula as a single argument.");
            System.out.println();
            
            final String syntax = "Propositonal Symbols:\r\n" + 
                    "        true, false\r\n" + 
                    "        any lowercase string\r\n" + 
                    "\r\n" + 
                    "Boolean operators:\r\n" + 
                    "        !   (negation)\r\n" + 
                    "        ->  (implication)\r\n" + 
                    "        <-> (equivalence)\r\n" + 
                    "        &&  (and)\r\n" + 
                    "        ||  (or)\r\n" + 
                    "\r\n" + 
                    "Temporal operators:\r\n" + 
                    "        []  (always)\r\n" + 
                    "        <>  (eventually)\r\n" + 
                    "        U   (until)\r\n" + 
                    "        V   (release)\r\n" + 
                    "        X   (next)";

            System.out.println(syntax);
        } else {

            Collection<ITransition> automaton = formulaToBA(str);
            System.out.println(DottyWriter.automatonToDot(automaton));
            
        }*/
    	Collection<ITransition> automaton = formulaToBA(str);
        System.out.println(DottyWriter.automatonToDot(automaton));
    }
    
}
