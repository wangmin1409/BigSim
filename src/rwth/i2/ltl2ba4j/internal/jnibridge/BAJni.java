package rwth.i2.ltl2ba4j.internal.jnibridge;

/**
 * DO NOT MOVE NOR RENAME!
 */

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import rwth.i2.ltl2ba4j.LTL2BA4J;
import rwth.i2.ltl2ba4j.formula.IAnd;
import rwth.i2.ltl2ba4j.formula.IEquivalent;
import rwth.i2.ltl2ba4j.formula.IFinally;
import rwth.i2.ltl2ba4j.formula.IFormula;
import rwth.i2.ltl2ba4j.formula.IGlobally;
import rwth.i2.ltl2ba4j.formula.IImplies;
import rwth.i2.ltl2ba4j.formula.INext;
import rwth.i2.ltl2ba4j.formula.INot;
import rwth.i2.ltl2ba4j.formula.IOr;
import rwth.i2.ltl2ba4j.formula.IProposition;
import rwth.i2.ltl2ba4j.formula.IRelease;
import rwth.i2.ltl2ba4j.formula.IUntil;
import rwth.i2.ltl2ba4j.model.IGraphProposition;
import rwth.i2.ltl2ba4j.model.IState;
import rwth.i2.ltl2ba4j.model.ITransition;

/**
 * @author amy
 *
 * BAJni
 */
public class BAJni {
	
    /**
     * Label for edge that can be taken by all propositions. 
     */
    private static final String SIGMA = "<SIGMA>";
    
    /**
     * Label for a state of the generated automaton.
     */
    private static final String STATE_LABEL_PREFIX = "state";
    
    /**
     * State number of the initial state returned by ltl2ba.
     */
    private static final int STATE_ID_INITIAL_STATE = -1;
    
    /**
     * Resets ltl2ba.
     */
    private native void init();

    /**
     * Sets a formula in ltl2ba.
     * @param f the formula
     */
    private native void setform(String f);

    /**
     * Parses the formula set by setForm.
     * @see #setform(String)
     */
    private native void tl_parse();

    /**
     * Returns a pointer to the first state of the resulting automaton.
     * This is a dummy state, since we have a circular list.
     * @return the pointer to the state before to the first state of the resulting automaton
     */
    private native long bstates();

    /**
     * A constant meaning <i>accepting state</i>.
     * @return a constant
     * @see #bstateFinal(long)
     */
    private native int b_accept();

    /**
     * Prints the automaton, whose root is given by <code>p</code>.
     * For debugging purposes only.
     * @param p pointer to the first state of the automaton to print
     */
    private native void print_buchi(long p);

    /**
     * Not actually needed.
     */
    private native int get_sym_id();

    /**
     * Not actually needed.
     */
    private native int get_sym_size();

    /**
     * Returns the symbol table used for assigning labels to edges.
     * @return the symbol table
     */
    private native String[] get_sym_table();

    /**
     * Returns the subsequent state to s in the list of all states.
     * @param s pointer to a state
     * @return pointer to the next state
     */
    private native long bstateNxt(long s);

    /**
     * Used to determine if a state is final.
     * @param s pointer to a state
     * @return <code>b_accept()</code> iff <code>s</code> is final
     * @see #b_accept() 
     */
    private native int bstateFinal(long s);

    /**
     * The id number for state s.
     * This is <code>STATE_ID_INITIAL_STATE</code> for the initial state.
     * @param s pointer to a state
     * @return the state id
     */
    private native int bstateId(long s);

    /**
     * Returns the root of the transition list. This is a dummy object,
     * since we have a circular list.
     * @param s pointer to a state
     * @return transition before the first outgoing transition of <code>s</code>
     */
    private native long bstateFirstTrans(long s);

    /**
     * Next transition in the list after t.
     * @param t a pointer to a transition
     * @return next transition
     * @see #bstateFirstTrans(long)
     */
    private native long btransNxt(long t);

    /**
     * Returns the state the outgoing transition is pointing to.
     * @param t a pointer to a transition
     * @return pointer to the state the transition is pointing to
     */
    private native long btransTo(long t);

    /**
     * Boolean coded int. The i-th bit from the right is <code>1</code> iff t is labelled
     * with the i-th label of the symbol table.
     * @param t a pointer to a transition
     * @return boolean coded int. 
     * @see #get_sym_table()
     */
    private native int btransPos(long t);

    /**
     * Boolean coded int. The i-th bit from the right is <code>1</code> iff t is labelled
     * with the negate of the i-th label of the symbol table.
     * @param t a pointer to a transition
     * @return boolean coded int. 
     * @see #get_sym_table()
     */
    private native int btransNeg(long t);

    static {
        /* Initialize the dynamic libraries */
    	
        String workDir = new File("").getAbsolutePath();
        String filePath = null;
        //load required libraries
        if (System.getProperty("os.name").startsWith("Windows")) {
            filePath = workDir+"\\c-lib\\win64\\ltl2ba.dll";
        } else if (System.getProperty("os.name").startsWith("Linux")) {
            filePath = workDir+"/c-lib/linux/libBAJni.so";
        } else if (System.getProperty("os.name").startsWith("FreeBSD")) {
            filePath = workDir+"/c-lib/freebsd/libBAJni.so";
        } else {
            //filePath = workDir+"ltl2ba";
        	filePath = workDir+"/c-lib/linux/libBAJni.so";
        }
        if(filePath != null) {
            try {
                System.load(filePath);
            } catch(Exception e) {
                throw new RuntimeException("LTL2BA: libary could not be loaded from "+filePath,e);
            }
        } else {
            try {
                System.loadLibrary("ltl2ba");
            } catch(Exception e) {
                throw new RuntimeException("LTL2BA: Unsupported OS; and 'ltl2ba' libary could not be" +
                        "loaded from 'java.library.path':\n" + System.getProperty("java.library.path",""),
                        e);
            }
        }
    }    
    
    /**
     * List of transitions defining the automaton.
     */
    private Collection<ITransition> transitions;
    
    /**
     * If set, this converter is used to code propositions to strings and back.
     */
    private FormulaConverter converter = null;
    
    /**
     * Constructs a new automaton for the given formula.
     * @throws IllegalArgumentException if formula has invalid syntax 
     * @param formula a formula
     */
    public BAJni(IFormula formula) {
        this.converter = new FormulaConverter(formula);
        this.process(this.converter.getLTL2BAInput());
    }
    
    /**
     * Constructs a new automaton for the given formula.
     * @throws IllegalArgumentException if formula has invalid syntax 
     * @param formula a formula
     */
    public BAJni(String formula) {
        this.process(formula);        
    }

    /**
     * Calls ltl2ba and constructs the output.
     * @param formula a formula
     * @throws IllegalArgumentException if formula has invalid syntax 
     * @throws RuntimeException if formula is longer than 4095 characters (ltl2ba limitation)
     */
    private void process(String formula) {
        if(formula.length()>4095) {
            throw new RuntimeException("LTL2BA limitation: formula must not be longer than 4095 characters");
        }
        this.transitions = new ArrayList<ITransition>();
        //initialize
        this.init();
        //set formula
        this.setform(formula);
        //parse formula and produce automaton
        this.tl_parse();
        //read output from library functions and
        //write into Java data structures
        try {
            this.digestOutput();
        } catch(IllegalArgumentException e) {
            throw new IllegalArgumentException("Invalid formula: '"+formula+"'");
        }
    }

    /**
     * Builds the output objects from the internal state list.
     * @throws IllegalArgumentException if formula has invalid syntax 
     */
    private void digestOutput() {
        
        Map<IState,IState> states = new HashMap<IState,IState>();
        
        //in the following, all "long" values are actually pointers in C !
        
        //dummy node in the circular list of states
        long root = this.bstates();
       
        //if there was no error and so we have states
        if(root!=0) {
            String[] allLabels = this.get_sym_table();

            //iterate over state list
            for (long pSourceState = this.bstateNxt(root); pSourceState != this.bstates(); pSourceState = this.bstateNxt(pSourceState)) {
                IState sourceState = LTL2BA4J.getGraphFactory().State(
                        STATE_LABEL_PREFIX + this.bstateFinal(pSourceState) + "_" + this.bstateId(pSourceState),
                        (this.bstateId(pSourceState) == STATE_ID_INITIAL_STATE),
                        (this.bstateFinal(pSourceState) == this.b_accept())
                        );
                //take care that equal states are unique
                if(states.containsKey(sourceState)) {
                    sourceState = states.get(sourceState);
                } else {
                    states.put(sourceState, sourceState);
                }
                
                long troot = this.bstateFirstTrans(pSourceState);
                for (long pTransition = this.btransNxt(troot); pTransition != troot; pTransition = this.btransNxt(pTransition)) {
                    long pTargetState = this.btransTo(pTransition);
                    IState targetState = LTL2BA4J.getGraphFactory().State(
                            STATE_LABEL_PREFIX + this.bstateFinal(pTargetState) + "_" + this.bstateId(pTargetState),
                            (this.bstateId(pTargetState) == STATE_ID_INITIAL_STATE),
                            (this.bstateFinal(pTargetState) == this.b_accept())
                            );
                    //take care that equal states are unique
                    if(states.containsKey(targetState)) {
                        targetState = states.get(targetState);
                    } else {
                        states.put(targetState, targetState);
                    }
                    
                    Set<IGraphProposition> labels = new HashSet<IGraphProposition>();
                    if(this.btransPos(pTransition)==0 && this.btransNeg(pTransition)==0) {
                        // we have a "Sigma" edge
                        labels.add(LTL2BA4J.getGraphFactory().SigmaProposition());
                    } else {
                        for(int i=0;i<allLabels.length;i++) {
                            if((this.btransPos(pTransition) & (1<<i))>0) {
                                if(this.converter!=null) {
                                    //we have a converter set, so convert propositions back
                                    IGraphProposition prop = this.converter.internalLabelToProposition(allLabels[i],false);
                                    labels.add(prop);
                                } else {
                                    labels.add(LTL2BA4J.getGraphFactory().Proposition(allLabels[i], false));
                                }
                            }
                            if((this.btransNeg(pTransition) & (1<<i))>0) {
                                if(this.converter!=null) {
                                    //we have a converter set, so convert propositions back
                                    IGraphProposition prop = this.converter.internalLabelToProposition(allLabels[i],true);
                                    labels.add(prop);
                                } else {
                                    labels.add(LTL2BA4J.getGraphFactory().Proposition(allLabels[i], true));
                                }
                            }
                        }
                    }
                    
                    ITransition transition = LTL2BA4J.getGraphFactory().Transition(labels, sourceState, targetState);
                    
                    this.transitions.add(transition);
                }
            }
        } else {
            throw new IllegalArgumentException("invalid formula");
        }
    }

    /**
     * Returns the resulting automaton as a list of transitions.
     * @return resulting automaton as a list of transitions
     */
    public Collection<ITransition> getTransitions() {
        return transitions;
    }

    /**
     * @author Eric Bodden
     *
     * Used to map propositions ot an internal string representation
     * and vice versa.
     */
    private class FormulaConverter {

        private static final String PROPOSITION_LABEL_PREFIX = "p";
        
        private String ltl2baInput;
        
        private Map<IProposition,String> propositionToInternalLabel;

        private Map<String,IProposition> internalLabelToProposition;
        
        private int propCount = 0;
        
        /**
         * @param formula
         */
        public FormulaConverter(IFormula formula) {
            super();
            this.propositionToInternalLabel = new HashMap<IProposition,String>();
            this.internalLabelToProposition = new HashMap<String,IProposition>();
            this.propCount = 0;
            this.ltl2baInput = constructLTL2BAInput(formula);
        }
        
        /**
         * @param formula
         * @return String to be passed to ltl2ba
         * @throws RuntimeException if more than 256 propositions are contained in the formula 
         */
        private String constructLTL2BAInput(IFormula formula) {
            if(formula==null) {
                throw new IllegalArgumentException("formula is null!");
            }
            if(formula instanceof IAnd) {
                return "(" +
                constructLTL2BAInput(((IAnd)formula).getSubformula1()) +
                ") && (" +             
                constructLTL2BAInput(((IAnd)formula).getSubformula2()) +
                ")";
            } else 
            if(formula instanceof IOr) {
                return "(" +
                constructLTL2BAInput(((IOr)formula).getSubformula1()) +
                ") || (" +             
                constructLTL2BAInput(((IOr)formula).getSubformula2()) +
                ")";
            } else 
            if(formula instanceof IImplies) {
                return "(" +
                constructLTL2BAInput(((IImplies)formula).getSubformula1()) +
                ") -> (" +             
                constructLTL2BAInput(((IImplies)formula).getSubformula2()) +
                ")";
            } else 
            if(formula instanceof IEquivalent) {
                return "(" +
                constructLTL2BAInput(((IEquivalent)formula).getSubformula1()) +
                ") <-> (" +             
                constructLTL2BAInput(((IEquivalent)formula).getSubformula2()) +
                ")";
            } else 
            if(formula instanceof IUntil) {
                return "(" +
                constructLTL2BAInput(((IUntil)formula).getSubformula1()) +
                ") U (" +             
                constructLTL2BAInput(((IUntil)formula).getSubformula2()) +
                ")";
            } else 
            if(formula instanceof IRelease) {
                return "(" +
                constructLTL2BAInput(((IRelease)formula).getSubformula1()) +
                ") V (" +             
                constructLTL2BAInput(((IRelease)formula).getSubformula2()) +
                ")";
            } else 
            if(formula instanceof INext) {
                return "X(" +
                constructLTL2BAInput(((INext)formula).getSubformula()) +
                ")";
            } else 
            if(formula instanceof IFinally) {
                return "<>(" +
                constructLTL2BAInput(((IFinally)formula).getSubformula()) +
                ")";
            } else 
            if(formula instanceof IGlobally) {
                return "[](" +
                constructLTL2BAInput(((IGlobally)formula).getSubformula()) +
                ")";
            } else 
            if(formula instanceof INot) {
                return "!(" +
                constructLTL2BAInput(((INot)formula).getSubformula()) +
                ")";
            } else 
            if(formula instanceof IProposition) {
                IProposition proposition = ((IProposition)formula);
                String internalLabel;
                if(this.propositionToInternalLabel.containsKey(proposition)) {
                    //fetch old label
                    internalLabel = this.propositionToInternalLabel.get(proposition);
                } else {
                    //generate new label
                    internalLabel = PROPOSITION_LABEL_PREFIX + this.propCount;
                    this.propCount++;
                    if(this.propositionToInternalLabel.size()>255) {
                        throw new RuntimeException("LTL2BA limitation: not more than " +
                            "256 propositions allowed in formula");
                    }
                    this.propositionToInternalLabel.put(proposition, internalLabel);                
                    this.internalLabelToProposition.put(internalLabel, proposition);                
                }
                return internalLabel;
            } else {
                throw new RuntimeException("unknown formula term constructor");
            }        
        }
        
        /**
         * @return String input for ltl2ba for the given formula.
         */
        public String getLTL2BAInput() {
            return this.ltl2baInput;
        }
        
        /**
         * Map back from internal labels back to graph propositions.
         * @param internalLabel
         * @param negated
         * @return
         */
        public IGraphProposition internalLabelToProposition(String internalLabel, boolean negated) {
            IProposition proposition = this.internalLabelToProposition.get(internalLabel);
            return LTL2BA4J.getGraphFactory().Proposition(proposition.getLabel(), negated);
        }
        
    }
    
}
