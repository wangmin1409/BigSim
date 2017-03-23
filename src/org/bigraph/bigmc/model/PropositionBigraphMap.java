package org.bigraph.bigmc.model;

import org.bigraph.bigsim.model.Bigraph;


public class PropositionBigraphMap {
	
	protected String proposition;
    
    protected Bigraph bigraph;
    
    public PropositionBigraphMap(String proposition, Bigraph bigraph) {
    	this.proposition = proposition;
    	this.bigraph = bigraph;
    }

	public String getProposition() {
		return proposition;
	}

	public void setProposition(String proposition) {
		this.proposition = proposition;
	}

	public Bigraph getBigraph() {
		return bigraph;
	}

	public void setBigraph(Bigraph bigraph) {
		this.bigraph = bigraph;
	}
    
    
    
}

