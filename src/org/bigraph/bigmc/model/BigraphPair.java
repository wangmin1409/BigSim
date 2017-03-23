package org.bigraph.bigmc.model;

import org.bigraph.bigsim.model.Bigraph;


public class BigraphPair {
	protected Bigraph sourceBigraph;
    
    protected Bigraph targetBigraph;  
    
    public BigraphPair(Bigraph sourceBigraph, Bigraph targetBigraph) {
    	this.sourceBigraph = sourceBigraph;
    	this.targetBigraph = targetBigraph;
    }

	public Bigraph getSourceBigraph() {
		return sourceBigraph;
	}

	public void setSourceBigraph(Bigraph sourceBigraph) {
		this.sourceBigraph = sourceBigraph;
	}

	public Bigraph getTargetBigraph() {
		return targetBigraph;
	}

	public void setTargetBigraph(Bigraph targetBigraph) {
		this.targetBigraph = targetBigraph;
	}
    
}

