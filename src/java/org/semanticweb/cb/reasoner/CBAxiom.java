package org.semanticweb.cb.reasoner;

public abstract class CBAxiom extends CBObject {
	
    protected abstract void addTo(CBOntology cbOntology);
    protected abstract void removeFrom(CBOntology cbOntology);    
    
    public abstract <O> O accept(CBAxiomVisitorEx<O> visitor);
    
	public <O> O accept(CBObjectVisitorEx<O> visitor) {
		return accept(visitor);
	}
}