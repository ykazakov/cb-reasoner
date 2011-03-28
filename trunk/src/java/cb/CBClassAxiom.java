package org.semanticweb.cb.reasoner;

/* Class Axioms */
public abstract class CBClassAxiom extends CBAxiom {
		
	protected synchronized native void destruct();
	
	public native void print();
	
	protected void addTo(CBOntology cbOntology) {
		cbOntology.addClassAxiom(this);
	}
	
	protected void removeFrom(CBOntology cbOntology) {
		cbOntology.removeClassAxiom(this);
	}
	
	public abstract <O> O accept(CBClassAxiomVisitorEx<O> visitor);
	
	public <O> O accept(CBAxiomVisitorEx<O> visitor) {
		return accept(visitor);
	}

}