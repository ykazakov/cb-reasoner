package org.semanticweb.cb.reasoner;

/* Object Property Axioms */
public abstract class CBObjectPropertyAxiom extends CBAxiom {
	
	protected synchronized native void destruct();

	public native void print();
	
	protected void addTo(CBOntology cbOntology) {
		cbOntology.addObjectPropertyAxiom(this);
	}
	
	protected void removeFrom(CBOntology cbOntology) {
		cbOntology.removeObjectPropertyAxiom(this);
	}
	
	public abstract <O> O accept (CBObjectPropertyAxiomVisitorEx<O> visitor);
	
	public <O> O accept(CBAxiomVisitorEx<O> visitor) {
		return accept(visitor);
	}	

}