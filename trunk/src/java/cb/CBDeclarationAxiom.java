package org.semanticweb.cb.reasoner;

public abstract class CBDeclarationAxiom extends CBAxiom {
	protected synchronized native void destruct();

	public native void print();

	protected void addTo(CBOntology cbOntology) {
		cbOntology.addDeclarationAxiom(this);
	}

	protected void removeFrom(CBOntology cbOntology) {
		cbOntology.removeDeclarationAxiom(this);
	}

	public abstract <O> O accept(CBDeclarationAxiomVisitorEx<O> visitor);

	public <O> O accept(CBAxiomVisitorEx<O> visitor) {
		return accept(visitor);
	}

}
