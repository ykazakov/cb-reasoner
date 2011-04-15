package org.semanticweb.cb.reasoner;

/**
 * Corresponds to a <a href=
 * "http://www.w3.org/TR/owl2-syntax/#Class_Expression_Axioms">Class Expression
 * Axiom<a> in the OWL 2 specification.
 * 
 * @author Yevgeny Kazakov
 */
public abstract class CbClassAxiom extends CbAxiom {

	/**
	 * @param ptr
	 */
	protected CbClassAxiom(long ptr) {
		super(ptr);
	}

	protected synchronized native void destruct();

	public native void print();

	protected void addTo(CbOntology ontology) {
		ontology.addClassAxiom(this);
	}

	protected void removeFrom(CbOntology ontology) {
		ontology.removeClassAxiom(this);
	}

	public abstract <O> O accept(CbClassAxiomVisitor<O> visitor);

	public <O> O accept(CbAxiomVisitor<O> visitor) {
		return accept((CbClassAxiomVisitor<O>) visitor);
	}

}