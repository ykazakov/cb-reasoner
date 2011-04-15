package org.semanticweb.cb.reasoner;

/**
 * Corresponds to an <a href=
 * "http://www.w3.org/TR/owl2-syntax/#Object_Property_Axioms">Object Property
 * Axiom<a> in the OWL 2 specification.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public abstract class CbObjectPropertyAxiom extends CbAxiom {

	/**
	 * @param ptr
	 */
	protected CbObjectPropertyAxiom(long ptr) {
		super(ptr);
	}

	protected synchronized native void destruct();

	public native void print();

	protected void addTo(CbOntology ontology) {
		ontology.addObjectPropertyAxiom(this);
	}

	protected void removeFrom(CbOntology cbOntology) {
		cbOntology.removeObjectPropertyAxiom(this);
	}

	public abstract <O> O accept(
			CbObjectPropertyAxiomVisitor<O> visitor);

	public <O> O accept(CbAxiomVisitor<O> visitor) {
		return accept((CbObjectPropertyAxiomVisitor<O>) visitor);
	}

}