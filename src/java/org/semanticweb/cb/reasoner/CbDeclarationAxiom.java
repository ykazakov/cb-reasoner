package org.semanticweb.cb.reasoner;

/**
 * Corresponds to a <a href=
 * "http://www.w3.org/TR/owl2-syntax/#Entity_Declarations_and_Typing">Entity
 * Declaration<a> in the OWL 2 specification.
 * 
 * @author Yevgeny Kazakov
 */
public abstract class CbDeclarationAxiom extends CbAxiom {

	/**
	 * @param ptr
	 */
	protected CbDeclarationAxiom(long ptr) {
		super(ptr);
	}

	protected synchronized native void destruct();

	public native void print();

	protected void addTo(CbOntology ontology) {
		ontology.addDeclarationAxiom(this);
	}

	protected void removeFrom(CbOntology ontology) {
		ontology.removeDeclarationAxiom(this);
	}

	public abstract <O> O accept(
			CbDeclarationAxiomVisitor<O> visitor);

	public <O> O accept(CbAxiomVisitor<O> visitor) {
		return accept((CbDeclarationAxiomVisitor<O>) visitor);
	}

}
