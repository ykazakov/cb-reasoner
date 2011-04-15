package org.semanticweb.cb.reasoner;

/**
 * Corresponds to an <a href=
 * "http://www.w3.org/TR/owl2-syntax/#Axioms">Axiom<a> in the OWL 2
 * specification.
 * 
 * @author Yevgeny Kazakov
 */
public abstract class CbAxiom extends CbObject {

	/**
	 * @param ptr
	 */
	protected CbAxiom(long ptr) {
		super(ptr);
	}

	protected abstract void addTo(CbOntology cbOntology);

	protected abstract void removeFrom(CbOntology cbOntology);

	public abstract <O> O accept(CbAxiomVisitor<O> visitor);

	public <O> O accept(CbObjectVisitor<O> visitor) {
		return accept( (CbAxiomVisitor<O>)visitor );
	}
}