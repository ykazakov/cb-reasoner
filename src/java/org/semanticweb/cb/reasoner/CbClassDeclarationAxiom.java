package org.semanticweb.cb.reasoner;

/**
 * Corresponds to a <a href=
 * "http://www.w3.org/TR/owl2-syntax/#Entity_Declarations_and_Typing">Class
 * Entity Declaration<a> in the OWL 2 specification.
 * 
 * @author Yevgeny Kazakov
 */
public class CbClassDeclarationAxiom extends CbDeclarationAxiom {

	public CbClassDeclarationAxiom(String iri) {
		super(getPtr(iri));
	}

	private static native long getPtr(String iri);

	@Override
	public <O> O accept(CbDeclarationAxiomVisitor<O> visitor) {
		return visitor.visit(this);
	}

}
