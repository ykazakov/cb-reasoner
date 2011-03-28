package org.semanticweb.cb.reasoner;

public class CBClassDeclarationAxiom extends CBDeclarationAxiom {
	
	public CBClassDeclarationAxiom(String iri) {
		create(iri);
	}

	private native void create(String iri);

	@Override
	public <O> O accept(CBDeclarationAxiomVisitorEx<O> visitor) {
		return visitor.visit(this);
	}

}
