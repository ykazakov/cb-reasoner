package org.semanticweb.cb.reasoner;

/**
 * Corresponds to a <a href=
 * "http://www.w3.org/TR/owl2-syntax/#Transitive_Object_Properties">Transitive
 * Object Property Axiom<a> in the OWL 2 specification.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public class CbTransitiveObjectPropertyAxiom extends CbObjectPropertyAxiom {

	private static native long getPtr(CbObjectPropertyExpression property);

	public CbTransitiveObjectPropertyAxiom(CbObjectPropertyExpression property) {
		super(getPtr(property));
	}

	public native CbObjectPropertyExpression getProperty();

	@Override
	public <O> O accept(CbObjectPropertyAxiomVisitor<O> visitor) {
		return visitor.visit(this);
	}
}