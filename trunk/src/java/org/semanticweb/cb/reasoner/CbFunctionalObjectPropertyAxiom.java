package org.semanticweb.cb.reasoner;

/**
 * Corresponds to a <a href=
 * "http://www.w3.org/TR/owl2-syntax/#Functional_Object_Properties">Functional
 * Object Property Axiom<a> in the OWL 2 specification.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public class CbFunctionalObjectPropertyAxiom extends CbObjectPropertyAxiom {

	public CbFunctionalObjectPropertyAxiom(CbObjectPropertyExpression property) {
		super(getPtr(property));
	}

	private static native long getPtr(
			CbObjectPropertyExpression cbObjectPropertyExpression);

	public native CbObjectPropertyExpression getProperty();

	@Override
	public <O> O accept(CbObjectPropertyAxiomVisitor<O> visitor) {
		return visitor.visit(this);
	}

}