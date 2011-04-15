package org.semanticweb.cb.reasoner;

/**
 * Corresponds to an <a href=
 * "http://www.w3.org/TR/owl2-syntax/#Inverse_Object_Properties_2">Inverse
 * Object Properties Axiom<a> in the OWL 2 specification.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public class CbInverseObjectPropertiesAxiom extends CbObjectPropertyAxiom {

	public CbInverseObjectPropertiesAxiom(
			CbObjectPropertyExpression firstProperty,
			CbObjectPropertyExpression secondProperty) {
		super(getPtr(firstProperty, secondProperty));
	}

	private static native long getPtr(CbObjectPropertyExpression firstProperty,
			CbObjectPropertyExpression secondProperty);

	public native CbObjectPropertyExpression getFirstProperty();

	public native CbObjectPropertyExpression getSecondProperty();

	@Override
	public <O> O accept(CbObjectPropertyAxiomVisitor<O> visitor) {
		return visitor.visit(this);
	}

}
