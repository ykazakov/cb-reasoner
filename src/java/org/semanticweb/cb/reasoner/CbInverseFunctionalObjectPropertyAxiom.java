package org.semanticweb.cb.reasoner;

/**
 * Corresponds to an <a href=
 * "http://www.w3.org/TR/owl2-syntax/#Inverse-Functional_Object_Properties"
 * >Inverse Functional Object Property Axiom<a> in the OWL 2 specification.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public class CbInverseFunctionalObjectPropertyAxiom extends
		CbObjectPropertyAxiom {

	public CbInverseFunctionalObjectPropertyAxiom(
			CbObjectPropertyExpression property) {
		super(getPtr(property));
	}

	private static native long getPtr(CbObjectPropertyExpression property);

	public native CbObjectPropertyExpression getProperty();

	@Override
	public <O> O accept(CbObjectPropertyAxiomVisitor<O> visitor) {
		return visitor.visit(this);
	}

}