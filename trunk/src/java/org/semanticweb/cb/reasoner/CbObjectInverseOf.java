package org.semanticweb.cb.reasoner;

/**
 * Corresponds to an <a href=
 * "http://www.w3.org/TR/owl2-syntax/#Inverse_Object_Properties">Inverse Object
 * Property<a> in the OWL 2 specification.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public class CbObjectInverseOf extends CbObjectPropertyExpression {

	public CbObjectInverseOf(String iri) {
		super(getPtr(iri));
	}

	private static native long getPtr(String iri);

	public native String getIRI();

	@Override
	public <O> O accept(
			CbObjectPropertyExpressionVisitor<O> visitor) {
		return visitor.visit(this);
	}

}