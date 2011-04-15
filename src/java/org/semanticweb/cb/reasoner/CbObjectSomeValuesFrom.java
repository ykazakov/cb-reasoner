package org.semanticweb.cb.reasoner;

/**
 * Corresponds to an <a href=
 * "http://www.w3.org/TR/owl2-syntax/#Existential_Quantification">Existential
 * Quantification Object Property Restriction<a> in the OWL 2 specification.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public class CbObjectSomeValuesFrom extends CbClassExpression {

	public CbObjectSomeValuesFrom(CbObjectPropertyExpression property,
			CbClassExpression filler) {
		super(getPtr(property, filler));
	}

	private static native long getPtr(CbObjectPropertyExpression property,
			CbClassExpression filler);

	public native CbObjectProperty getProperty();

	public native CbClassExpression getFiller();

	@Override
	public <O> O accept(CbClassExpressionVisitor<O> visitor) {
		return visitor.visit(this);
	}

}
