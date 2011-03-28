package org.semanticweb.cb.reasoner;

/* ObjectInverseOf property */
public class CBObjectInverseOf extends CBObjectPropertyExpression {

	public CBObjectInverseOf(String iri) {
		create(iri);
	}

	private native void create(String iri);
	
	public native String getIRI();

	@Override
	public <O> O accept(CBObjectPropertyExpressionVisitorEx<O> visitor) {
		return visitor.visit(this);
	}

}