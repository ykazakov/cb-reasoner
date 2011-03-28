package org.semanticweb.cb.reasoner;

/* Object property */
public class CBObjectProperty extends CBObjectPropertyExpression {

	public CBObjectProperty(String iri) {
		create(iri);
	}

	private native void create(String iri);

	public static final CBClass TopObjectProperty = new CBClass(
			"owl:TopObjectProperty");
	public static final CBClass BottomObjectProperty = new CBClass(
			"owl:BottomObjectProperty");

	public native String getIRI();
	
	public <O> O accept(CBObjectPropertyExpressionVisitorEx<O> visitor) {
		return visitor.visit(this);
	}

}
