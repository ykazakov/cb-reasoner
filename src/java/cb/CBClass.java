package org.semanticweb.cb.reasoner;

/* Class */
public class CBClass extends CBClassExpression {

	public CBClass(String iri) {
		create(iri);		
	}
		
	private native void create(String iri);
	
	public static final CBClass Thing = new CBClass("owl:Thing");
	public static final CBClass Nothing = new CBClass("owl:Nothing");
	
	public native String getIRI();

	@Override
	public <O> O accept(CBClassExpressionVisitorEx<O> visitor) {
		return visitor.visit(this);
	}			
		
}

