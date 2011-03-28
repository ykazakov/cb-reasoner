package org.semanticweb.cb.reasoner;

/* ObjectIntersectionOf */
public class CBObjectIntersectionOf extends CBClassExpression {

	public CBObjectIntersectionOf(CBClassExpression... ces) {
		create(ces);
	}

	private native void create(CBClassExpression[] ces);
	
	public native CBClassExpression[] getOperands();	

	@Override
	public <O> O accept(CBClassExpressionVisitorEx<O> visitor) {
		return visitor.visit(this);
	}
	
}
