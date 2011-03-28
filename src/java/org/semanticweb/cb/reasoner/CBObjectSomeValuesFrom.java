package org.semanticweb.cb.reasoner;


/* ObjectSomeValuesFrom */
public class CBObjectSomeValuesFrom extends CBClassExpression {
	
	public CBObjectSomeValuesFrom(CBObjectPropertyExpression ope, CBClassExpression ce) {
		create(ope, ce);		
	}

	private native void create(CBObjectPropertyExpression ope, CBClassExpression ce);
	
	public native CBObjectProperty getProperty();	
	public native CBClassExpression getFiller();

	@Override
	public <O> O accept(CBClassExpressionVisitorEx<O> visitor) {
		return visitor.visit(this);
	}

}
