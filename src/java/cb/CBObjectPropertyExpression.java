package org.semanticweb.cb.reasoner;


/* Object Property Expressions */
public abstract class CBObjectPropertyExpression extends CBObject {
	
	protected native synchronized void destruct();
	
	public native void print();
		
	public abstract <O> O accept (CBObjectPropertyExpressionVisitorEx<O> visitor);
		
	public <O> O accept(CBObjectVisitorEx<O> visitor) {
		return accept(visitor);
	}
			
}
