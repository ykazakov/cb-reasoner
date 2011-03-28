package org.semanticweb.cb.reasoner;

/* Class Expressions */
public abstract class CBClassExpression extends CBObject {
	
	protected synchronized native void destruct();
	
	public native void print();
	
	public abstract <O> O accept(CBClassExpressionVisitorEx<O> visitor);
	
	public <O> O accept(CBObjectVisitorEx<O> visitor) {
		return accept(visitor);
	}

}