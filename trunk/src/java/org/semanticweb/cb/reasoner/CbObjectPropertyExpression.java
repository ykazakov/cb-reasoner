package org.semanticweb.cb.reasoner;

/**
 * Corresponds to an <a href=
 * "http://www.w3.org/TR/owl2-syntax/#Object_Property_Expressions">Object
 * Property Expression<a> in the OWL 2 specification.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public abstract class CbObjectPropertyExpression extends CbObject {

	/**
	 * @param ptr
	 */
	protected CbObjectPropertyExpression(long ptr) {
		super(ptr);
	}

	protected native synchronized void destruct();

	public native void print();

	public abstract <O> O accept(CbObjectPropertyExpressionVisitor<O> visitor);

	public <O> O accept(CbObjectVisitor<O> visitor) {
		return accept((CbObjectPropertyExpressionVisitor<O>) visitor);
	}

}
