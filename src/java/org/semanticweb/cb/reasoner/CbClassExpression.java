package org.semanticweb.cb.reasoner;

/**
 * Corresponds to a <a href=
 * "http://www.w3.org/TR/owl2-syntax/#Class_Expressions">Class Expression<a> in
 * the OWL 2 specification.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public abstract class CbClassExpression extends CbObject {

	/**
	 * @param ptr
	 */
	protected CbClassExpression(long ptr) {
		super(ptr);
	}

	protected synchronized native void destruct();

	public native void print();

	public abstract <O> O accept(
			CbClassExpressionVisitor<O> visitor);

	public <O> O accept(CbObjectVisitor<O> visitor) {
		return accept((CbClassExpressionVisitor<O>) visitor);
	}

}