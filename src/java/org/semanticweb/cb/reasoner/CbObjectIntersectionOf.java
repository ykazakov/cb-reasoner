package org.semanticweb.cb.reasoner;

/**
 * Corresponds to an <a href=
 * "http://www.w3.org/TR/owl2-syntax/#Intersection_of_Class_Expressions"
 * >Intersection of Class Expressions<a> in the OWL 2 specification.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public class CbObjectIntersectionOf extends CbClassExpression {

	public CbObjectIntersectionOf(CbClassExpression... classes) {
		super(getPtr(classes));
	}

	private static native long getPtr(CbClassExpression[] classes);

	public native CbClassExpression[] getOperands();

	@Override
	public <O> O accept(CbClassExpressionVisitor<O> visitor) {
		return visitor.visit(this);
	}

}
