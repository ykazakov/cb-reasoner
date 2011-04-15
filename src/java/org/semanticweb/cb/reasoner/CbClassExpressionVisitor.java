package org.semanticweb.cb.reasoner;

/**
 * Visitor pattern interface for instances of {@link CbClassExpression}.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public interface CbClassExpressionVisitor<O> {

	O visit(CbClass ce);

	O visit(CbObjectIntersectionOf ce);

	O visit(CbObjectSomeValuesFrom ce);
}