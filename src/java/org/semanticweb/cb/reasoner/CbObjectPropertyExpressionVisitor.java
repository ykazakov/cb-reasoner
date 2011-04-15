package org.semanticweb.cb.reasoner;

/**
 * Visitor pattern interface for instances of {@link CbObjectPropertyExpression}
 * .
 * 
 * @author Yevgeny Kazakov
 * 
 */
public interface CbObjectPropertyExpressionVisitor<O> {

	O visit(CbObjectProperty ope);

	O visit(CbObjectInverseOf ope);
}
