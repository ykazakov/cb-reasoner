package org.semanticweb.cb.reasoner;

public interface CBObjectPropertyExpressionVisitorEx<O> {
	O visit(CBObjectProperty ope);
	O visit(CBObjectInverseOf ope);
}
