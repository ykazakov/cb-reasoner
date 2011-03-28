package org.semanticweb.cb.reasoner;

public interface CBClassExpressionVisitorEx<O> {
	O visit(CBClass ce);
	O visit(CBObjectIntersectionOf ce);
	O visit(CBObjectSomeValuesFrom ce);		
}