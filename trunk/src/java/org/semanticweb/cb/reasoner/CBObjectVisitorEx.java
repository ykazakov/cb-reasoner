package org.semanticweb.cb.reasoner;

public interface CBObjectVisitorEx<O> extends CBAxiomVisitorEx<O>,
		CBClassExpressionVisitorEx<O>, CBObjectPropertyExpressionVisitorEx<O> {		
	O visit(CBOntology ont);
	O visit(CBClassNode node);
}