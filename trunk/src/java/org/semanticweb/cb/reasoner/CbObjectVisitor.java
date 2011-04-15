package org.semanticweb.cb.reasoner;

/**
 * Visitor pattern interface for instances of {@link CbObject}.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public interface CbObjectVisitor<O> extends CbAxiomVisitor<O>,
		CbClassExpressionVisitor<O>, CbObjectPropertyExpressionVisitor<O> {
	
	O visit(CbOntology ont);

	O visit(CbClassNode node);
}