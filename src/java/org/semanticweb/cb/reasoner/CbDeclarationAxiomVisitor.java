package org.semanticweb.cb.reasoner;

/**
 * Visitor pattern interface for instances of {@link CbDeclarationAxiom}.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public interface CbDeclarationAxiomVisitor<O> {
	
	O visit(CbClassDeclarationAxiom ax);

	O visit(CbObjectPropertyDeclarationAxiom ax);
}
