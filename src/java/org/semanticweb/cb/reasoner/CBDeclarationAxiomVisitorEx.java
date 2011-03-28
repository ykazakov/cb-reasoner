package org.semanticweb.cb.reasoner;

public interface CBDeclarationAxiomVisitorEx<O> {	
	O visit(CBClassDeclarationAxiom ax);
	O visit(CBObjectPropertyDeclarationAxiom ax);
}
