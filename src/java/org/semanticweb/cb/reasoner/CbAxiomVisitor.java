package org.semanticweb.cb.reasoner;

/**
 * Visitor pattern interface for instances of {@link CbAxiom}.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public interface CbAxiomVisitor<O> extends CbClassAxiomVisitor<O>,
		CbObjectPropertyAxiomVisitor<O>, CbDeclarationAxiomVisitor<O> {

}
