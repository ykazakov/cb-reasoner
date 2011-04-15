package org.semanticweb.cb.reasoner;

/**
 * Visitor pattern interface for instances of {@link CbClassAxiom}.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public interface CbClassAxiomVisitor<O> {

	O visit(CbEquivalentClassesAxiom ax);

	O visit(CbSubClassOfAxiom ax);
}
