package org.semanticweb.cb.reasoner;

public interface CBClassAxiomVisitorEx<O> {
	O visit(CBEquivalentClassesAxiom ax);
	O visit(CBSubClassOfAxiom ax);
}
