package org.semanticweb.cb.reasoner;

/**
 * Visitor pattern interface for instances of {@link CbObjectPropertyAxiom}.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public interface CbObjectPropertyAxiomVisitor<O> {
	
	O visit(CbFunctionalObjectPropertyAxiom ax);

	O visit(CbInverseFunctionalObjectPropertyAxiom ax);

	O visit(CbInverseObjectPropertiesAxiom ax);

	O visit(CbSubObjectPropertyOfAxiom ax);

	O visit(CbTransitiveObjectPropertyAxiom ax);
}
