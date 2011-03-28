package org.semanticweb.cb.reasoner;

public interface CBObjectPropertyAxiomVisitorEx<O> {
	O visit(CBFunctionalObjectPropertyAxiom ax);
	O visit(CBInverseFunctionalObjectPropertyAxiom ax);
	O visit(CBInverseObjectPropertiesAxiom ax);
	O visit(CBSubObjectPropertyOfAxiom ax);
	O visit(CBTransitiveObjectPropertyAxiom ax);
}
