package org.semanticweb.cb.reasoner;

/* InverseFunctionalObjectProperty axiom */
public class CBInverseFunctionalObjectPropertyAxiom extends CBObjectPropertyAxiom {

	public CBInverseFunctionalObjectPropertyAxiom(CBObjectPropertyExpression ope) {		
		create(ope);
	}
	
	private native void create(CBObjectPropertyExpression ope);

	public native CBObjectPropertyExpression getProperty();
	
	@Override
	public <O> O accept(CBObjectPropertyAxiomVisitorEx<O> visitor) {
		return visitor.visit(this);
	}
	
}