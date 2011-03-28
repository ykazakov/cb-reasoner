package org.semanticweb.cb.reasoner;

/* FunctionalObjectProperty axiom */
public class CBFunctionalObjectPropertyAxiom extends CBObjectPropertyAxiom {

	public CBFunctionalObjectPropertyAxiom(CBObjectPropertyExpression ope) {		
		create(ope);
	}
	
	private native void create(CBObjectPropertyExpression ope);
	
	public native CBObjectPropertyExpression getProperty();

	@Override
	public <O> O accept(CBObjectPropertyAxiomVisitorEx<O> visitor) {		
		return visitor.visit(this);
	}
	
}