package org.semanticweb.cb.reasoner;

/* TransitiveObjectProperty axiom */
public class CBTransitiveObjectPropertyAxiom extends CBObjectPropertyAxiom {

	public CBTransitiveObjectPropertyAxiom(CBObjectPropertyExpression ope) {		
		create(ope);
	}
	
	private native void create(CBObjectPropertyExpression ope);
	
	public native CBObjectPropertyExpression getProperty();

	@Override
	public <O> O accept(CBObjectPropertyAxiomVisitorEx<O> visitor) {
		return visitor.visit(this);
	}
}