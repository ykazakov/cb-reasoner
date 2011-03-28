package org.semanticweb.cb.reasoner;

/* InverseObjectProperties axiom */
public class CBInverseObjectPropertiesAxiom extends CBObjectPropertyAxiom {

	public CBInverseObjectPropertiesAxiom(CBObjectPropertyExpression opea,
			CBObjectPropertyExpression opeb) {
		create(opea, opeb);
	}

	private native void create(CBObjectPropertyExpression opea,
			CBObjectPropertyExpression opeb);
	
	public native CBObjectPropertyExpression getFirstProperty();
	public native CBObjectPropertyExpression getSecondProperty();
	
	@Override
	public <O> O accept(CBObjectPropertyAxiomVisitorEx<O> visitor) {
		return visitor.visit(this);
	}

}
