package org.semanticweb.cb.reasoner;

/* SubObjectPropertyOf axiom */
public class CBSubObjectPropertyOfAxiom extends CBObjectPropertyAxiom {

	public CBSubObjectPropertyOfAxiom(CBObjectPropertyExpression opea,
			CBObjectPropertyExpression opeb) {
		CBObjectPropertyExpression opearr[] = { opea };
		create(opearr, opeb);
	}
	
	public CBSubObjectPropertyOfAxiom(CBObjectPropertyExpression[] opearr,
			CBObjectPropertyExpression opeb) {
		create(opearr, opeb);
	}

	private native void create(CBObjectPropertyExpression[] opearr,
			CBObjectPropertyExpression opeb);

	public native CBObjectPropertyExpression[] getSubPropertyChain();
	public native CBObjectPropertyExpression getSuperProperty();
	
	@Override
	public <O> O accept(CBObjectPropertyAxiomVisitorEx<O> visitor) {
		return visitor.visit(this);
	}

}
