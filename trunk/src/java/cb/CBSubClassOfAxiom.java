package org.semanticweb.cb.reasoner;

/* SubClassOf axiom */
public class CBSubClassOfAxiom extends CBClassAxiom {

	public CBSubClassOfAxiom(CBClassExpression cea, CBClassExpression ceb) {
		create(cea, ceb);
	}

	private native void create(CBClassExpression cea, CBClassExpression ceb);
	
	public native CBClassExpression getSubClass();
	public native CBClassExpression getSuperClass();

	@Override
	public <O> O accept(CBClassAxiomVisitorEx<O> visitor) {
		return visitor.visit(this);		
	}

}
