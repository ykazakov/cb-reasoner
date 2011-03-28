package org.semanticweb.cb.reasoner;

/* EquivalentClasses axiom */
public class CBEquivalentClassesAxiom extends CBClassAxiom {

	public CBEquivalentClassesAxiom(CBClassExpression... ces) {
		create(ces);
	}

	private native void create(CBClassExpression[] ces);
	
	public native CBClassExpression[] getClassExpressions();

	@Override
	public <O> O accept(CBClassAxiomVisitorEx<O> visitor) {
		return visitor.visit(this);
	}
		
}
