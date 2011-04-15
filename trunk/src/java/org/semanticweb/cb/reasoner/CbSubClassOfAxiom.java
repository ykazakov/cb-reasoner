package org.semanticweb.cb.reasoner;

/**
 * Corresponds to a <a href=
 * "http://www.w3.org/TR/owl2-syntax/#Subclass_Axioms">Subclass Axiom<a> in the
 * OWL 2 specification.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public class CbSubClassOfAxiom extends CbClassAxiom {

	public CbSubClassOfAxiom(CbClassExpression subClass,
			CbClassExpression superClass) {
		super(getPtr(subClass, superClass));
	}

	private static native long getPtr(CbClassExpression subClass,
			CbClassExpression superClass);

	public native CbClassExpression getSubClass();

	public native CbClassExpression getSuperClass();

	@Override
	public <O> O accept(CbClassAxiomVisitor<O> visitor) {
		return visitor.visit(this);
	}

}
