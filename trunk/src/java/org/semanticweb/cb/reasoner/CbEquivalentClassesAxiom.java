package org.semanticweb.cb.reasoner;

/**
 * Corresponds to an <a href=
 * "http://www.w3.org/TR/owl2-syntax/#Equivalent_Classes">Equivalent Class
 * Axiom<a> in the OWL 2 specification.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public class CbEquivalentClassesAxiom extends CbClassAxiom {
		
	public CbEquivalentClassesAxiom(CbClassExpression... classes) {
		super(getPtr(classes));
	}
	
	private static native long getPtr(CbClassExpression[] classes);

	public native CbClassExpression[] getClassExpressions();

	@Override
	public <O> O accept(CbClassAxiomVisitor<O> visitor) {
		return visitor.visit(this);
	}
		
}
