package org.semanticweb.cb.reasoner;

/**
 * Corresponds to an <a href=
 * "http://www.w3.org/TR/owl2-syntax/#Object_Subproperties">Object Subproperty
 * Axiom<a> in the OWL 2 specification.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public class CbSubObjectPropertyOfAxiom extends CbObjectPropertyAxiom {

	public CbSubObjectPropertyOfAxiom(CbObjectPropertyExpression subProperty,
			CbObjectPropertyExpression superProperty) {
		super(getPtr(getSingleton(subProperty), superProperty));
	}

	public CbSubObjectPropertyOfAxiom(
			CbObjectPropertyExpression[] subPropertyChain,
			CbObjectPropertyExpression superProperty) {
		super(getPtr(subPropertyChain, superProperty));
	}

	private static CbObjectPropertyExpression[] getSingleton(
			CbObjectPropertyExpression property) {
		CbObjectPropertyExpression opearr[] = { property };
		return opearr;
	}

	private static native long getPtr(
			CbObjectPropertyExpression[] subPropertyChain,
			CbObjectPropertyExpression superProperty);

	public native CbObjectPropertyExpression[] getSubPropertyChain();

	public native CbObjectPropertyExpression getSuperProperty();

	@Override
	public <O> O accept(CbObjectPropertyAxiomVisitor<O> visitor) {
		return visitor.visit(this);
	}

}
