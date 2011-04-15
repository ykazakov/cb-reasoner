package org.semanticweb.cb.reasoner;

/**
 * Corresponds to an <a href=
 * "http://www.w3.org/TR/owl2-syntax/#Object_Properties">Object Property<a> in
 * the OWL 2 specification.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public class CbObjectProperty extends CbObjectPropertyExpression {

	public CbObjectProperty(String iri) {
		super(getPtr(iri));
	}

	private static native long getPtr(String iri);

	public static final CbClass CbOwlTopObjectProperty = new CbClass(
			"owl:TopObjectProperty");
	public static final CbClass CbOwlBottomObjectProperty = new CbClass(
			"owl:BottomObjectProperty");

	public native String getIRI();

	public <O> O accept(CbObjectPropertyExpressionVisitor<O> visitor) {
		return visitor.visit(this);
	}

}
