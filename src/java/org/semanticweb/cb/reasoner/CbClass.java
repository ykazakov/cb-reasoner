package org.semanticweb.cb.reasoner;

/**
 * Corresponds to a <a href=
 * "http://www.w3.org/TR/owl2-syntax/#Classes">Class<a> in the OWL 2
 * specification.
 * 
 * @author Yevgeny Kazakov
 */
public class CbClass extends CbClassExpression {	

	public CbClass(String iri) {
		super(getPtr(iri));		
	}
	
	private static native long getPtr(String iri);
	
	public static final CbClass CbOwlThing = new CbClass("owl:Thing");
	public static final CbClass CbOwlNothing = new CbClass("owl:Nothing");
	
	public native String getIRI();

	@Override
	public <O> O accept(CbClassExpressionVisitor<O> visitor) {
		return visitor.visit(this);
	}			
		
}

