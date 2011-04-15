package org.semanticweb.cb.reasoner;

/**
 * Corresponds to an <a href=
 * "http://www.w3.org/TR/owl2-syntax/#Ontologies">Ontology<a> in the OWL 2
 * specification.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public class CbOntology extends CbObject {

	public CbOntology() {
		super(getPtr());
	}

	private static native long getPtr();

	protected native synchronized void destruct();

	public void add(CbAxiom axiom) {
		axiom.addTo(this);
	}

	public void remove(CbAxiom axiom) {
		axiom.removeFrom(this);
	}

	protected native void addDeclarationAxiom(CbDeclarationAxiom axiom);

	protected native void removeDeclarationAxiom(CbDeclarationAxiom axiom);

	protected native void addClassAxiom(CbClassAxiom axiom);

	protected native void removeClassAxiom(CbClassAxiom axiom);

	protected native void addObjectPropertyAxiom(CbObjectPropertyAxiom axiom);

	protected native void removeObjectPropertyAxiom(CbObjectPropertyAxiom axiom);

	public native void printInfo();

	public native void classify();

	private native void classifyPm(ProgressMonitor monitor);

	public void classify(ProgressMonitor monitor) {
		classifyPm(monitor);
	}

	public CbClassAxiomIteratable getClassAxioms() {
		return new CbClassAxiomIteratable(this);
	}

	public native long getClassTaxonomyNodePtr(CbClass cbClass);

	public CbClassTaxonomyNode getClassTaxonomyNode(CbClass cbClass) {
		return new CbClassTaxonomyNode(getClassTaxonomyNodePtr(cbClass));
	}

	public CbClassTaxonomyNode getTopNode() {
		return getClassTaxonomyNode(CbClass.CbOwlThing);
	}

	public CbClassTaxonomyNode getBotNode() {
		return getClassTaxonomyNode(CbClass.CbOwlNothing);
	}

	@Override
	public <O> O accept(CbObjectVisitor<O> visitor) {
		return visitor.visit(this);
	}

}
