package org.semanticweb.cb.reasoner;



/**
 * @author ecull
 *
 */
public class CBOntology extends CBObject {
	
	public CBOntology() {
		create();
	}

	private native void create();

	protected native synchronized void destruct();
		
	public void add(CBAxiom ax) {
		ax.addTo(this);
	}
	
	public void remove(CBAxiom ax) {
		ax.removeFrom(this);
	}
	
	protected native void addDeclarationAxiom(CBDeclarationAxiom ax);
	protected native void removeDeclarationAxiom(CBDeclarationAxiom ax);
	protected native void addClassAxiom(CBClassAxiom ax);
	protected native void removeClassAxiom(CBClassAxiom ax);
	protected native void addObjectPropertyAxiom(CBObjectPropertyAxiom ax);
	protected native void removeObjectPropertyAxiom(CBObjectPropertyAxiom ax);
	
	public native void printInfo();	
	public native void classify();
	private native void classifyPm(ProgressMonitor pm);
	public void classify(ProgressMonitor pm) {
		classifyPm(pm);
	}
		
	public CBClassAxiomIteratable getClassAxioms() {				
		return new CBClassAxiomIteratable(this);		
	}
		
	public native CBClassTaxonomyNode getClassTaxonomyNode(CBClass cls);
	
	public CBClassTaxonomyNode getTopNode() {
        return getClassTaxonomyNode(CBClass.Thing);	
	}	
	
	public CBClassTaxonomyNode getBotNode() {
		return getClassTaxonomyNode(CBClass.Nothing);	
	}

	@Override
	public <O> O accept(CBObjectVisitorEx<O> visitor) {
		return visitor.visit(this);
	}
	
}
