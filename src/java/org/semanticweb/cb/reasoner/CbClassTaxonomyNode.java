package org.semanticweb.cb.reasoner;

/* Class Nodes in Taxonomies */
public class CbClassTaxonomyNode extends CbClassNode {
	
	/**
	 * @param ptr
	 */
	protected CbClassTaxonomyNode(long ptr) {
		super(ptr);
	}

	protected synchronized native void destruct();
	
	public native CbClassTaxonomyNode[] getChildNodes();
	
	public native CbClassTaxonomyNode[] getParentNodes();

	@Override
	public native CbClass[] getClasses();
				
	public <O> O accept(CbObjectVisitor<O> visitor) {
		return visitor.visit(this);
	}
	
}
