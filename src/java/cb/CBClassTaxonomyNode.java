package org.semanticweb.cb.reasoner;

/* Class Nodes in Taxonomies */
public class CBClassTaxonomyNode extends CBClassNode {
	
	protected synchronized native void destruct();
	
	public native CBClassTaxonomyNode[] getChildNodes();
	public native CBClassTaxonomyNode[] getParentNodes();

	@Override
	public native CBClass[] getCBClasses();
				
	public <O> O accept(CBObjectVisitorEx<O> visitor) {
		return visitor.visit(this);
	}
	
}
