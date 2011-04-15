package org.semanticweb.cb.reasoner;

public abstract class CbClassNode extends CbObject {
	
	/**
	 * @param ptr
	 */
	protected CbClassNode(long ptr) {
		super(ptr);
	}

	public abstract CbClass[] getClasses();
}
