package org.semanticweb.cb.reasoner;

/**
 * @author Yevgeny Kazakov
 * 
 */
public abstract class CbObject extends CObject {

	/**
	 * @param ptr
	 */
	protected CbObject(long ptr) {
		super(ptr);
	}

	public abstract <O> O accept(CbObjectVisitor<O> visitor);

}
