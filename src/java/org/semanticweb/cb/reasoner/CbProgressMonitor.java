package org.semanticweb.cb.reasoner;

/**
 * Monitoring progress for CB reasoner processes.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public abstract class CbProgressMonitor extends CObject implements
		ProgressMonitor {

	/**
	 * @param ptr
	 */
	protected CbProgressMonitor(long ptr) {
		super(ptr);
	}

	protected synchronized native void destruct();
}
