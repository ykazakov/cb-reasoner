package org.semanticweb.cb.reasoner;

/**
 * Interface for monitoring progress in processes that can take some time to
 * complete.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public interface ProgressMonitor {

	/**
	 * This method should be called in the beginning of the process. It should
	 * be called only once until the method {@link #finish()} is called.
	 * 
	 * @param message
	 *            a description of the process.
	 */
	public void start(String message);

	/**
	 * Reports the current progress.
	 * 
	 * @param state
	 *            the current value of the progress. Must not be larger then
	 *            {@code max}.
	 * @param maxState
	 *            the maximal (estimated) value of the progress.
	 */
	public void report(int state, int maxState);

	/**
	 * Indicates that the process is finished. Should be be only called if the
	 * method {@link #start(String)} was called before.
	 * 
	 */
	public void finish();
}
