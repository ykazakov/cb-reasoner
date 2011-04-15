package org.semanticweb.cb.reasoner;

/**
 * A progress monitor that does nothing.
 * 
 * 
 * @author Yevgeny Kazakov
 * 
 */
public class DummyProgressMonitor implements ProgressMonitor {

	@Override
	public void start(String message) {
	}

	@Override
	public void report(int state, int maxState) {
	}

	@Override
	public void finish() {
	}

}
