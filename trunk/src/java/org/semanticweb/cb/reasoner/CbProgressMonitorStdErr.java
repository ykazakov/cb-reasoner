package org.semanticweb.cb.reasoner;

/**
 * A monitor for progress in CB reasoner processes printed to the standard
 * error channel.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public class CbProgressMonitorStdErr extends CbProgressMonitor {

	private static native long getPtr();

	public CbProgressMonitorStdErr() {
		super(getPtr());
	}

	@Override
	public native void start(String message);

	@Override
	public native void report(int state, int maxState);

	@Override
	public native void finish();

}
