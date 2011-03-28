package org.semanticweb.cb.reasoner;

public class DummyProgressMonitor implements ProgressMonitor {

	@Override
	public void start(String message) {
	}

	@Override
	public void report(int state, int max) {
	}

	@Override
	public void finish() {
	}

}
