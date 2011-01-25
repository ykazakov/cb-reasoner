package org.semanticweb.cb;

import org.semanticweb.owlapi.reasoner.ReasonerProgressMonitor;

import cb.ProgressMonitor;

public class CBReasonerProgressMonitor implements ProgressMonitor {
	private final ReasonerProgressMonitor pm;
	private static final long updateInterval = 10;
	private static final double mimimalIncrement = 0.005;
	private long lastUpdateTime;
	private double lastProgress;

	public CBReasonerProgressMonitor(ReasonerProgressMonitor pm) {
		this.pm = pm;
	}

	@Override
	public void start(String message) {
		pm.reasonerTaskStarted(message);
		lastProgress = 0;
		lastUpdateTime = System.currentTimeMillis();
	}

	@Override
	public void report(int state, int max) {
		long time = System.currentTimeMillis();
		double progress;
		if (max == 0)
			progress = 0;
		else
			progress = (double)state / (double)max;
		if (time > lastUpdateTime + updateInterval
				&& progress > lastProgress + mimimalIncrement) {
			pm.reasonerTaskProgressChanged(state, max);
			lastUpdateTime = time;
			lastProgress = progress;
		}
	}

	@Override
	public void finish() {
		pm.reasonerTaskStopped();
	}

}
