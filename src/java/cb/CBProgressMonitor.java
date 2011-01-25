package cb;

public abstract class CBProgressMonitor implements ProgressMonitor {
	static {
		Loader.load();
		init();
	}

	private static native void init();

	private long ptr = 0;

	protected void finalize() {
		destruct();
	}

	private native synchronized void destruct();	

}
