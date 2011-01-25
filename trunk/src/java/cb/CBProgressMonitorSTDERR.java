package cb;

public class CBProgressMonitorSTDERR extends CBProgressMonitor {
	public CBProgressMonitorSTDERR() {
		create();
	}

	private native void create();
	
	@Override
	public native void start(String message);
		
	@Override
	public native void report(int state, int max);
	
	@Override
	public native void finish();
	
}
