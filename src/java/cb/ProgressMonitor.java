package cb;

public interface ProgressMonitor {
	
	public void start(String message);
	public void report(int state, int max);
	public void finish();
}
