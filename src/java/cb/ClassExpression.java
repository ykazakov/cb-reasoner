package cb;

/* Class Expressions */
public abstract class ClassExpression {

	static {
		Loader.load();
		init();
	}

	private static native void init();

	private long ptr = 0;

	protected ClassExpression() {
	}
	
	protected ClassExpression(long ptr) {
		this.ptr = ptr;
	}

	protected void finalize() {
		destruct();
	}

	private native synchronized void destruct();
	
	public native void print();

}
