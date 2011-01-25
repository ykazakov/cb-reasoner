package cb;

/* Object Property Expressions */
public abstract class ObjectPropertyExpression {

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
	
	public native void print();

}
