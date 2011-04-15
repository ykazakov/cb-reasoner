package org.semanticweb.cb.reasoner;

/**
 * Wrapper class for objects created in C through JNI.
 * 
 * @author Yevgeny Kazakov
 * 
 */
public abstract class CObject {

	static {
		Loader.load();
		init();
	}

	private static native void init();

	private final long ptr;

	protected CObject(long ptr) {
		this.ptr = ptr;
	}

	protected abstract void destruct();

	protected void finalize() {
		destruct();
	}

}
