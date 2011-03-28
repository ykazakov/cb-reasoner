package org.semanticweb.cb.reasoner;

public abstract class CObject {
	
	static {
		Loader.load();
		init();
	}

	private static native void init();

	private long ptr = 0;

	protected CObject() {
	}
	
	protected CObject(long ptr) {
		this.ptr = ptr;
	}
	
	protected abstract void destruct();

	protected void finalize() {		
		destruct();
	}

}
