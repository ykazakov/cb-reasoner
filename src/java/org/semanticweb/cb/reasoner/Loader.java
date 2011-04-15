package org.semanticweb.cb.reasoner;

/**
 * Class for loading CB native library. Every class that uses native methods
 * from this library should call the {@link Loader.load()} method.
 * 
 * 
 * @author Yevgeny Kazakov
 * 
 */
class Loader {
	
	private static boolean loaded = false;

	static void load() {
		if (loaded)
			return;
		try {
			System.loadLibrary("jcb");
			loaded = true;
		} catch (UnsatisfiedLinkError e) {
			System.err.println("CB native code library failed to load.\n" + e);
		}
	}
}
