package cb;

/* Class Axioms */
public abstract class ClassAxiom extends Axiom {

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
	
	protected void addTo(Ontology cbOntology) {
		cbOntology.addcax(this);
	}

}