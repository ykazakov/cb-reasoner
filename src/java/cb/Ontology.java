package cb;

/**
 * @author ecull
 *
 */
public class Ontology {

	static {
		Loader.load();
		init();
	}

	private static native void init();

	private long ptr = 0;

	public Ontology() {
		create();
	}

	protected void finalize() {
		destruct();
	}

	private native void create();

	private native synchronized void destruct();
		
	public void add(Axiom ax) {
		ax.addTo(this);
	}
	
	protected native void addcax(ClassAxiom ax);
	protected native void addopax(ObjectPropertyAxiom ax);
	
	public native void printInfo();	
	public native void classify();
	public native void classifyPm(ProgressMonitor pm);
	public void classify(ProgressMonitor pm) {
		classifyPm(pm);
	}
	
//	public native ClassExpression[] getEquivalentClasses(ClassExpression ce);
//	public native ClassExpression[] getDirectSubClassRepr(ClassExpression ce);
//	public native ClassExpression[] getDirectSupClassRepr(ClassExpression ce);	
}
