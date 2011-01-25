package cb;

/* EquivalentClasses axiom */
public class EquivalentClassesAxiom extends ClassAxiom {

	public EquivalentClassesAxiom(ClassExpression... ces) {
		create(ces);
	}

	private native void create(ClassExpression[] ces);

}
