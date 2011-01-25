package cb;

/* SubClassOf axiom */
public class SubClassOfAxiom extends ClassAxiom {

	public SubClassOfAxiom(ClassExpression cea, ClassExpression ceb) {
		create(cea, ceb);
	}

	private native void create(ClassExpression cea, ClassExpression ceb);

}
