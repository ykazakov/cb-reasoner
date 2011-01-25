package cb;

/* ObjectIntersectionOf */
public class ObjectIntersectionOf extends ClassExpression {

	public ObjectIntersectionOf(ClassExpression... ces) {
		create(ces);
	}

	private native void create(ClassExpression[] ces);

}
