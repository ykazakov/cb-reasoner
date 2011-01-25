package cb;

/* ObjectSomeValuesFrom */
public class ObjectSomeValuesFrom extends ClassExpression {

	public ObjectSomeValuesFrom(ObjectPropertyExpression ope, ClassExpression ce) {
		create(ope, ce);
	}

	private native void create(ObjectPropertyExpression ope, ClassExpression ce);

}
