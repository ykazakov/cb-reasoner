package cb;

/* Object property */
public class ObjectProperty extends ObjectPropertyExpression {

	public ObjectProperty(String iri) {
		create(iri);
	}

	private native void create(String iri);

	public static final Class TopObjectProperty = new Class(
			"owl:TopObjectProperty");
	public static final Class BottomObjectProperty = new Class(
			"owl:BottomObjectProperty");

}
