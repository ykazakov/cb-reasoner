package cb;

/* ObjectInverseOf property */
public class ObjectInverseOf extends ObjectPropertyExpression {

	public ObjectInverseOf(String iri) {
		create(iri);
	}

	private native void create(String iri);	

}