package cb;

/* Class */
public class Class extends ClassExpression {

	public Class(String iri) {
		create(iri);		
	}
	
	public Class(long ptr) {
		super(ptr);
	}
	
	private native void create(String iri);
	
	public static final Class Thing = new Class("owl:Thing");
	public static final Class Nohing = new Class("owl:Nothing");
	
}

