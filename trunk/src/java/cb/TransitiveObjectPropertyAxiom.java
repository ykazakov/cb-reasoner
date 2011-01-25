package cb;

/* TransitiveObjectProperty axiom */
public class TransitiveObjectPropertyAxiom extends ObjectPropertyAxiom {

	public TransitiveObjectPropertyAxiom(ObjectPropertyExpression ope) {		
		create(ope);
	}
	
	private native void create(ObjectPropertyExpression ope);
}