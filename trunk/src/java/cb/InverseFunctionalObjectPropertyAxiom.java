package cb;

/* InverseFunctionalObjectProperty axiom */
public class InverseFunctionalObjectPropertyAxiom extends ObjectPropertyAxiom {

	public InverseFunctionalObjectPropertyAxiom(ObjectPropertyExpression ope) {		
		create(ope);
	}
	
	private native void create(ObjectPropertyExpression ope);
}