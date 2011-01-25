package cb;

/* FunctionalObjectProperty axiom */
public class FunctionalObjectPropertyAxiom extends ObjectPropertyAxiom {

	public FunctionalObjectPropertyAxiom(ObjectPropertyExpression ope) {		
		create(ope);
	}
	
	private native void create(ObjectPropertyExpression ope);
}