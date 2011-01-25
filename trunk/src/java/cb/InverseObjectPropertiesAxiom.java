package cb;

/* InverseObjectProperties axiom */
public class InverseObjectPropertiesAxiom extends ObjectPropertyAxiom {

	public InverseObjectPropertiesAxiom(ObjectPropertyExpression opea,
			ObjectPropertyExpression opeb) {
		create(opea, opeb);
	}

	private native void create(ObjectPropertyExpression opea,
			ObjectPropertyExpression opeb);

}
