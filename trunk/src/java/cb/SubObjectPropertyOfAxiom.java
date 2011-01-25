package cb;

/* SubObjectPropertyOf axiom */
public class SubObjectPropertyOfAxiom extends ObjectPropertyAxiom {

	public SubObjectPropertyOfAxiom(ObjectPropertyExpression opea,
			ObjectPropertyExpression opeb) {
		ObjectPropertyExpression opearr[] = { opea };
		create(opearr, opeb);
	}
	
	public SubObjectPropertyOfAxiom(ObjectPropertyExpression[] opearr,
			ObjectPropertyExpression opeb) {
		create(opearr, opeb);
	}

	private native void create(ObjectPropertyExpression[] opearr,
			ObjectPropertyExpression opeb);

}
