package org.semanticweb.cb.owlapi;

import org.semanticweb.cb.reasoner.CBObjectInverseOf;
import org.semanticweb.cb.reasoner.CBObjectProperty;
import org.semanticweb.cb.reasoner.CBObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLObjectInverseOf;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLPropertyExpressionVisitorEx;

public final class OWLPropertyExpressionConverter implements
		OWLPropertyExpressionVisitorEx<CBObjectPropertyExpression> {
			
	private static final OWLPropertyExpressionConverter converter = new OWLPropertyExpressionConverter();
	private OWLPropertyExpressionConverter() {		
	}
	
	static OWLPropertyExpressionConverter getInstance() {
		return converter;
	}

	@Override
	public CBObjectProperty visit(OWLObjectProperty op) {
		return new CBObjectProperty(op.getIRI().toString());
	}

	@Override
	public CBObjectInverseOf visit(OWLObjectInverseOf op) {
		return new CBObjectInverseOf(op.getNamedProperty().getIRI().toString());
	}

	@Override
	public CBObjectPropertyExpression visit(OWLDataProperty op) {
		// TODO Auto-generated method stub
		throw new ConverterException(op.getEntityType().getName() + " not supported");
	}

}
