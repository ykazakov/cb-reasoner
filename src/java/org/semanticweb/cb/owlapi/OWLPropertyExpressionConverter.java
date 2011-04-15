package org.semanticweb.cb.owlapi;

import org.semanticweb.cb.reasoner.CbObjectInverseOf;
import org.semanticweb.cb.reasoner.CbObjectProperty;
import org.semanticweb.cb.reasoner.CbObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLObjectInverseOf;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLPropertyExpressionVisitorEx;

public final class OWLPropertyExpressionConverter implements
		OWLPropertyExpressionVisitorEx<CbObjectPropertyExpression> {
			
	private static final OWLPropertyExpressionConverter converter = new OWLPropertyExpressionConverter();
	private OWLPropertyExpressionConverter() {		
	}
	
	static OWLPropertyExpressionConverter getInstance() {
		return converter;
	}

	@Override
	public CbObjectProperty visit(OWLObjectProperty op) {
		return new CbObjectProperty(op.getIRI().toString());
	}

	@Override
	public CbObjectInverseOf visit(OWLObjectInverseOf op) {
		return new CbObjectInverseOf(op.getNamedProperty().getIRI().toString());
	}

	@Override
	public CbObjectPropertyExpression visit(OWLDataProperty op) {
		// TODO Auto-generated method stub
		throw new ConverterException(op.getEntityType().getName() + " not supported");
	}

}
