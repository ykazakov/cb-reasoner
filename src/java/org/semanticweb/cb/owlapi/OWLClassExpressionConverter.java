package org.semanticweb.cb.owlapi;

import java.util.List;

import org.semanticweb.cb.reasoner.CbClass;
import org.semanticweb.cb.reasoner.CbClassExpression;
import org.semanticweb.cb.reasoner.CbObjectIntersectionOf;
import org.semanticweb.cb.reasoner.CbObjectSomeValuesFrom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLClassExpressionVisitorEx;
import org.semanticweb.owlapi.model.OWLDataAllValuesFrom;
import org.semanticweb.owlapi.model.OWLDataExactCardinality;
import org.semanticweb.owlapi.model.OWLDataHasValue;
import org.semanticweb.owlapi.model.OWLDataMaxCardinality;
import org.semanticweb.owlapi.model.OWLDataMinCardinality;
import org.semanticweb.owlapi.model.OWLDataSomeValuesFrom;
import org.semanticweb.owlapi.model.OWLObjectAllValuesFrom;
import org.semanticweb.owlapi.model.OWLObjectComplementOf;
import org.semanticweb.owlapi.model.OWLObjectExactCardinality;
import org.semanticweb.owlapi.model.OWLObjectHasSelf;
import org.semanticweb.owlapi.model.OWLObjectHasValue;
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf;
import org.semanticweb.owlapi.model.OWLObjectMaxCardinality;
import org.semanticweb.owlapi.model.OWLObjectMinCardinality;
import org.semanticweb.owlapi.model.OWLObjectOneOf;
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom;
import org.semanticweb.owlapi.model.OWLObjectUnionOf;

public final class OWLClassExpressionConverter implements
		OWLClassExpressionVisitorEx<CbClassExpression> {
	
	private static final OWLClassExpressionConverter converter = new OWLClassExpressionConverter();
	private OWLClassExpressionConverter() {		
	}
	
	static OWLClassExpressionConverter getInstance() {
		return converter;
	}

	@Override
	public CbClass visit(OWLClass ce) {
		if (ce.isOWLThing())
			return CbClass.CbOwlThing;
		else if (ce.isOWLNothing())
			return CbClass.CbOwlNothing;
		else return new CbClass(ce.getIRI().toString());
	}

	@Override
	public CbObjectIntersectionOf visit(OWLObjectIntersectionOf ce) {
		List<OWLClassExpression> owlConjuncts = ce.getOperandsAsList();
		int length = owlConjuncts.size();
		CbClassExpression[] cbConjuncts = new CbClassExpression[length];
		int i = 0;
		for (OWLClassExpression cce : owlConjuncts) {
			cbConjuncts[i] = cce.accept(this);
			i++;
		}
		return new CbObjectIntersectionOf(cbConjuncts);
	}

	@Override
	public CbClassExpression visit(OWLObjectUnionOf ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CbClassExpression visit(OWLObjectComplementOf ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CbObjectSomeValuesFrom visit(OWLObjectSomeValuesFrom ce) {
		OWLPropertyExpressionConverter peConverter = 
			OWLPropertyExpressionConverter.getInstance(); 
		return new CbObjectSomeValuesFrom(ce.getProperty().accept(peConverter), 
				ce.getFiller().accept(this));		
	}

	@Override
	public CbClassExpression visit(OWLObjectAllValuesFrom ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CbClassExpression visit(OWLObjectHasValue ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CbClassExpression visit(OWLObjectMinCardinality ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CbClassExpression visit(OWLObjectExactCardinality ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CbClassExpression visit(OWLObjectMaxCardinality ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CbClassExpression visit(OWLObjectHasSelf ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CbClassExpression visit(OWLObjectOneOf ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CbClassExpression visit(OWLDataSomeValuesFrom ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CbClassExpression visit(OWLDataAllValuesFrom ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CbClassExpression visit(OWLDataHasValue ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CbClassExpression visit(OWLDataMinCardinality ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CbClassExpression visit(OWLDataExactCardinality ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CbClassExpression visit(OWLDataMaxCardinality ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

}
