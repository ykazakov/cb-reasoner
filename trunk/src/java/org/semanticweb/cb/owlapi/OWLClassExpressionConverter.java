package org.semanticweb.cb.owlapi;

import java.util.List;

import org.semanticweb.cb.reasoner.CBClass;
import org.semanticweb.cb.reasoner.CBClassExpression;
import org.semanticweb.cb.reasoner.CBObjectIntersectionOf;
import org.semanticweb.cb.reasoner.CBObjectSomeValuesFrom;
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
		OWLClassExpressionVisitorEx<CBClassExpression> {
	
	private static final OWLClassExpressionConverter converter = new OWLClassExpressionConverter();
	private OWLClassExpressionConverter() {		
	}
	
	static OWLClassExpressionConverter getInstance() {
		return converter;
	}

	@Override
	public CBClass visit(OWLClass ce) {
		if (ce.isOWLThing())
			return CBClass.Thing;
		else if (ce.isOWLNothing())
			return CBClass.Nothing;
		else return new CBClass(ce.getIRI().toString());
	}

	@Override
	public CBObjectIntersectionOf visit(OWLObjectIntersectionOf ce) {
		List<OWLClassExpression> owlConjuncts = ce.getOperandsAsList();
		int length = owlConjuncts.size();
		CBClassExpression[] cbConjuncts = new CBClassExpression[length];
		int i = 0;
		for (OWLClassExpression cce : owlConjuncts) {
			cbConjuncts[i] = cce.accept(this);
			i++;
		}
		return new CBObjectIntersectionOf(cbConjuncts);
	}

	@Override
	public CBClassExpression visit(OWLObjectUnionOf ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CBClassExpression visit(OWLObjectComplementOf ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CBObjectSomeValuesFrom visit(OWLObjectSomeValuesFrom ce) {
		OWLPropertyExpressionConverter peConverter = 
			OWLPropertyExpressionConverter.getInstance(); 
		return new CBObjectSomeValuesFrom(ce.getProperty().accept(peConverter), 
				ce.getFiller().accept(this));		
	}

	@Override
	public CBClassExpression visit(OWLObjectAllValuesFrom ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CBClassExpression visit(OWLObjectHasValue ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CBClassExpression visit(OWLObjectMinCardinality ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CBClassExpression visit(OWLObjectExactCardinality ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CBClassExpression visit(OWLObjectMaxCardinality ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CBClassExpression visit(OWLObjectHasSelf ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CBClassExpression visit(OWLObjectOneOf ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CBClassExpression visit(OWLDataSomeValuesFrom ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CBClassExpression visit(OWLDataAllValuesFrom ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CBClassExpression visit(OWLDataHasValue ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CBClassExpression visit(OWLDataMinCardinality ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CBClassExpression visit(OWLDataExactCardinality ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

	@Override
	public CBClassExpression visit(OWLDataMaxCardinality ce) {
		// TODO Auto-generated method stub
		throw new ConverterException(ce.getClassExpressionType().getName() + " not supported");
	}

}
