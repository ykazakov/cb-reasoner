package org.semanticweb.cb.owlapi;

import org.semanticweb.cb.reasoner.CBClass;
import org.semanticweb.cb.reasoner.CBClassExpressionVisitorEx;
import org.semanticweb.cb.reasoner.CBObjectIntersectionOf;
import org.semanticweb.cb.reasoner.CBObjectSomeValuesFrom;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf;
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom;

public final class CBClassExpressionConverter implements
		CBClassExpressionVisitorEx<OWLClassExpression> {
	
	final OWLDataFactory owlDataFactory = OWLManager.getOWLDataFactory();	
	private static final CBClassExpressionConverter converter = new CBClassExpressionConverter();
	
	private CBClassExpressionConverter() {		
	}
	
	static CBClassExpressionConverter getInstance() {
		return converter;
	}
	
	@Override
	public OWLClass visit(CBClass ce) {		
		String iri = ce.getIRI();
		if (iri.equals("owl:Thing")) 
			return owlDataFactory.getOWLThing();
		else if (iri.equals("owl:Nothing")) 
			return owlDataFactory.getOWLNothing();
		else return owlDataFactory.getOWLClass(IRI.create(iri));		
	}

	@Override
	public OWLObjectIntersectionOf visit(CBObjectIntersectionOf ce) {
		// TODO Auto-generated method stub
		throw new ConverterException("Not yet implemented.");
	}

	@Override
	public OWLObjectSomeValuesFrom visit(CBObjectSomeValuesFrom ce) {
		// TODO Auto-generated method stub
		throw new ConverterException("Not yet implemented.");
	}

}
