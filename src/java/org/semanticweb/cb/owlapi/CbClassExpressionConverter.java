package org.semanticweb.cb.owlapi;

import org.semanticweb.cb.reasoner.CbClass;
import org.semanticweb.cb.reasoner.CbClassExpressionVisitor;
import org.semanticweb.cb.reasoner.CbObjectIntersectionOf;
import org.semanticweb.cb.reasoner.CbObjectSomeValuesFrom;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf;
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom;

public final class CbClassExpressionConverter implements
		CbClassExpressionVisitor<OWLClassExpression> {
	
	final OWLDataFactory owlDataFactory = OWLManager.getOWLDataFactory();	
	private static final CbClassExpressionConverter converter = new CbClassExpressionConverter();
	
	private CbClassExpressionConverter() {		
	}
	
	static CbClassExpressionConverter getInstance() {
		return converter;
	}
	
	@Override
	public OWLClass visit(CbClass ce) {		
		String iri = ce.getIRI();
		if (iri.equals("owl:Thing")) 
			return owlDataFactory.getOWLThing();
		else if (iri.equals("owl:Nothing")) 
			return owlDataFactory.getOWLNothing();
		else return owlDataFactory.getOWLClass(IRI.create(iri));		
	}

	@Override
	public OWLObjectIntersectionOf visit(CbObjectIntersectionOf ce) {
		// TODO Auto-generated method stub
		throw new ConverterException("Not yet implemented.");
	}

	@Override
	public OWLObjectSomeValuesFrom visit(CbObjectSomeValuesFrom ce) {
		// TODO Auto-generated method stub
		throw new ConverterException("Not yet implemented.");
	}

}
