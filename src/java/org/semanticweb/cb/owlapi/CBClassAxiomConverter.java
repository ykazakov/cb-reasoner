package org.semanticweb.cb.owlapi;

import org.semanticweb.cb.reasoner.CBClassAxiomVisitorEx;
import org.semanticweb.cb.reasoner.CBEquivalentClassesAxiom;
import org.semanticweb.cb.reasoner.CBSubClassOfAxiom;
import org.semanticweb.owlapi.model.OWLClassAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom;
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom;

public final class CBClassAxiomConverter implements
		CBClassAxiomVisitorEx<OWLClassAxiom> {

	@Override
	public OWLEquivalentClassesAxiom visit(CBEquivalentClassesAxiom ax) {
		// TODO Auto-generated method stub
		throw new ConverterException("Not yet implemented.");
	}

	@Override
	public OWLSubClassOfAxiom visit(CBSubClassOfAxiom ax) {
		// TODO Auto-generated method stub
		throw new ConverterException("Not yet implemented.");
	}

}
