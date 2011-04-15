package org.semanticweb.cb.owlapi;

import org.semanticweb.cb.reasoner.CbClassAxiomVisitor;
import org.semanticweb.cb.reasoner.CbEquivalentClassesAxiom;
import org.semanticweb.cb.reasoner.CbSubClassOfAxiom;
import org.semanticweb.owlapi.model.OWLClassAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom;
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom;

public final class CbClassAxiomConverter implements
		CbClassAxiomVisitor<OWLClassAxiom> {

	@Override
	public OWLEquivalentClassesAxiom visit(CbEquivalentClassesAxiom ax) {
		// TODO Auto-generated method stub
		throw new ConverterException("Not yet implemented.");
	}

	@Override
	public OWLSubClassOfAxiom visit(CbSubClassOfAxiom ax) {
		// TODO Auto-generated method stub
		throw new ConverterException("Not yet implemented.");
	}

}
