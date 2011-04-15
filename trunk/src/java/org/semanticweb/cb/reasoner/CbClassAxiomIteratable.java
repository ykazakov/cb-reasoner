package org.semanticweb.cb.reasoner;

import java.util.Iterator;

public class CbClassAxiomIteratable implements Iterable<CbClassAxiom> {
	
	CbClassAxiomIterator iterator;
	
	public CbClassAxiomIteratable(CbOntology ontology) {
		iterator = new CbClassAxiomIterator(ontology);
	}

	@Override
	public Iterator<CbClassAxiom> iterator() {
		return iterator;
	}

}