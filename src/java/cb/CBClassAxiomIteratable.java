package org.semanticweb.cb.reasoner;

import java.util.Iterator;

public class CBClassAxiomIteratable implements Iterable<CBClassAxiom> {
	
	CBClassAxiomIterator iterator;
	
	public CBClassAxiomIteratable(CBOntology ont) {
		iterator = new CBClassAxiomIterator(ont);
	}

	@Override
	public Iterator<CBClassAxiom> iterator() {
		return iterator;
	}

}