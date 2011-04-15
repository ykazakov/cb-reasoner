package org.semanticweb.cb.reasoner;

import java.util.Iterator;

public class CbClassAxiomIterator extends CObject implements Iterator<CbClassAxiom> {	
	
	public CbClassAxiomIterator(CbOntology ontology) {
		super(getPtr(ontology));		
	}
	
	private static native long getPtr(CbOntology ontology);
		
	protected native synchronized void destruct();
	
	@Override
	public native boolean hasNext();

	@Override
	public native CbClassAxiom next();

	@Override
	public void remove() {
		throw new UnsupportedOperationException();
	}		
}