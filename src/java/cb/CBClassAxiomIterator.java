package org.semanticweb.cb.reasoner;

import java.util.Iterator;

public class CBClassAxiomIterator extends CObject implements Iterator<CBClassAxiom> {

	public CBClassAxiomIterator(CBOntology ont) {
		create(ont);		
	}
	
	private native void create(CBOntology ont);
		
	protected native synchronized void destruct();
	
	@Override
	public native boolean hasNext();

	@Override
	public native CBClassAxiom next();

	@Override
	public void remove() {
		throw new UnsupportedOperationException();
	}		
}