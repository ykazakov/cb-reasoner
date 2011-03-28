package org.semanticweb.cb.reasoner;

public abstract class CBObject extends CObject {

	public abstract <O> O accept(CBObjectVisitorEx<O> visitor);

}
