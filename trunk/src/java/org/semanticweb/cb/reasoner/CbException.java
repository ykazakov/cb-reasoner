package org.semanticweb.cb.reasoner;

public class CbException extends Exception {

	private static final long serialVersionUID = 2217345833594331341L;
	
	protected CbException() {
    }

    public CbException(String message) {
        super(message);
    }

    public CbException(String message, Throwable cause) {
        super(message, cause);
    }

    public CbException(Throwable cause) {
        super(cause);
    }    
    
}
