package org.semanticweb.cb.owlapi;

public class ConverterException extends RuntimeException {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = -8503046439108873431L;

	protected ConverterException() {
    }

    public ConverterException(String message) {
        super(message);
    }

    public ConverterException(String message, Throwable cause) {
        super(message, cause);
    }

    public ConverterException(Throwable cause) {
        super(cause);
    }    
    
}
