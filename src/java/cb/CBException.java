package cb;

public class CBException extends Exception {

	private static final long serialVersionUID = 2217345833594331341L;

	protected CBException() {
    }

    public CBException(String message) {
        super(message);
    }

    public CBException(String message, Throwable cause) {
        super(message, cause);
    }

    public CBException(Throwable cause) {
        super(cause);
    }    
    
}
