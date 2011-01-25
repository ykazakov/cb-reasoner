package cb;

class Loader {
  private static boolean loaded = false;
   
  static void load(){
	if(loaded) return;	
	try {		
        System.loadLibrary("jcb");
        loaded = true;
    } catch (UnsatisfiedLinkError e) {
      System.err.println("CB native code library failed to load.\n" + e);
    }
  }
}
