package org.semanticweb.cb;
import org.protege.editor.owl.model.inference.AbstractProtegeOWLReasonerInfo;
import org.semanticweb.owlapi.reasoner.BufferingMode;
import org.semanticweb.owlapi.reasoner.FreshEntityPolicy;
import org.semanticweb.owlapi.reasoner.IndividualNodeSetPolicy;
import org.semanticweb.owlapi.reasoner.OWLReasonerConfiguration;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;
import org.semanticweb.owlapi.reasoner.ReasonerProgressMonitor;
import org.semanticweb.owlapi.reasoner.SimpleConfiguration;


public class ProtegeReasonerFactory extends AbstractProtegeOWLReasonerInfo {
		
    protected final OWLReasonerFactory factory=new CBReasonerFactory(); 
    
    public BufferingMode getRecommendedBuffering() {
        return BufferingMode.BUFFERING;
    }
    
    public OWLReasonerFactory getReasonerFactory() {
        return factory;
    }
        
    public OWLReasonerConfiguration getConfiguration(ReasonerProgressMonitor monitor) {
    	return new SimpleConfiguration(monitor, FreshEntityPolicy.ALLOW, 0, IndividualNodeSetPolicy.BY_NAME);
    }
    
    public void initialise() throws Exception {
    }
    
    public void dispose() throws Exception {
    }
}