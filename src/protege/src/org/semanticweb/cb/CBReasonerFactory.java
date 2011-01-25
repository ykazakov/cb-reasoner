package org.semanticweb.cb;

import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.IllegalConfigurationException;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerConfiguration;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;

public class CBReasonerFactory implements OWLReasonerFactory {
	
	@Override
	public String getReasonerName() {
        return getClass().getPackage().getImplementationTitle();
    }
	@Override
	public OWLReasoner createNonBufferingReasoner(OWLOntology ontology) {
		return new CBReasoner(ontology, false, null);
	}

	@Override
	public OWLReasoner createReasoner(OWLOntology ontology) {
		return new CBReasoner(ontology, true, null);
	}

	@Override
	public OWLReasoner createNonBufferingReasoner(OWLOntology ontology, OWLReasonerConfiguration config)
			throws IllegalConfigurationException {
//		if (config.getFreshEntityPolicy()!=FreshEntityPolicy.DISALLOW || config.getIndividualNodeSetPolicy()!=IndividualNodeSetPolicy.BY_SAME_AS) {
//			throw new IllegalConfigurationException("This configuration is not supported. ", config);
//		}
		return new CBReasoner(ontology, false, config.getProgressMonitor());
	}
	@Override
	public OWLReasoner createReasoner(OWLOntology ontology, OWLReasonerConfiguration config)
			throws IllegalConfigurationException {
//		if (config.getFreshEntityPolicy()!=FreshEntityPolicy.DISALLOW || config.getIndividualNodeSetPolicy()!=IndividualNodeSetPolicy.BY_SAME_AS) {
//			throw new IllegalConfigurationException("This configuration is not supported. ", config);
//		}
		return new CBReasoner(ontology, true, config.getProgressMonitor());
	}

}
