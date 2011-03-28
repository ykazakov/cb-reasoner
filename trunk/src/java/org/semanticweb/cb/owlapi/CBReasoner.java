package org.semanticweb.cb.owlapi;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.semanticweb.cb.reasoner.CBClass;
import org.semanticweb.cb.reasoner.CBClassTaxonomyNode;
import org.semanticweb.cb.reasoner.CBOntology;
import org.semanticweb.cb.reasoner.CBProgressMonitorSTDERR;
import org.semanticweb.cb.reasoner.DummyProgressMonitor;
import org.semanticweb.cb.reasoner.ProgressMonitor;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDataPropertyExpression;
import org.semanticweb.owlapi.model.OWLException;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyChange;
import org.semanticweb.owlapi.model.OWLOntologyChangeListener;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.RemoveAxiom;
import org.semanticweb.owlapi.reasoner.AxiomNotInProfileException;
import org.semanticweb.owlapi.reasoner.BufferingMode;
import org.semanticweb.owlapi.reasoner.ClassExpressionNotInProfileException;
import org.semanticweb.owlapi.reasoner.FreshEntitiesException;
import org.semanticweb.owlapi.reasoner.FreshEntityPolicy;
import org.semanticweb.owlapi.reasoner.InconsistentOntologyException;
import org.semanticweb.owlapi.reasoner.IndividualNodeSetPolicy;
import org.semanticweb.owlapi.reasoner.InferenceType;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.ReasonerInterruptedException;
import org.semanticweb.owlapi.reasoner.ReasonerProgressMonitor;
import org.semanticweb.owlapi.reasoner.TimeOutException;
import org.semanticweb.owlapi.reasoner.UnsupportedEntailmentTypeException;
import org.semanticweb.owlapi.reasoner.impl.OWLClassNode;
import org.semanticweb.owlapi.reasoner.impl.OWLObjectPropertyNode;
import org.semanticweb.owlapi.util.Version;

public class CBReasoner implements OWLReasoner {

	protected final OWLOntologyManager manager;
	protected final OWLOntology owlOntology;
	public CBOntology cbOntology;
	protected final OntologyChangeListener ontologyChangeListener;
	protected final boolean buffering;
	protected final OWLDataFactory owlDataFactory;
	protected final ProgressMonitor internalProgressMonitor = new CBProgressMonitorSTDERR();
	protected final ProgressMonitor externalProgressMonitor;
	protected final List<OWLOntologyChange> pendingChanges;
	protected volatile boolean interrupted = false;
	protected boolean ontologySynced = false;
	protected boolean classified = false;

	public CBReasoner(OWLOntology ontology, boolean buffering,
			ReasonerProgressMonitor progressMonitor) {
		this.owlOntology = ontology;
		this.ontologyChangeListener = new OntologyChangeListener();
		manager = ontology.getOWLOntologyManager();
		manager.addOntologyChangeListener(ontologyChangeListener);
		this.buffering = buffering;
		this.owlDataFactory = OWLManager.getOWLDataFactory();
		if (progressMonitor == null)
			this.externalProgressMonitor = new DummyProgressMonitor();
		else
			this.externalProgressMonitor = new CBReasonerProgressMonitor(
					progressMonitor);
		this.pendingChanges = new ArrayList<OWLOntologyChange>();
	}

	protected void addAxiom(OWLAxiom ax) {
		try {
			cbOntology.add(Converter.convert(ax));
		} catch (RuntimeException e) {
			System.out.println("Axiom ignored: " + ax.toString() + ": "
					+ e.getMessage());
		}
	}

	protected void removeAxiom(OWLAxiom ax) {
		try {
			cbOntology.remove(Converter.convert(ax));
		} catch (RuntimeException e) {
			System.out.println("Axiom ignored: " + ax.toString() + ": "
					+ e.getMessage());
		}
	}

	protected void syncOntology() {
		if (!ontologySynced) {
			cbOntology = new CBOntology();
			try {
				Set<OWLOntology> importsClosure = owlOntology
						.getImportsClosure();
				int ontCount = importsClosure.size();
				int currentOntology = 0;
				for (OWLOntology ont : importsClosure) {
					currentOntology++;
					String status;
					if (ontCount == 1)
						status = ReasonerProgressMonitor.LOADING;
					else
						status = ReasonerProgressMonitor.LOADING + " "
								+ currentOntology + " of " + ontCount;
					internalProgressMonitor.start(status);
					externalProgressMonitor.start(status);
					Set<OWLAxiom> axioms = ont.getAxioms();
					int axiomCount = axioms.size();
					int currentAxiom = 0;
					for (OWLAxiom ax : axioms) {
						currentAxiom++;
						if (ax.isLogicalAxiom()
								|| ax.isOfType(AxiomType.DECLARATION))
							addAxiom(ax);
						internalProgressMonitor
								.report(currentAxiom, axiomCount);
						externalProgressMonitor
								.report(currentAxiom, axiomCount);
					}
					internalProgressMonitor.finish();
					externalProgressMonitor.finish();
				}

			} catch (ReasonerInterruptedException e) {
				System.out.println("interrupted.");
			}
			ontologySynced = true;
			pendingChanges.clear();
		}
	}

	protected void reloadChanges() {
		if (!pendingChanges.isEmpty()) {
			String status = ReasonerProgressMonitor.LOADING;
			internalProgressMonitor.start(status);
			externalProgressMonitor.start(status);
			int axiomCount = pendingChanges.size();
			int currentAxiom = 0;
			for (OWLOntologyChange change : pendingChanges) {
				if (change instanceof AddAxiom)
					addAxiom(change.getAxiom());
				if (change instanceof RemoveAxiom) {
					removeAxiom(change.getAxiom());
				}
				currentAxiom++;
				internalProgressMonitor.report(currentAxiom, axiomCount);
				externalProgressMonitor.report(currentAxiom, axiomCount);
			}
			internalProgressMonitor.finish();
			externalProgressMonitor.finish();
			pendingChanges.clear();
		}
	}

	protected void classifyOntology() {
		try {
			cbOntology.classify(this.externalProgressMonitor);
			classified = true;
		} catch (ReasonerInterruptedException e) {
			System.out.println("interrupted.");
		}
	}

	public String getReasonerName() {
		return getClass().getPackage().getImplementationTitle();
	}

	public Version getReasonerVersion() {
		String versionString = CBReasoner.class.getPackage()
				.getImplementationVersion();
		String[] splitted;
		int filled = 0;
		int version[] = new int[4];
		if (versionString != null) {
			splitted = versionString.split("\\.");
			while (filled < splitted.length) {
				version[filled] = Integer.parseInt(splitted[filled]);
				filled++;
			}
		}
		while (filled < version.length) {
			version[filled] = 0;
			filled++;
		}
		return new Version(version[0], version[1], version[2], version[3]);
	}

	@Override
	public BufferingMode getBufferingMode() {
		return buffering ? BufferingMode.BUFFERING
				: BufferingMode.NON_BUFFERING;
	}

	public void dispose() {
		owlOntology.getOWLOntologyManager().removeOntologyChangeListener(
				ontologyChangeListener);
		pendingChanges.clear();
	}

	protected class OntologyChangeListener implements OWLOntologyChangeListener {
		public void ontologiesChanged(List<? extends OWLOntologyChange> changes)
				throws OWLException {
			for (OWLOntologyChange change : changes) {
				if (change.isAxiomChange()) {
					OWLAxiom axiom = change.getAxiom();
					if (axiom.isLogicalAxiom()
							|| axiom.isOfType(AxiomType.DECLARATION))
						pendingChanges.add(change);
				} else if (change.isImportChange())
					ontologySynced = false;
			}
		}
	}

	public Set<OWLAxiom> getPendingAxiomAdditions() {
		Set<OWLAxiom> added = new HashSet<OWLAxiom>();
		for (OWLOntologyChange change : pendingChanges)
			if (change instanceof AddAxiom)
				added.add(change.getAxiom());
		return added;
	}

	public Set<OWLAxiom> getPendingAxiomRemovals() {
		Set<OWLAxiom> removed = new HashSet<OWLAxiom>();
		for (OWLOntologyChange change : pendingChanges)
			if (change instanceof RemoveAxiom)
				removed.add(change.getAxiom());
		return removed;
	}

	public List<OWLOntologyChange> getPendingChanges() {
		return pendingChanges;
	}

	public void flush() {
		syncOntology();
		reloadChanges();
	}

	public OWLOntology getRootOntology() {
		return owlOntology;
	}

	@Override
	public void interrupt() {
		System.out.println("interrupted");
		interrupted = true;
	}

	@Override
	public void precomputeInferences(InferenceType... inferenceTypes)
			throws ReasonerInterruptedException, TimeOutException,
			InconsistentOntologyException {
		for (InferenceType inferenceType : inferenceTypes) {
			if (inferenceType.equals(InferenceType.CLASS_HIERARCHY)) {
				syncOntology();
				reloadChanges();
				cbOntology.printInfo();
				classifyOntology();
			}
		}
	}

	@Override
	public boolean isPrecomputed(InferenceType inferenceType) {
		// TODO Auto-generated method stub
		if (inferenceType.equals(InferenceType.CLASS_HIERARCHY))
			return classified;
		else
			return false;
	}

	@Override
	public Set<InferenceType> getPrecomputableInferenceTypes() {
		return Collections.singleton(InferenceType.CLASS_HIERARCHY);
	}

	@Override
	public boolean isConsistent() throws ReasonerInterruptedException,
			TimeOutException {
		return true;
	}

	@Override
	public boolean isSatisfiable(OWLClassExpression classExpression)
			throws ReasonerInterruptedException, TimeOutException,
			ClassExpressionNotInProfileException, FreshEntitiesException,
			InconsistentOntologyException {
		if (classExpression.isAnonymous())
			return true;
		else {
			OWLClassNode botNode = Converter.convert(cbOntology.getBotNode());
			return (!botNode.contains(classExpression.asOWLClass()));
		}
	}

	@Override
	public Node<OWLClass> getUnsatisfiableClasses()
			throws ReasonerInterruptedException, TimeOutException,
			InconsistentOntologyException {
		return Converter.convert(cbOntology.getBotNode());
	}

	@Override
	public boolean isEntailed(OWLAxiom axiom)
			throws ReasonerInterruptedException,
			UnsupportedEntailmentTypeException, TimeOutException,
			AxiomNotInProfileException, FreshEntitiesException,
			InconsistentOntologyException {
		throw new UnsupportedEntailmentTypeException(axiom);
	}

	@Override
	public boolean isEntailed(Set<? extends OWLAxiom> axioms)
			throws ReasonerInterruptedException,
			UnsupportedEntailmentTypeException, TimeOutException,
			AxiomNotInProfileException, FreshEntitiesException,
			InconsistentOntologyException {
		throw new UnsupportedEntailmentTypeException(axioms.iterator().next());
	}

	@Override
	public boolean isEntailmentCheckingSupported(AxiomType<?> axiomType) {
		return false;
	}

	@Override
	public Node<OWLClass> getTopClassNode() {
		return Converter.convert(cbOntology.getTopNode());
	}

	@Override
	public Node<OWLClass> getBottomClassNode() {
		return Converter.convert(cbOntology.getBotNode());
	}

	@Override
	public NodeSet<OWLClass> getSubClasses(OWLClassExpression ce, boolean direct)
			throws ReasonerInterruptedException, TimeOutException,
			FreshEntitiesException, InconsistentOntologyException,
			ClassExpressionNotInProfileException {
		if (ce.isAnonymous())
			return null;
		CBClassTaxonomyNode node = cbOntology.getClassTaxonomyNode(Converter
				.convert(ce.asOWLClass()));
		return Converter.convert(node.getChildNodes());
	}

	@Override
	public NodeSet<OWLClass> getSuperClasses(OWLClassExpression ce,
			boolean direct) throws InconsistentOntologyException,
			ClassExpressionNotInProfileException, FreshEntitiesException,
			ReasonerInterruptedException, TimeOutException {
		if (ce.isAnonymous())
			return null;
		CBClassTaxonomyNode node = cbOntology.getClassTaxonomyNode(Converter
				.convert(ce.asOWLClass()));
		return Converter.convert(node.getParentNodes());
	}

	@Override
	public Node<OWLClass> getEquivalentClasses(OWLClassExpression ce)
			throws InconsistentOntologyException,
			ClassExpressionNotInProfileException, FreshEntitiesException,
			ReasonerInterruptedException, TimeOutException {
		if (ce.isAnonymous())
			return null;
		CBClass cls = Converter.convert(ce.asOWLClass());
		return Converter.convert(cbOntology.getClassTaxonomyNode(cls));
	}

	@Override
	public NodeSet<OWLClass> getDisjointClasses(OWLClassExpression ce)
			throws ReasonerInterruptedException, TimeOutException,
			FreshEntitiesException, InconsistentOntologyException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Node<OWLObjectPropertyExpression> getTopObjectPropertyNode() {
		// TODO: Add support
		return new OWLObjectPropertyNode(
				owlDataFactory.getOWLTopObjectProperty());
	}

	@Override
	public Node<OWLObjectPropertyExpression> getBottomObjectPropertyNode() {
		// TODO: Add support
		return new OWLObjectPropertyNode(
				owlDataFactory.getOWLBottomObjectProperty());
	}

	@Override
	public NodeSet<OWLObjectPropertyExpression> getSubObjectProperties(
			OWLObjectPropertyExpression pe, boolean direct)
			throws InconsistentOntologyException, FreshEntitiesException,
			ReasonerInterruptedException, TimeOutException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public NodeSet<OWLObjectPropertyExpression> getSuperObjectProperties(
			OWLObjectPropertyExpression pe, boolean direct)
			throws InconsistentOntologyException, FreshEntitiesException,
			ReasonerInterruptedException, TimeOutException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Node<OWLObjectPropertyExpression> getEquivalentObjectProperties(
			OWLObjectPropertyExpression pe)
			throws InconsistentOntologyException, FreshEntitiesException,
			ReasonerInterruptedException, TimeOutException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public NodeSet<OWLObjectPropertyExpression> getDisjointObjectProperties(
			OWLObjectPropertyExpression pe)
			throws InconsistentOntologyException, FreshEntitiesException,
			ReasonerInterruptedException, TimeOutException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Node<OWLObjectPropertyExpression> getInverseObjectProperties(
			OWLObjectPropertyExpression pe)
			throws InconsistentOntologyException, FreshEntitiesException,
			ReasonerInterruptedException, TimeOutException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public NodeSet<OWLClass> getObjectPropertyDomains(
			OWLObjectPropertyExpression pe, boolean direct)
			throws InconsistentOntologyException, FreshEntitiesException,
			ReasonerInterruptedException, TimeOutException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public NodeSet<OWLClass> getObjectPropertyRanges(
			OWLObjectPropertyExpression pe, boolean direct)
			throws InconsistentOntologyException, FreshEntitiesException,
			ReasonerInterruptedException, TimeOutException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Node<OWLDataProperty> getTopDataPropertyNode() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Node<OWLDataProperty> getBottomDataPropertyNode() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public NodeSet<OWLDataProperty> getSubDataProperties(OWLDataProperty pe,
			boolean direct) throws InconsistentOntologyException,
			FreshEntitiesException, ReasonerInterruptedException,
			TimeOutException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public NodeSet<OWLDataProperty> getSuperDataProperties(OWLDataProperty pe,
			boolean direct) throws InconsistentOntologyException,
			FreshEntitiesException, ReasonerInterruptedException,
			TimeOutException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Node<OWLDataProperty> getEquivalentDataProperties(OWLDataProperty pe)
			throws InconsistentOntologyException, FreshEntitiesException,
			ReasonerInterruptedException, TimeOutException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public NodeSet<OWLDataProperty> getDisjointDataProperties(
			OWLDataPropertyExpression pe) throws InconsistentOntologyException,
			FreshEntitiesException, ReasonerInterruptedException,
			TimeOutException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public NodeSet<OWLClass> getDataPropertyDomains(OWLDataProperty pe,
			boolean direct) throws InconsistentOntologyException,
			FreshEntitiesException, ReasonerInterruptedException,
			TimeOutException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public NodeSet<OWLClass> getTypes(OWLNamedIndividual ind, boolean direct)
			throws InconsistentOntologyException, FreshEntitiesException,
			ReasonerInterruptedException, TimeOutException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public NodeSet<OWLNamedIndividual> getInstances(OWLClassExpression ce,
			boolean direct) throws InconsistentOntologyException,
			ClassExpressionNotInProfileException, FreshEntitiesException,
			ReasonerInterruptedException, TimeOutException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public NodeSet<OWLNamedIndividual> getObjectPropertyValues(
			OWLNamedIndividual ind, OWLObjectPropertyExpression pe)
			throws InconsistentOntologyException, FreshEntitiesException,
			ReasonerInterruptedException, TimeOutException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Set<OWLLiteral> getDataPropertyValues(OWLNamedIndividual ind,
			OWLDataProperty pe) throws InconsistentOntologyException,
			FreshEntitiesException, ReasonerInterruptedException,
			TimeOutException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Node<OWLNamedIndividual> getSameIndividuals(OWLNamedIndividual ind)
			throws InconsistentOntologyException, FreshEntitiesException,
			ReasonerInterruptedException, TimeOutException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public NodeSet<OWLNamedIndividual> getDifferentIndividuals(
			OWLNamedIndividual ind) throws InconsistentOntologyException,
			FreshEntitiesException, ReasonerInterruptedException,
			TimeOutException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public long getTimeOut() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public FreshEntityPolicy getFreshEntityPolicy() {
		return FreshEntityPolicy.ALLOW;
	}

	@Override
	public IndividualNodeSetPolicy getIndividualNodeSetPolicy() {
		return IndividualNodeSetPolicy.BY_NAME;
	}

}
