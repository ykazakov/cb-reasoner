package org.semanticweb.cb;

import java.util.List;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLAnnotationPropertyDomainAxiom;
import org.semanticweb.owlapi.model.OWLAnnotationPropertyRangeAxiom;
import org.semanticweb.owlapi.model.OWLAnonymousIndividual;
import org.semanticweb.owlapi.model.OWLAsymmetricObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataAllValuesFrom;
import org.semanticweb.owlapi.model.OWLDataComplementOf;
import org.semanticweb.owlapi.model.OWLDataExactCardinality;
import org.semanticweb.owlapi.model.OWLDataHasValue;
import org.semanticweb.owlapi.model.OWLDataIntersectionOf;
import org.semanticweb.owlapi.model.OWLDataMaxCardinality;
import org.semanticweb.owlapi.model.OWLDataMinCardinality;
import org.semanticweb.owlapi.model.OWLDataOneOf;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLDataPropertyDomainAxiom;
import org.semanticweb.owlapi.model.OWLDataPropertyRangeAxiom;
import org.semanticweb.owlapi.model.OWLDataSomeValuesFrom;
import org.semanticweb.owlapi.model.OWLDataUnionOf;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLDatatypeDefinitionAxiom;
import org.semanticweb.owlapi.model.OWLDatatypeRestriction;
import org.semanticweb.owlapi.model.OWLDeclarationAxiom;
import org.semanticweb.owlapi.model.OWLDifferentIndividualsAxiom;
import org.semanticweb.owlapi.model.OWLDisjointClassesAxiom;
import org.semanticweb.owlapi.model.OWLDisjointDataPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLDisjointObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLDisjointUnionAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentDataPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLFacetRestriction;
import org.semanticweb.owlapi.model.OWLFunctionalDataPropertyAxiom;
import org.semanticweb.owlapi.model.OWLFunctionalObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLHasKeyAxiom;
import org.semanticweb.owlapi.model.OWLInverseFunctionalObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLInverseObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLIrreflexiveObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLNegativeDataPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLNegativeObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLObjectAllValuesFrom;
import org.semanticweb.owlapi.model.OWLObjectComplementOf;
import org.semanticweb.owlapi.model.OWLObjectExactCardinality;
import org.semanticweb.owlapi.model.OWLObjectHasSelf;
import org.semanticweb.owlapi.model.OWLObjectHasValue;
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf;
import org.semanticweb.owlapi.model.OWLObjectInverseOf;
import org.semanticweb.owlapi.model.OWLObjectMaxCardinality;
import org.semanticweb.owlapi.model.OWLObjectMinCardinality;
import org.semanticweb.owlapi.model.OWLObjectOneOf;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLObjectPropertyDomainAxiom;
import org.semanticweb.owlapi.model.OWLObjectPropertyRangeAxiom;
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom;
import org.semanticweb.owlapi.model.OWLObjectUnionOf;
import org.semanticweb.owlapi.model.OWLObjectVisitorEx;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLReflexiveObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLSameIndividualAxiom;
import org.semanticweb.owlapi.model.OWLSubAnnotationPropertyOfAxiom;
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom;
import org.semanticweb.owlapi.model.OWLSubDataPropertyOfAxiom;
import org.semanticweb.owlapi.model.OWLSubObjectPropertyOfAxiom;
import org.semanticweb.owlapi.model.OWLSubPropertyChainOfAxiom;
import org.semanticweb.owlapi.model.OWLSymmetricObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLTransitiveObjectPropertyAxiom;
import org.semanticweb.owlapi.model.SWRLBuiltInAtom;
import org.semanticweb.owlapi.model.SWRLClassAtom;
import org.semanticweb.owlapi.model.SWRLDataPropertyAtom;
import org.semanticweb.owlapi.model.SWRLDataRangeAtom;
import org.semanticweb.owlapi.model.SWRLDifferentIndividualsAtom;
import org.semanticweb.owlapi.model.SWRLIndividualArgument;
import org.semanticweb.owlapi.model.SWRLLiteralArgument;
import org.semanticweb.owlapi.model.SWRLObjectPropertyAtom;
import org.semanticweb.owlapi.model.SWRLRule;
import org.semanticweb.owlapi.model.SWRLSameIndividualAtom;
import org.semanticweb.owlapi.model.SWRLVariable;

import cb.Axiom;
import cb.ClassExpression;
import cb.EquivalentClassesAxiom;
import cb.FunctionalObjectPropertyAxiom;
import cb.InverseFunctionalObjectPropertyAxiom;
import cb.InverseObjectPropertiesAxiom;
import cb.ObjectIntersectionOf;
import cb.ObjectInverseOf;
import cb.ObjectProperty;
import cb.ObjectPropertyExpression;
import cb.ObjectSomeValuesFrom;
import cb.Ontology;
import cb.SubClassOfAxiom;
import cb.SubObjectPropertyOfAxiom;
import cb.TransitiveObjectPropertyAxiom;

public class LoaderVisitor implements OWLObjectVisitorEx<Object> {	
	
	protected final CBReasoner reasoner;
	
	public LoaderVisitor(CBReasoner reasoner) {
		this.reasoner=reasoner;
	} 
			
	@Override
	public Object visit(OWLSubClassOfAxiom axiom) {
		ClassExpression cbSubClassPointer=(ClassExpression)axiom.getSubClass().accept(this);
		ClassExpression cbSuperClassPointer=(ClassExpression)axiom.getSuperClass().accept(this);
		return new SubClassOfAxiom(cbSubClassPointer, cbSuperClassPointer);
	}
	
	@Override
	public Object visit(OWLNegativeObjectPropertyAssertionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLAsymmetricObjectPropertyAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLReflexiveObjectPropertyAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLDisjointClassesAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLDataPropertyDomainAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLObjectPropertyDomainAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLEquivalentObjectPropertiesAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLNegativeDataPropertyAssertionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLDifferentIndividualsAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLDisjointDataPropertiesAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLDisjointObjectPropertiesAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLObjectPropertyRangeAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLObjectPropertyAssertionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLFunctionalObjectPropertyAxiom axiom) {
		ObjectPropertyExpression ope=(ObjectPropertyExpression)axiom.getProperty().accept(this);
		return new FunctionalObjectPropertyAxiom(ope);
	}

	@Override
	public Object visit(OWLSubObjectPropertyOfAxiom axiom) {
		ObjectPropertyExpression sub=(ObjectPropertyExpression)axiom.getSubProperty().accept(this);
		ObjectPropertyExpression sup=(ObjectPropertyExpression)axiom.getSuperProperty().accept(this);
		return new SubObjectPropertyOfAxiom(sub, sup);
	}

	@Override
	public Object visit(OWLDisjointUnionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLDeclarationAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLAnnotationAssertionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLSymmetricObjectPropertyAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLDataPropertyRangeAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLFunctionalDataPropertyAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLEquivalentDataPropertiesAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLClassAssertionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLEquivalentClassesAxiom axiom) {
		List<OWLClassExpression> operands=axiom.getClassExpressionsAsList();
		int length=operands.size();
		ClassExpression[] cbObjects=new ClassExpression[length];
		for (int i=0; i<length; i++) {
			cbObjects[i]=(ClassExpression)operands.get(i).accept(this);
		}
		return new EquivalentClassesAxiom(cbObjects);
	}

	@Override
	public Object visit(OWLDataPropertyAssertionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLTransitiveObjectPropertyAxiom axiom) {
		ObjectPropertyExpression ope=(ObjectPropertyExpression)axiom.getProperty().accept(this);
		return new TransitiveObjectPropertyAxiom(ope);
	}

	@Override
	public Object visit(OWLIrreflexiveObjectPropertyAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLSubDataPropertyOfAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLInverseFunctionalObjectPropertyAxiom axiom) {
		ObjectPropertyExpression ope=(ObjectPropertyExpression)axiom.getProperty().accept(this);
		return new InverseFunctionalObjectPropertyAxiom(ope);
	}

	@Override
	public Object visit(OWLSameIndividualAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLSubPropertyChainOfAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLInverseObjectPropertiesAxiom axiom) {
		ObjectPropertyExpression opea = (ObjectPropertyExpression)axiom.getFirstProperty().accept(this);
		ObjectPropertyExpression opeb = (ObjectPropertyExpression)axiom.getSecondProperty().accept(this);
		return new InverseObjectPropertiesAxiom(opea,opeb);
	}

	@Override
	public Object visit(OWLHasKeyAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLDatatypeDefinitionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(SWRLRule rule) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLSubAnnotationPropertyOfAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLAnnotationPropertyDomainAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLAnnotationPropertyRangeAxiom axiom) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLClass ce) {
		String iri=ce.getIRI().toString();
		return new cb.Class(iri);
	}

	@Override
	public Object visit(OWLObjectIntersectionOf ce) {
		List<OWLClassExpression> operands=ce.getOperandsAsList();
		int length=operands.size();
		ClassExpression[] cbObjects=new ClassExpression[length];
		for (int i=0; i<length; i++) {
			cbObjects[i]=(ClassExpression)operands.get(i).accept(this);
		}
		return new ObjectIntersectionOf(cbObjects);
	}

	@Override
	public Object visit(OWLObjectUnionOf ce) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLObjectComplementOf ce) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLObjectSomeValuesFrom ce) {
		ObjectPropertyExpression ope = (ObjectPropertyExpression)ce.getProperty().accept(this);
		ClassExpression cce = (ClassExpression)ce.getFiller().accept(this);
		return new ObjectSomeValuesFrom(ope, cce);
	}

	@Override
	public Object visit(OWLObjectAllValuesFrom ce) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLObjectHasValue ce) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLObjectMinCardinality ce) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLObjectExactCardinality ce) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLObjectMaxCardinality ce) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLObjectHasSelf ce) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLObjectOneOf ce) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLDataSomeValuesFrom ce) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLDataAllValuesFrom ce) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLDataHasValue ce) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLDataMinCardinality ce) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLDataExactCardinality ce) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLDataMaxCardinality ce) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLDatatype node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLDataComplementOf node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLDataOneOf node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLDataIntersectionOf node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLDataUnionOf node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLDatatypeRestriction node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLLiteral node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLFacetRestriction node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLObjectProperty op) {
		String iri=op.getIRI().toString();
		return new ObjectProperty(iri);
	}

	@Override
	public Object visit(OWLObjectInverseOf op) {
		String iri=op.getNamedProperty().getIRI().toString();
		return new ObjectInverseOf(iri);
	}

	@Override
	public Object visit(OWLDataProperty property) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLNamedIndividual individual) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLAnnotationProperty property) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLAnnotation node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(IRI iri) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLAnonymousIndividual individual) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(SWRLClassAtom node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(SWRLDataRangeAtom node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(SWRLObjectPropertyAtom node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(SWRLDataPropertyAtom node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(SWRLBuiltInAtom node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(SWRLVariable node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(SWRLIndividualArgument node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(SWRLLiteralArgument node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(SWRLSameIndividualAtom node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(SWRLDifferentIndividualsAtom node) {
		// TODO Auto-generated method stub
		throw new RuntimeException("Not yet implemented.");
	}

	@Override
	public Object visit(OWLOntology ontology) {
		Ontology cbOntology=new Ontology();
		int numberOfAxioms=ontology.getLogicalAxioms().size();
		int currentAxiom=0;
		for (OWLAxiom axiom : ontology.getLogicalAxioms()) {
			currentAxiom++;
			cbOntology.add((Axiom)axiom.accept(this));			
			if (reasoner.interrupted) break;
			reasoner.internalProgressMonitor.report(currentAxiom, numberOfAxioms);
			reasoner.externalProgressMonitor.report(currentAxiom, numberOfAxioms);
		}
		return cbOntology;
	}
}
