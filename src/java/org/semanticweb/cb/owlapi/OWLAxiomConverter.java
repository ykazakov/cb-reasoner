package org.semanticweb.cb.owlapi;

import java.util.List;

import org.semanticweb.cb.reasoner.CBAxiom;
import org.semanticweb.cb.reasoner.CBClassDeclarationAxiom;
import org.semanticweb.cb.reasoner.CBClassExpression;
import org.semanticweb.cb.reasoner.CBEquivalentClassesAxiom;
import org.semanticweb.cb.reasoner.CBFunctionalObjectPropertyAxiom;
import org.semanticweb.cb.reasoner.CBInverseFunctionalObjectPropertyAxiom;
import org.semanticweb.cb.reasoner.CBInverseObjectPropertiesAxiom;
import org.semanticweb.cb.reasoner.CBObjectPropertyDeclarationAxiom;
import org.semanticweb.cb.reasoner.CBSubClassOfAxiom;
import org.semanticweb.cb.reasoner.CBSubObjectPropertyOfAxiom;
import org.semanticweb.cb.reasoner.CBTransitiveObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom;
import org.semanticweb.owlapi.model.OWLAnnotationPropertyDomainAxiom;
import org.semanticweb.owlapi.model.OWLAnnotationPropertyRangeAxiom;
import org.semanticweb.owlapi.model.OWLAsymmetricObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLAxiomVisitorEx;
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLDataPropertyDomainAxiom;
import org.semanticweb.owlapi.model.OWLDataPropertyRangeAxiom;
import org.semanticweb.owlapi.model.OWLDatatypeDefinitionAxiom;
import org.semanticweb.owlapi.model.OWLDeclarationAxiom;
import org.semanticweb.owlapi.model.OWLDifferentIndividualsAxiom;
import org.semanticweb.owlapi.model.OWLDisjointClassesAxiom;
import org.semanticweb.owlapi.model.OWLDisjointDataPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLDisjointObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLDisjointUnionAxiom;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentDataPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLEquivalentObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLFunctionalDataPropertyAxiom;
import org.semanticweb.owlapi.model.OWLFunctionalObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLHasKeyAxiom;
import org.semanticweb.owlapi.model.OWLInverseFunctionalObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLInverseObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLIrreflexiveObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLNegativeDataPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLNegativeObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLObjectPropertyDomainAxiom;
import org.semanticweb.owlapi.model.OWLObjectPropertyRangeAxiom;
import org.semanticweb.owlapi.model.OWLReflexiveObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLSameIndividualAxiom;
import org.semanticweb.owlapi.model.OWLSubAnnotationPropertyOfAxiom;
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom;
import org.semanticweb.owlapi.model.OWLSubDataPropertyOfAxiom;
import org.semanticweb.owlapi.model.OWLSubObjectPropertyOfAxiom;
import org.semanticweb.owlapi.model.OWLSubPropertyChainOfAxiom;
import org.semanticweb.owlapi.model.OWLSymmetricObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLTransitiveObjectPropertyAxiom;
import org.semanticweb.owlapi.model.SWRLRule;

public final class OWLAxiomConverter implements OWLAxiomVisitorEx<CBAxiom> {
	
	private static final OWLAxiomConverter converter = new OWLAxiomConverter();
	private OWLAxiomConverter() {		
	}
	
	static OWLAxiomConverter getInstance() {
		return converter;
	}

	@Override
	public CBAxiom visit(OWLSubAnnotationPropertyOfAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported"); 		
	}

	@Override
	public CBAxiom visit(OWLAnnotationPropertyDomainAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(OWLAnnotationPropertyRangeAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBSubClassOfAxiom visit(OWLSubClassOfAxiom axiom) {
		OWLClassExpressionConverter ceConverter = OWLClassExpressionConverter.getInstance(); 
		return new CBSubClassOfAxiom(axiom.getSubClass().accept(ceConverter), 
				axiom.getSuperClass().accept(ceConverter));		
	}

	@Override
	public CBAxiom visit(OWLNegativeObjectPropertyAssertionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(OWLAsymmetricObjectPropertyAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(OWLReflexiveObjectPropertyAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(OWLDisjointClassesAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(OWLDataPropertyDomainAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(OWLObjectPropertyDomainAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(OWLEquivalentObjectPropertiesAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(OWLNegativeDataPropertyAssertionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(OWLDifferentIndividualsAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(OWLDisjointDataPropertiesAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(OWLDisjointObjectPropertiesAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(OWLObjectPropertyRangeAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(OWLObjectPropertyAssertionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBFunctionalObjectPropertyAxiom visit(OWLFunctionalObjectPropertyAxiom axiom) {
		OWLPropertyExpressionConverter peConverter = 
			OWLPropertyExpressionConverter.getInstance(); 		
		return new CBFunctionalObjectPropertyAxiom(axiom.getProperty().accept(peConverter));
	}

	@Override
	public CBSubObjectPropertyOfAxiom visit(OWLSubObjectPropertyOfAxiom axiom) {		
		OWLPropertyExpressionConverter peConverter = 
			OWLPropertyExpressionConverter.getInstance();
		return new CBSubObjectPropertyOfAxiom(axiom.getSubProperty().accept(peConverter), 
				axiom.getSuperProperty().accept(peConverter));
	}

	@Override
	public CBAxiom visit(OWLDisjointUnionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(OWLDeclarationAxiom axiom) {
		OWLEntity entity = axiom.getEntity();
		if (entity.isOWLClass())
			return new CBClassDeclarationAxiom(entity.getIRI().toString());
		else if (entity.isOWLObjectProperty())
			return new CBObjectPropertyDeclarationAxiom(entity.getIRI().toString());
		else throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(OWLAnnotationAssertionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(OWLSymmetricObjectPropertyAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(OWLDataPropertyRangeAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(OWLFunctionalDataPropertyAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(OWLEquivalentDataPropertiesAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(OWLClassAssertionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBEquivalentClassesAxiom visit(OWLEquivalentClassesAxiom axiom) {
		OWLClassExpressionConverter ceConverter = 
			OWLClassExpressionConverter.getInstance();
		List<OWLClassExpression> operands=axiom.getClassExpressionsAsList();
		int length=operands.size();
		CBClassExpression[] cbObjects=new CBClassExpression[length];
		int i = 0;
		for (OWLClassExpression ce : operands) {
			cbObjects[i] = ce.accept(ceConverter);
			i++;
		}
		return new CBEquivalentClassesAxiom(cbObjects);
	}

	@Override
	public CBAxiom visit(OWLDataPropertyAssertionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBTransitiveObjectPropertyAxiom visit(OWLTransitiveObjectPropertyAxiom axiom) {
		OWLPropertyExpressionConverter peConverter = 
			OWLPropertyExpressionConverter.getInstance(); 		
		return new CBTransitiveObjectPropertyAxiom(axiom.getProperty().accept(peConverter));		
	}

	@Override
	public CBAxiom visit(OWLIrreflexiveObjectPropertyAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(OWLSubDataPropertyOfAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBInverseFunctionalObjectPropertyAxiom visit(OWLInverseFunctionalObjectPropertyAxiom axiom) {
		OWLPropertyExpressionConverter peConverter = 
			OWLPropertyExpressionConverter.getInstance(); 		
		return new CBInverseFunctionalObjectPropertyAxiom(axiom.getProperty().accept(peConverter));		
	}

	@Override
	public CBAxiom visit(OWLSameIndividualAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(OWLSubPropertyChainOfAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBInverseObjectPropertiesAxiom visit(OWLInverseObjectPropertiesAxiom axiom) {
		OWLPropertyExpressionConverter peConverter = 
			OWLPropertyExpressionConverter.getInstance();				
		return new CBInverseObjectPropertiesAxiom(axiom.getFirstProperty().accept(peConverter),
				axiom.getSecondProperty().accept(peConverter));		
	}

	@Override
	public CBAxiom visit(OWLHasKeyAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(OWLDatatypeDefinitionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CBAxiom visit(SWRLRule rule) {
		// TODO Auto-generated method stub
		throw new ConverterException(rule.getAxiomType().getName() + " not supported");
	}
	

}
