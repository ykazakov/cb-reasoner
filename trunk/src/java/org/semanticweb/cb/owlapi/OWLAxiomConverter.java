package org.semanticweb.cb.owlapi;

import java.util.List;

import org.semanticweb.cb.reasoner.CbAxiom;
import org.semanticweb.cb.reasoner.CbClassDeclarationAxiom;
import org.semanticweb.cb.reasoner.CbClassExpression;
import org.semanticweb.cb.reasoner.CbEquivalentClassesAxiom;
import org.semanticweb.cb.reasoner.CbFunctionalObjectPropertyAxiom;
import org.semanticweb.cb.reasoner.CbInverseFunctionalObjectPropertyAxiom;
import org.semanticweb.cb.reasoner.CbInverseObjectPropertiesAxiom;
import org.semanticweb.cb.reasoner.CbObjectPropertyDeclarationAxiom;
import org.semanticweb.cb.reasoner.CbSubClassOfAxiom;
import org.semanticweb.cb.reasoner.CbSubObjectPropertyOfAxiom;
import org.semanticweb.cb.reasoner.CbTransitiveObjectPropertyAxiom;
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

public final class OWLAxiomConverter implements OWLAxiomVisitorEx<CbAxiom> {
	
	private static final OWLAxiomConverter converter = new OWLAxiomConverter();
	private OWLAxiomConverter() {		
	}
	
	static OWLAxiomConverter getInstance() {
		return converter;
	}

	@Override
	public CbAxiom visit(OWLSubAnnotationPropertyOfAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported"); 		
	}

	@Override
	public CbAxiom visit(OWLAnnotationPropertyDomainAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(OWLAnnotationPropertyRangeAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbSubClassOfAxiom visit(OWLSubClassOfAxiom axiom) {
		OWLClassExpressionConverter ceConverter = OWLClassExpressionConverter.getInstance(); 
		return new CbSubClassOfAxiom(axiom.getSubClass().accept(ceConverter), 
				axiom.getSuperClass().accept(ceConverter));		
	}

	@Override
	public CbAxiom visit(OWLNegativeObjectPropertyAssertionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(OWLAsymmetricObjectPropertyAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(OWLReflexiveObjectPropertyAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(OWLDisjointClassesAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(OWLDataPropertyDomainAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(OWLObjectPropertyDomainAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(OWLEquivalentObjectPropertiesAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(OWLNegativeDataPropertyAssertionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(OWLDifferentIndividualsAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(OWLDisjointDataPropertiesAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(OWLDisjointObjectPropertiesAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(OWLObjectPropertyRangeAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(OWLObjectPropertyAssertionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbFunctionalObjectPropertyAxiom visit(OWLFunctionalObjectPropertyAxiom axiom) {
		OWLPropertyExpressionConverter peConverter = 
			OWLPropertyExpressionConverter.getInstance(); 		
		return new CbFunctionalObjectPropertyAxiom(axiom.getProperty().accept(peConverter));
	}

	@Override
	public CbSubObjectPropertyOfAxiom visit(OWLSubObjectPropertyOfAxiom axiom) {		
		OWLPropertyExpressionConverter peConverter = 
			OWLPropertyExpressionConverter.getInstance();
		return new CbSubObjectPropertyOfAxiom(axiom.getSubProperty().accept(peConverter), 
				axiom.getSuperProperty().accept(peConverter));
	}

	@Override
	public CbAxiom visit(OWLDisjointUnionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(OWLDeclarationAxiom axiom) {
		OWLEntity entity = axiom.getEntity();
		if (entity.isOWLClass())
			return new CbClassDeclarationAxiom(entity.getIRI().toString());
		else if (entity.isOWLObjectProperty())
			return new CbObjectPropertyDeclarationAxiom(entity.getIRI().toString());
		else throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(OWLAnnotationAssertionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(OWLSymmetricObjectPropertyAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(OWLDataPropertyRangeAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(OWLFunctionalDataPropertyAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(OWLEquivalentDataPropertiesAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(OWLClassAssertionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbEquivalentClassesAxiom visit(OWLEquivalentClassesAxiom axiom) {
		OWLClassExpressionConverter ceConverter = 
			OWLClassExpressionConverter.getInstance();
		List<OWLClassExpression> operands=axiom.getClassExpressionsAsList();
		int length=operands.size();
		CbClassExpression[] cbObjects=new CbClassExpression[length];
		int i = 0;
		for (OWLClassExpression ce : operands) {
			cbObjects[i] = ce.accept(ceConverter);
			i++;
		}
		return new CbEquivalentClassesAxiom(cbObjects);
	}

	@Override
	public CbAxiom visit(OWLDataPropertyAssertionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbTransitiveObjectPropertyAxiom visit(OWLTransitiveObjectPropertyAxiom axiom) {
		OWLPropertyExpressionConverter peConverter = 
			OWLPropertyExpressionConverter.getInstance(); 		
		return new CbTransitiveObjectPropertyAxiom(axiom.getProperty().accept(peConverter));		
	}

	@Override
	public CbAxiom visit(OWLIrreflexiveObjectPropertyAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(OWLSubDataPropertyOfAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbInverseFunctionalObjectPropertyAxiom visit(OWLInverseFunctionalObjectPropertyAxiom axiom) {
		OWLPropertyExpressionConverter peConverter = 
			OWLPropertyExpressionConverter.getInstance(); 		
		return new CbInverseFunctionalObjectPropertyAxiom(axiom.getProperty().accept(peConverter));		
	}

	@Override
	public CbAxiom visit(OWLSameIndividualAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(OWLSubPropertyChainOfAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbInverseObjectPropertiesAxiom visit(OWLInverseObjectPropertiesAxiom axiom) {
		OWLPropertyExpressionConverter peConverter = 
			OWLPropertyExpressionConverter.getInstance();				
		return new CbInverseObjectPropertiesAxiom(axiom.getFirstProperty().accept(peConverter),
				axiom.getSecondProperty().accept(peConverter));		
	}

	@Override
	public CbAxiom visit(OWLHasKeyAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(OWLDatatypeDefinitionAxiom axiom) {
		// TODO Auto-generated method stub
		throw new ConverterException(axiom.getAxiomType().getName() + " not supported");
	}

	@Override
	public CbAxiom visit(SWRLRule rule) {
		// TODO Auto-generated method stub
		throw new ConverterException(rule.getAxiomType().getName() + " not supported");
	}
	

}
