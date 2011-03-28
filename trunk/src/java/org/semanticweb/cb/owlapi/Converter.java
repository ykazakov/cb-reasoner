package org.semanticweb.cb.owlapi;

import java.util.HashSet;
import java.util.Set;

import org.semanticweb.cb.reasoner.CBAxiom;
import org.semanticweb.cb.reasoner.CBClass;
import org.semanticweb.cb.reasoner.CBClassExpression;
import org.semanticweb.cb.reasoner.CBClassNode;
import org.semanticweb.cb.reasoner.CBClassTaxonomyNode;
import org.semanticweb.cb.reasoner.CBEquivalentClassesAxiom;
import org.semanticweb.cb.reasoner.CBFunctionalObjectPropertyAxiom;
import org.semanticweb.cb.reasoner.CBInverseFunctionalObjectPropertyAxiom;
import org.semanticweb.cb.reasoner.CBObjectIntersectionOf;
import org.semanticweb.cb.reasoner.CBObjectInverseOf;
import org.semanticweb.cb.reasoner.CBObjectProperty;
import org.semanticweb.cb.reasoner.CBObjectPropertyExpression;
import org.semanticweb.cb.reasoner.CBObjectSomeValuesFrom;
import org.semanticweb.cb.reasoner.CBOntology;
import org.semanticweb.cb.reasoner.CBSubClassOfAxiom;
import org.semanticweb.cb.reasoner.CBSubObjectPropertyOfAxiom;
import org.semanticweb.cb.reasoner.CBTransitiveObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom;
import org.semanticweb.owlapi.model.OWLFunctionalObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLInverseFunctionalObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf;
import org.semanticweb.owlapi.model.OWLObjectInverseOf;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom;
import org.semanticweb.owlapi.model.OWLSubObjectPropertyOfAxiom;
import org.semanticweb.owlapi.model.OWLTransitiveObjectPropertyAxiom;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.impl.OWLClassNode;
import org.semanticweb.owlapi.reasoner.impl.OWLClassNodeSet;

public abstract class Converter {
	
	/* OWL to CB */
	
	public static CBClass convert(OWLClass cls) {
		OWLClassExpressionConverter converter = OWLClassExpressionConverter.getInstance();
		return converter.visit(cls);		
	}
	
	public static CBObjectIntersectionOf convert(OWLObjectIntersectionOf ce) {
		OWLClassExpressionConverter converter = OWLClassExpressionConverter.getInstance();
		return converter.visit(ce);		
	}
	
	public static CBObjectSomeValuesFrom convert(OWLObjectSomeValuesFrom ce) {
		OWLClassExpressionConverter converter = OWLClassExpressionConverter.getInstance();
		return converter.visit(ce);		
	}
	
	public static CBClassExpression convert(OWLClassExpression ce) {
		OWLClassExpressionConverter converter = OWLClassExpressionConverter.getInstance();
		return ce.accept(converter);
	}
	
	public static CBObjectProperty convert(OWLObjectProperty op) {
		OWLPropertyExpressionConverter converter = OWLPropertyExpressionConverter.getInstance();
		return converter.visit(op);		
	}
	
	public static CBObjectInverseOf convert(OWLObjectInverseOf op) {
		OWLPropertyExpressionConverter converter = OWLPropertyExpressionConverter.getInstance();
		return converter.visit(op);		
	}
	
	public static CBObjectPropertyExpression convert(OWLObjectPropertyExpression pe) {
		OWLPropertyExpressionConverter converter = OWLPropertyExpressionConverter.getInstance();
		return pe.accept(converter);
	}
	
	public static CBEquivalentClassesAxiom convert(OWLEquivalentClassesAxiom ax) {
		OWLAxiomConverter converter = OWLAxiomConverter.getInstance();
		return converter.visit(ax);
	}
	
	public static CBFunctionalObjectPropertyAxiom convert(OWLFunctionalObjectPropertyAxiom ax) {
		OWLAxiomConverter converter = OWLAxiomConverter.getInstance();
		return converter.visit(ax);
	}
	
	public static CBInverseFunctionalObjectPropertyAxiom convert(OWLInverseFunctionalObjectPropertyAxiom ax) {
		OWLAxiomConverter converter = OWLAxiomConverter.getInstance();
		return converter.visit(ax);
	}
	
	public static CBSubClassOfAxiom convert(OWLSubClassOfAxiom ax) {
		OWLAxiomConverter converter = OWLAxiomConverter.getInstance();
		return converter.visit(ax);
	}
	
	public static CBSubObjectPropertyOfAxiom convert(OWLSubObjectPropertyOfAxiom ax) {
		OWLAxiomConverter converter = OWLAxiomConverter.getInstance();
		return converter.visit(ax);
	}
	
	public static CBTransitiveObjectPropertyAxiom convert(OWLTransitiveObjectPropertyAxiom ax) {
		OWLAxiomConverter converter = OWLAxiomConverter.getInstance();
		return converter.visit(ax);
	}
	
	public static CBAxiom convert(OWLAxiom ax) {
		OWLAxiomConverter converter = OWLAxiomConverter.getInstance();
		return ax.accept(converter);
	}
	
	public static CBOntology convert(OWLOntology ont) {
		CBOntology cbOntology = new CBOntology();		
		for (OWLAxiom ax : ont.getLogicalAxioms()) {
			    try {
				cbOntology.add(convert(ax));
			    } catch (ConverterException e) {
			    	System.out.println("Axiom ignored: " + ax.toString() + ": " + e.getMessage());
			    }
		}
		return cbOntology;
	}
	
	/* CB to OWL */
	
	public static OWLClass convert(CBClass cls) {
		CBClassExpressionConverter converter = CBClassExpressionConverter.getInstance();
		return converter.visit(cls);		
	}
	
	public static OWLObjectIntersectionOf convert(CBObjectIntersectionOf ce) {
		CBClassExpressionConverter converter = CBClassExpressionConverter.getInstance();
		return converter.visit(ce);		
	}
	
	public static OWLObjectSomeValuesFrom convert(CBObjectSomeValuesFrom ce) {
		CBClassExpressionConverter converter = CBClassExpressionConverter.getInstance();
		return converter.visit(ce);		
	}
	
	public static OWLClassExpression convert(CBClassExpression ce) {
		CBClassExpressionConverter converter = CBClassExpressionConverter.getInstance();
		return ce.accept(converter);
	}
	
	public static OWLClassNode convert(CBClassNode node) {
		Set<OWLClass> owlClasses = new HashSet<OWLClass>();
		for (CBClass cls : node.getCBClasses()) {
			owlClasses.add(convert(cls));
		}
		return new OWLClassNode(owlClasses);
	}
	
	public static OWLClassNodeSet convert(CBClassTaxonomyNode[] nodes) {
		Set<Node<OWLClass>> owlNodes = new HashSet<Node<OWLClass>>();
		for (CBClassTaxonomyNode node : nodes) {
			owlNodes.add(convert(node));					
		}
		return new OWLClassNodeSet(owlNodes);
	}	
	
}
