package org.semanticweb.cb.owlapi;

import java.util.HashSet;
import java.util.Set;

import org.semanticweb.cb.reasoner.CbAxiom;
import org.semanticweb.cb.reasoner.CbClass;
import org.semanticweb.cb.reasoner.CbClassExpression;
import org.semanticweb.cb.reasoner.CbClassNode;
import org.semanticweb.cb.reasoner.CbClassTaxonomyNode;
import org.semanticweb.cb.reasoner.CbEquivalentClassesAxiom;
import org.semanticweb.cb.reasoner.CbFunctionalObjectPropertyAxiom;
import org.semanticweb.cb.reasoner.CbInverseFunctionalObjectPropertyAxiom;
import org.semanticweb.cb.reasoner.CbObjectIntersectionOf;
import org.semanticweb.cb.reasoner.CbObjectInverseOf;
import org.semanticweb.cb.reasoner.CbObjectProperty;
import org.semanticweb.cb.reasoner.CbObjectPropertyExpression;
import org.semanticweb.cb.reasoner.CbObjectSomeValuesFrom;
import org.semanticweb.cb.reasoner.CbOntology;
import org.semanticweb.cb.reasoner.CbSubClassOfAxiom;
import org.semanticweb.cb.reasoner.CbSubObjectPropertyOfAxiom;
import org.semanticweb.cb.reasoner.CbTransitiveObjectPropertyAxiom;
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
	
	public static CbClass convert(OWLClass cls) {
		OWLClassExpressionConverter converter = OWLClassExpressionConverter.getInstance();
		return converter.visit(cls);		
	}
	
	public static CbObjectIntersectionOf convert(OWLObjectIntersectionOf ce) {
		OWLClassExpressionConverter converter = OWLClassExpressionConverter.getInstance();
		return converter.visit(ce);		
	}
	
	public static CbObjectSomeValuesFrom convert(OWLObjectSomeValuesFrom ce) {
		OWLClassExpressionConverter converter = OWLClassExpressionConverter.getInstance();
		return converter.visit(ce);		
	}
	
	public static CbClassExpression convert(OWLClassExpression ce) {
		OWLClassExpressionConverter converter = OWLClassExpressionConverter.getInstance();
		return ce.accept(converter);
	}
	
	public static CbObjectProperty convert(OWLObjectProperty op) {
		OWLPropertyExpressionConverter converter = OWLPropertyExpressionConverter.getInstance();
		return converter.visit(op);		
	}
	
	public static CbObjectInverseOf convert(OWLObjectInverseOf op) {
		OWLPropertyExpressionConverter converter = OWLPropertyExpressionConverter.getInstance();
		return converter.visit(op);		
	}
	
	public static CbObjectPropertyExpression convert(OWLObjectPropertyExpression pe) {
		OWLPropertyExpressionConverter converter = OWLPropertyExpressionConverter.getInstance();
		return pe.accept(converter);
	}
	
	public static CbEquivalentClassesAxiom convert(OWLEquivalentClassesAxiom ax) {
		OWLAxiomConverter converter = OWLAxiomConverter.getInstance();
		return converter.visit(ax);
	}
	
	public static CbFunctionalObjectPropertyAxiom convert(OWLFunctionalObjectPropertyAxiom ax) {
		OWLAxiomConverter converter = OWLAxiomConverter.getInstance();
		return converter.visit(ax);
	}
	
	public static CbInverseFunctionalObjectPropertyAxiom convert(OWLInverseFunctionalObjectPropertyAxiom ax) {
		OWLAxiomConverter converter = OWLAxiomConverter.getInstance();
		return converter.visit(ax);
	}
	
	public static CbSubClassOfAxiom convert(OWLSubClassOfAxiom ax) {
		OWLAxiomConverter converter = OWLAxiomConverter.getInstance();
		return converter.visit(ax);
	}
	
	public static CbSubObjectPropertyOfAxiom convert(OWLSubObjectPropertyOfAxiom ax) {
		OWLAxiomConverter converter = OWLAxiomConverter.getInstance();
		return converter.visit(ax);
	}
	
	public static CbTransitiveObjectPropertyAxiom convert(OWLTransitiveObjectPropertyAxiom ax) {
		OWLAxiomConverter converter = OWLAxiomConverter.getInstance();
		return converter.visit(ax);
	}
	
	public static CbAxiom convert(OWLAxiom ax) {
		OWLAxiomConverter converter = OWLAxiomConverter.getInstance();
		return ax.accept(converter);
	}
	
	public static CbOntology convert(OWLOntology ont) {
		CbOntology cbOntology = new CbOntology();		
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
	
	public static OWLClass convert(CbClass cls) {
		CbClassExpressionConverter converter = CbClassExpressionConverter.getInstance();
		return converter.visit(cls);		
	}
	
	public static OWLObjectIntersectionOf convert(CbObjectIntersectionOf ce) {
		CbClassExpressionConverter converter = CbClassExpressionConverter.getInstance();
		return converter.visit(ce);		
	}
	
	public static OWLObjectSomeValuesFrom convert(CbObjectSomeValuesFrom ce) {
		CbClassExpressionConverter converter = CbClassExpressionConverter.getInstance();
		return converter.visit(ce);		
	}
	
	public static OWLClassExpression convert(CbClassExpression ce) {
		CbClassExpressionConverter converter = CbClassExpressionConverter.getInstance();
		return ce.accept(converter);
	}
	
	public static OWLClassNode convert(CbClassNode node) {
		Set<OWLClass> owlClasses = new HashSet<OWLClass>();
		for (CbClass cls : node.getClasses()) {
			owlClasses.add(convert(cls));
		}
		return new OWLClassNode(owlClasses);
	}
	
	public static OWLClassNodeSet convert(CbClassTaxonomyNode[] nodes) {
		Set<Node<OWLClass>> owlNodes = new HashSet<Node<OWLClass>>();
		for (CbClassTaxonomyNode node : nodes) {
			owlNodes.add(convert(node));					
		}
		return new OWLClassNodeSet(owlNodes);
	}	
	
}
