#ifndef CB_H_          /* duplication check */
#define CB_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <caml/mlvalues.h>

/* types */

typedef struct cb_progress_monitor {
	value v;
} CBPM;
typedef struct cb_ontology {
	value v;
} CBONT;
typedef struct cb_class_expression {
	value v;
} CBCE;
typedef struct cb_object_property_expression {
	value v;
} CBOPE;
typedef struct cb_declaration_axiom {
	value v;
} CBDAX;
typedef struct cb_class_axiom {
	value v;
} CBCAX;
typedef struct cb_object_property_axiom {
	value v;
} CBOPAX;
typedef struct cb_class_node {
	value v;
} CBCN;
typedef struct cb_class_axiom_iterator {
	value v;
} CBCAXI;


/* constructor type cases */
typedef enum class_expression_cases {
	CB_Class,
	CB_ObjectIntersectionOf,
	CB_ObjectIntersectionOfrec,
	CB_ObjectUnionOf,
	CB_ObjectUnionOfrec,
	CB_ObjectComplementOf,
	CB_ObjectOneOf,
	CB_ObjectSomeValuesFrom,
	CB_ObjectAllValuesFrom,
	CB_ObjectHasValue,
	CB_ObjectHasSelf,
	CB_ObjectMinCardinality,
	CB_ObjectMaxCardinality,
	CB_ObjectExactCardinality,
	CB_DataSomeValuesFrom,
	CB_DataAllValuesFrom,
	CB_DataHasValue,
	CB_DataMinCardinality,
	CB_DataMaxCardinality,
	CB_DataExactCardinality
} CBCET;

typedef enum object_property_expression_cases {
	CB_ObjectProperty,
	CB_ObjectInverseOf
} CBOPET;

typedef enum declaration_axiom_cases {
	CB_ClassDeclaration,
	CB_DatatypeDeclaration,
	CB_ObjectPropertyDeclaration,
	CB_DataPropertyDeclaration,
	CB_AnnotationPropertyDeclaration,
	CB_NamedIndividualDeclaration
} CBDAXT;

typedef enum class_axiom_cases {
	CB_SubClassOf,
	CB_EquivalentClasses,
	CB_DisjointClasses,
	CB_DisjointUnion
} CBCAXT;

typedef enum object_property_axiom_cases {
	CB_SubObjectPropertyOf,
	CB_EquivalentObjectProperties,
	CB_DisjointObjectProperties,
	CB_InverseObjectProperties,
	CB_ObjectPropertyDomain,
	CB_ObjectPropertyRange,
	CB_FunctionalObjectProperty,
	CB_InverseFunctionalObjectProperty,
	CB_ReflexiveObjectProperty,
	CB_IrreflexiveObjectProperty,
	CB_SymmetricObjectProperty,
	CB_AsymmetricObjectProperty,
	CB_TransitiveObjectProperty
} CBOPAXT;


/* custom progress monitors */
typedef struct progress_monitor {
	void * pm; /* the progress monitor object */
	void (*start)(void * pm, const char *message);
	void (*report)(void *pm, int state, int max);
	void (*finish)(void *pm);
} PM;

/* exception handling */

char * cb_get_exception();
char * cb_get_backtrace();

/* initialization */

int cb_startup(char **);

/* progress monitor */

void cb_pm_delete(CBPM *pm);
CBPM * cb_pm_stderr_new();
int cb_pm_stderr_start(CBPM *pm, const char *message);
int cb_pm_stderr_report(CBPM *pm, int state, int max);
int cb_pm_stderr_finish(CBPM *pm);

/* class expression */

void cb_class_expression_delete (CBCE *);
char * cb_class_get_iri (const CBCE *);
int cb_class_expression_print (const CBCE *);
CBCE ** cb_object_intersection_of_get_operands(const CBCE *, int *);
CBOPE * cb_object_some_values_from_get_property(const CBCE *);
CBCE * cb_object_some_values_from_get_filler(const CBCE *);
CBCE * cb_thing_get ();
CBCE * cb_nothing_get ();
CBCE * cb_class_new (const char *);
CBCE * cb_object_intersection_of_new (int, CBCE * const *);
CBCE * cb_object_some_values_from_new(const CBOPE *, const CBCE *);
CBCET cb_class_expression_get_case (const CBCE *);

/* object property expression */

void cb_object_property_expression_delete (CBOPE *);
char * cb_object_property_get_iri (const CBOPE *);
char * cb_object_inverse_of_get_iri (const CBOPE *);
int cb_object_property_expression_print (const CBOPE *);
CBOPE * cb_top_object_property_get ();
CBOPE * cb_bottom_object_property_get ();
CBOPE * cb_object_property_new (const char *);
CBOPE * cb_object_inverse_of_new (const char *);
CBOPET cb_object_property_expression_get_case(const CBOPE *);

/* declarations */

void cb_declaration_axiom_delete (CBDAX *);
int cb_declaration_axiom_print (const CBDAX *);
CBDAX * cb_class_declaration_axiom_new (const char *);
CBDAX * cb_object_property_declaration_axiom_new (const char *);
CBDAXT cb_declaration_axiom_get_case(const CBDAX *);

/* class axiom */

void cb_class_axiom_delete (CBCAX *);
CBCE * cb_sub_class_of_axiom_get_sub_class(const CBCAX *);
CBCE * cb_sub_class_of_axiom_get_super_class(const CBCAX *);
CBCE ** cb_equivalent_classes_axiom_get_class_expressions(const CBCAX *, int *);
int cb_class_axiom_print (const CBCAX *);
CBCAX * cb_sub_class_of_axiom_new (const CBCE *, const CBCE *);
CBCAX * cb_equivalent_classes_axiom_new (int, CBCE * const *);
CBCAXT cb_class_axiom_get_case(const CBCAX *);

/* object property axiom */

void cb_object_property_axiom_delete (CBOPAX *);
CBOPE ** cb_sub_object_property_of_axiom_get_sub_property_chain(const CBOPAX *, int *);
CBOPE * cb_sub_object_property_of_axiom_get_super_property(const CBOPAX *);
CBOPE * cb_inverse_object_properties_axiom_get_first_property(const CBOPAX *);
CBOPE * cb_inverse_object_properties_axiom_get_second_property(const CBOPAX *);
CBOPE * cb_functional_object_property_axiom_get_property(const CBOPAX *);
CBOPE * cb_inverse_functional_object_property_axiom_get_property(const CBOPAX *);
CBOPE * cb_transitive_object_property_axiom_get_property(const CBOPAX *);
int cb_object_property_axiom_print (const CBOPAX *);
CBOPAX * cb_sub_object_property_of_axiom_new (int, CBOPE * const *, const CBOPE *);
CBOPAX * cb_inverse_object_properties_axiom_new (const CBOPE *, const CBOPE *);
CBOPAX * cb_functional_object_property_axiom_new (const CBOPE *);
CBOPAX * cb_inverse_functional_object_property_axiom_new (const CBOPE *);
CBOPAX * cb_transitive_object_property_axiom_new (const CBOPE *);
CBOPAXT cb_object_property_axiom_get_case(const CBOPAX *);

/* ontology */

void cb_ontology_delete (CBONT *);
CBONT * cb_ontology_new ();
int cb_ontology_add_declaration_axiom(CBONT *, const CBDAX *);
int cb_ontology_remove_declaration_axiom(CBONT *, const CBDAX *);
int cb_ontology_add_class_axiom(CBONT *, const CBCAX *);
int cb_ontology_remove_class_axiom(CBONT *, const CBCAX *);
int cb_ontology_add_object_property_axiom(CBONT *, const CBOPAX *);
int cb_ontology_remove_object_property_axiom(CBONT *, const CBOPAX *);
int cb_ontology_print_info (const CBONT *);
int cb_ontology_classify (CBONT *);
int cb_ontology_classify_pm(CBONT *, PM *);

/* class axiom iterator */

void cb_class_axiom_iterator_delete(CBCAXI *);
CBCAXI * cb_class_axiom_iterator_new(const CBONT *);
int cb_class_axiom_iterator_has_next(const CBCAXI *);
CBCAX * cb_class_axiom_iterator_next(const CBCAXI *);
int cb_class_axiom_iterator_remove(CBCAXI *itr);

/* class nodes */

void cb_class_node_delete (CBCN *);
CBCN * cb_class_node_get(const CBONT *, const CBCE *);
CBCN * cb_class_node_top_get (const CBONT *);
CBCN * cb_class_node_bot_get (const CBONT *);
CBCE ** cb_class_node_get_classes(const CBCN *, int *sp);
CBCN ** cb_class_node_get_child_nodes(const CBCN *, int *sp);
CBCN ** cb_class_node_get_parent_nodes(const CBCN *, int *sp);

#ifdef __cplusplus
}
#endif

#endif /* CB_H_ */
