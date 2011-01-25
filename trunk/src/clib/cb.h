#ifndef CB_H_          /* duplication check */
#define CB_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <caml/mlvalues.h>

/* types */

typedef struct caml_progress_monitor {
	value v;
} CBPM;
typedef struct caml_ontology {
	value v;
} CBONT;
typedef struct caml_class_expression {
	value v;
} CBCE;
typedef struct caml_object_property_expression {
	value v;
} CBOPE;
typedef struct caml_class_axiom {
	value v;
} CBCAX;
typedef struct caml_object_property_axiom {
	value v;
} CBOPAX;
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
int cb_class_expression_print (const CBCE *);
CBCE * cb_thing_get ();
CBCE * cb_nothing_get ();
CBCE * cb_class_new (const char *);
CBCE * cb_object_intersection_of_new (int, const CBCE **);
CBCE * cb_object_some_values_from_new(const CBOPE *, const CBCE *);

/* object property expression */

void cb_object_property_expression_delete (CBOPE *);
int cb_object_property_expression_print (const CBOPE *);
CBOPE * cb_top_object_property_get ();
CBOPE * cb_bottom_object_property_get ();
CBOPE * cb_object_property_new (const char *);
CBOPE * cb_object_inverse_of_new (const char *);

/* class axiom */

void cb_class_axiom_delete (CBCAX *);
int cb_class_axiom_print (const CBCAX *);
CBCAX * cb_sub_class_of_axiom_new (const CBCE *, const CBCE *);
CBCAX * cb_equivalent_classes_axiom_new (int, const CBCE **);

/* object property axiom */

void cb_object_property_axiom_delete (CBOPAX *);
int cb_object_property_axiom_print (const CBOPAX *);
CBOPAX * cb_sub_object_property_of_axiom_new (int, const CBOPE **, const CBOPE *);
CBOPAX * cb_inverse_object_properties_axiom_new (const CBOPE *, const CBOPE *);
CBOPAX * cb_functional_object_property_axiom_new (const CBOPE *);
CBOPAX * cb_inverse_functional_object_property_axiom_new (const CBOPE *);
CBOPAX * cb_transitive_object_property_axiom_new (const CBOPE *);

/* ontology */

void cb_ontology_delete (CBONT *);
CBONT * cb_ontology_new ();
int cb_ontology_add_class_axiom(CBONT *, const CBCAX *);
int cb_ontology_add_object_property_axiom(CBONT *, const CBOPAX *);
int cb_ontology_print_info (const CBONT *);
int cb_ontology_classify (CBONT *);
int cb_ontology_classify_pm(CBONT *, PM *);

#ifdef __cplusplus
}
#endif

#endif /* CB_H_ */
