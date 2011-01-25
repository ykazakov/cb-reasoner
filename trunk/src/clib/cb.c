#include "cb.h"
#include <stdio.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <pthread.h>

/* prevent concurrent ocaml calls */
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

/* last happened exception and backtrace */
static char *exception;
static char *backtrace;

static char *exception_fmt = "CB exception in function %s";
static char *backtrace_fmt = "Raised at file: \"%s\", line %d";

#define CBregister_exception \
	static int cb__str_len; \
	static int cb__line = __LINE__; \
	cb__str_len = snprintf(NULL, 0, exception_fmt, __FUNCTION__); \
	exception = malloc((cb__str_len + 1) * sizeof(char)); \
	snprintf(exception, cb__str_len + 1, exception_fmt, __FUNCTION__); \
	cb__str_len = snprintf(NULL, 0, backtrace_fmt, __FILE__, cb__line); \
	backtrace = malloc((cb__str_len + 1) * sizeof(char)); \
	snprintf(backtrace, cb__str_len + 1, backtrace_fmt, __FILE__, cb__line);

char * cb_get_exception() {
	return exception;
}

char * cb_get_backtrace() {
	return backtrace;
}

/* FIXME: make sure the lock is acquired when the functions are called */
char * cb_exception_to_string(value exn) {
	CAMLparam1(exn);
	CAMLlocal1(res);
	static value *closure = NULL;
	if (closure == NULL)
		closure = caml_named_value("cb_exception_to_string");
	res = caml_callback(*closure, exn);
	CAMLreturnT (char*, String_val(res));
}

int got_exception(value v) {
	CAMLparam1(v);
	CAMLlocal2(exn, res);
	static value *closure = NULL;
	if (closure == NULL)
		closure = caml_named_value("cb_get_backtrace");
	if (Is_exception_result(v)) {
		exn = Extract_exception(v);
		exception = cb_exception_to_string(exn);
		res = caml_callback(*closure, Val_unit);
		backtrace = String_val(res);
		CAMLreturnT(int, 1);
	}
	CAMLreturnT(int, 0);
}

/* initialization */

int cb_startup(char **argv) {
	CAMLparam0();
	CAMLlocal2(res,exn);
	pthread_mutex_lock(&mutex);
	caml_startup(argv);
	fflush(stdout);
	pthread_mutex_unlock(&mutex);
	CAMLreturnT(int, 0);
}

/* custom progress monitor */

CAMLprim value pm_start(value pm, value message) {
	CAMLparam2(pm, message);
	PM * c_pm = (PM*) pm;
	c_pm->start(c_pm->pm, String_val(message));
	CAMLreturn(Val_unit);
}

CAMLprim value pm_report(value pm, value state, value max) {
	CAMLparam3(pm, state, max);
	PM * c_pm = (PM*) pm;
	c_pm->report(c_pm->pm, Int_val(state), Int_val(max));
	CAMLreturn(Val_unit);
}

CAMLprim value pm_finish(value pm) {
	CAMLparam1(pm);
	PM * c_pm = (PM*) pm;
	c_pm->finish(c_pm->pm);
	CAMLreturn(Val_unit);
}

/* cb progress monitor */

CBPM * cb_pm_alloc(value v) {
	CAMLparam1(v);
	CBPM * wrapper = malloc(sizeof(CBPM));
	if (wrapper == NULL)
		return NULL;
	wrapper->v = v;
	pthread_mutex_lock(&mutex);
	caml_register_generational_global_root(&(wrapper->v));
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (CBPM*, wrapper);
}

void cb_pm_delete(CBPM *pm) {
	pthread_mutex_lock(&mutex);
	caml_remove_generational_global_root(&(pm->v));
	pthread_mutex_unlock(&mutex);
	free(pm);
}

CBPM * cb_pm_stderr_new(void) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_pm_stderr_new");
	v = caml_callback_exn(*closure, Val_unit);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (CBPM*, NULL);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (CBPM*, cb_pm_alloc(v));
}

int cb_pm_stderr_start(CBPM *pm, const char *message) {
	CAMLparam0();
	CAMLlocal2(v,vmessage);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_pm_stderr_start");
	vmessage = caml_copy_string(message);
	v = caml_callback2_exn(*closure, pm->v, vmessage);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (int, 1);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (int, 1);;
}

int cb_pm_stderr_report(CBPM *pm, int state, int max) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_pm_stderr_report");
	v = caml_callback3_exn(*closure, pm->v, Val_int(state), Val_int(max));
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (int, 1);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (int, 1);;
}

int cb_pm_stderr_finish(CBPM *pm) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_pm_stderr_finish");
	v = caml_callback_exn(*closure, pm->v);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (int, 1);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (int, 1);;
}

/* class expression */

CBCE * class_expression_alloc(value v) {
	CAMLparam1(v);
	CBCE * wrapper = malloc(sizeof(CBCE));
	if (wrapper == NULL)
		return NULL;
	wrapper->v = v;
	pthread_mutex_lock(&mutex);
	caml_register_generational_global_root(&(wrapper->v));
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (CBCE*, wrapper);
}

void cb_class_expression_delete(CBCE * ce) {
	pthread_mutex_lock(&mutex);
	caml_remove_generational_global_root(&(ce->v));
	pthread_mutex_unlock(&mutex);
	free(ce);
}

int cb_class_expression_print(const CBCE *ce) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_class_expression_print");
	v = caml_callback_exn(*closure, ce->v);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (int, 0);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (int, 1);;
}

CBCE * cb_thing_get() {
	CAMLparam0();
	CAMLlocal2(v, viri);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_thing_get");
	v = caml_callback_exn(*closure, Val_unit);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (CBCE*, NULL);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (CBCE*, class_expression_alloc(v));
}

CBCE * cb_nothing_new() {
	CAMLparam0();
	CAMLlocal2(v, viri);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_nothing_get");
	v = caml_callback_exn(*closure, Val_unit);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (CBCE*, NULL);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (CBCE*, class_expression_alloc(v));
}

CBCE * cb_class_new(const char *iri) {
	CAMLparam0();
	CAMLlocal2(v, viri);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_class_get");
	viri = caml_copy_string(iri);
	v = caml_callback_exn(*closure, viri);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (CBCE*, NULL);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (CBCE*, class_expression_alloc(v));
}

CBCE * cb_object_intersection_of_new(int len, const CBCE ** cearr) {
	CAMLparam0();
	CAMLlocal2(va, v);
	int i;
	static value *closure = NULL;
	static value *closure_rec = NULL;
	if (len > 1) {
		v = cearr[0]->v;
		pthread_mutex_lock(&mutex);
		if (closure == NULL)
			closure = caml_named_value("cb_object_intersection_of_get");
		if (closure_rec == NULL)
			closure_rec = caml_named_value("cb_object_intersection_of_rec_get");
		for (i = 1; i < len; i++) {
			v = caml_callback2_exn(*closure_rec, v, cearr[i]->v);
		}
		v = caml_callback2_exn(*closure, cearr[0]->v, v);
		if (got_exception(v)) {
			pthread_mutex_unlock(&mutex);
			CAMLreturnT (CBCE*, NULL);
		}
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (CBCE*, class_expression_alloc(v));
	} else {
		CBregister_exception;
		CAMLreturnT (CBCE*, NULL);
	}
}

CBCE * cb_object_some_values_from_new(const CBOPE *ope, const CBCE *ce) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_object_some_values_from_get");
	v = caml_callback2_exn(*closure, ope->v, ce->v);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (CBCE*, NULL);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (CBCE*, class_expression_alloc(v));
}

/* object property expression */

CBOPE * object_property_expression_alloc(value v) {
	CAMLparam1(v);
	CBOPE * wrapper = malloc(sizeof(CBOPE));
	if (wrapper == NULL)
		return NULL;
	wrapper->v = v;
	pthread_mutex_lock(&mutex);
	caml_register_generational_global_root(&(wrapper->v));
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (CBOPE*, wrapper);
}

void cb_object_property_expression_delete(CBOPE * ope) {
	pthread_mutex_lock(&mutex);
	caml_remove_generational_global_root(&(ope->v));
	pthread_mutex_unlock(&mutex);
	free(ope);
}

int cb_object_property_expression_print(const CBOPE *ope) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_object_property_expression_print");
	v = caml_callback_exn(*closure, ope->v);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (int, 0);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (int, 1);;
}

CBOPE * cb_top_object_property_get() {
	CAMLparam0();
	CAMLlocal2(v, viri);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_top_object_property_get");
	v = caml_callback_exn(*closure, Val_unit);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (CBOPE*, NULL);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (CBOPE*, object_property_expression_alloc(v));
}

CBOPE * cb_bottom_object_property_get() {
	CAMLparam0();
	CAMLlocal2(v, viri);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_bottom_object_property_get");
	v = caml_callback_exn(*closure, Val_unit);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (CBOPE*, NULL);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (CBOPE*, object_property_expression_alloc(v));
}

CBOPE * cb_object_property_new(const char *iri) {
	CAMLparam0();
	CAMLlocal2(v, viri);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_object_property_get");
	viri = caml_copy_string(iri);
	v = caml_callback_exn(*closure, viri);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (CBOPE*, NULL);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (CBOPE*, object_property_expression_alloc(v));
}

CBOPE * cb_object_inverse_of_new(const char *iri) {
	CAMLparam0();
	CAMLlocal2(v, viri);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_object_inverse_of_get");
	viri = caml_copy_string(iri);
	v = caml_callback_exn(*closure, viri);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (CBOPE*, NULL);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (CBOPE*, object_property_expression_alloc(v));
}

/* class axiom */

CBCAX * class_axiom_alloc(value v) {
	CAMLparam1(v);
	CBCAX * wrapper = malloc(sizeof(CBCAX));
	if (wrapper == NULL)
		return NULL;
	wrapper->v = v;
	pthread_mutex_lock(&mutex);
	caml_register_generational_global_root(&(wrapper->v));
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (CBCAX*, wrapper);
}

void cb_class_axiom_delete(CBCAX * ax) {
	pthread_mutex_lock(&mutex);
	caml_remove_generational_global_root(&(ax->v));
	pthread_mutex_unlock(&mutex);
	free(ax);
}

int cb_class_axiom_print(const CBCAX *ax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_class_axiom_print");
	v = caml_callback_exn(*closure, ax->v);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (int, 0);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (int, 1);;
}

CBCAX * cb_sub_class_of_axiom_new(const CBCE *cea, const CBCE *ceb) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_sub_class_of_axiom_get");
	v = caml_callback2_exn(*closure, cea->v, ceb->v);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (CBCAX*, NULL);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (CBCAX*, class_axiom_alloc(v));
}

CBCAX * cb_equivalent_classes_axiom_new(int len, const CBCE ** cearr) {
	CAMLparam0();
	CAMLlocal3(v, cli, cons);
	int i;
	if (len > 1) {
		pthread_mutex_lock(&mutex);
		static value *closure = NULL;
		if (closure == NULL)
			closure = caml_named_value("cb_equivalent_classes_axiom_get");
		cli = Val_emptylist;
		for (i = len - 1; i >= 0; i--) {
			cons = caml_alloc(2, 0);
			Store_field(cons, 0, cearr[i]->v); // head
			Store_field(cons, 1, cli); // tail
			cli = cons;
		}
		v = caml_callback_exn(*closure, cli);
		if (got_exception(v)) {
			pthread_mutex_unlock(&mutex);
			CAMLreturnT (CBCAX*, NULL);
		}
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (CBCAX*, class_axiom_alloc(v));
	} else {
		CBregister_exception;
		CAMLreturnT (CBCAX*, NULL);
	}
}

/* object property axiom */

CBOPAX * object_property_axiom_alloc(value v) {
	CAMLparam1(v);
	CBOPAX * wrapper = malloc(sizeof(CBOPAX));
	if (wrapper == NULL)
		return NULL;
	wrapper->v = v;
	pthread_mutex_lock(&mutex);
	caml_register_generational_global_root(&(wrapper->v));
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (CBOPAX*, wrapper);
}

void cb_object_property_axiom_delete(CBOPAX * ax) {
	pthread_mutex_lock(&mutex);
	caml_remove_generational_global_root(&(ax->v));
	pthread_mutex_unlock(&mutex);
	free(ax);
}

int cb_object_property_axiom_print(const CBOPAX *ax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_object_property_axiom_print");
	v = caml_callback_exn(*closure, ax->v);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (int, 0);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (int, 1);;
}

CBOPAX * cb_sub_object_property_of_axiom_new(int len, const CBOPE **opearr,
		const CBOPE *opeb) {
	CAMLparam0();
	CAMLlocal3(v, cli, cons);
	int i;
	if (len > 0) {
		pthread_mutex_lock(&mutex);
		static value *closure = NULL;
		if (closure == NULL)
			closure = caml_named_value("cb_sub_object_property_of_axiom_get");
		cli = Val_emptylist;
		for (i = len - 1; i >= 0; i--) {
			cons = caml_alloc(2, 0);
			Store_field(cons, 0, opearr[i]->v); // head
			Store_field(cons, 1, cli); // tail
			cli = cons;
		}
		v = caml_callback2_exn(*closure, cli, opeb->v);
		if (got_exception(v)) {
			pthread_mutex_unlock(&mutex);
			CAMLreturnT (CBOPAX*, NULL);
		}
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (CBOPAX*, object_property_axiom_alloc(v));
	} else {
		CBregister_exception;
		CAMLreturnT (CBOPAX*, NULL);
	}
}

CBOPAX * cb_inverse_object_properties_axiom_new(const CBOPE *opea,
		const CBOPE *opeb) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_inverse_object_properties_axiom_get");
	v = caml_callback2_exn(*closure, opea->v, opeb->v);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (CBOPAX*, NULL);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (CBOPAX*, object_property_axiom_alloc(v));
}

CBOPAX * cb_functional_object_property_axiom_new(const CBOPE *ope) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_functional_object_property_axiom_get");
	v = caml_callback_exn(*closure, ope->v);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (CBOPAX*, NULL);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (CBOPAX*, object_property_axiom_alloc(v));
}

CBOPAX * cb_inverse_functional_object_property_axiom_new(const CBOPE *ope) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value(
				"cb_inverse_functional_object_property_axiom_get");
	v = caml_callback_exn(*closure, ope->v);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (CBOPAX*, NULL);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (CBOPAX*, object_property_axiom_alloc(v));
}

CBOPAX * cb_transitive_object_property_axiom_new(const CBOPE *ope) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_transitive_object_property_axiom_get");
	v = caml_callback_exn(*closure, ope->v);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (CBOPAX*, NULL);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (CBOPAX*, object_property_axiom_alloc(v));
}

/* ontology */

CBONT * ontology_alloc(value v) {
	CAMLparam1(v);
	CBONT * wrapper = malloc(sizeof(CBONT));
	if (wrapper == NULL)
		return NULL;
	wrapper->v = v;
	pthread_mutex_lock(&mutex);
	caml_register_generational_global_root(&(wrapper->v));
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (CBONT*, wrapper);
}

void cb_ontology_delete(CBONT *ont) {
	pthread_mutex_lock(&mutex);
	caml_remove_generational_global_root(&(ont->v));
	pthread_mutex_unlock(&mutex);
	free(ont);
}

CBONT * cb_ontology_new(void) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_ontology_new");
	v = caml_callback_exn(*closure, Val_unit);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (CBONT*, NULL);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (CBONT*, ontology_alloc(v));
}

int cb_ontology_add_class_axiom(CBONT *ont, const CBCAX *cax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_ontology_add_class_axiom");
	v = caml_callback2_exn(*closure, ont->v, cax->v);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (int, 1);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (int, 1);;
}

int cb_ontology_add_object_property_axiom(CBONT *ont, const CBOPAX *opax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_ontology_add_object_property_axiom");
	v = caml_callback2_exn(*closure, ont->v, opax->v);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (int, 0);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (int, 1);;
}

int cb_ontology_print_info(const CBONT *ont) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_ontology_print_info");
	v = caml_callback_exn(*closure, ont->v);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (int, 0);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (int, 1);;
}

int cb_ontology_classify(CBONT *ont) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_ontology_classify");
	v = caml_callback_exn(*closure, ont->v);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (int, 0);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (int, 1);
}

int cb_ontology_classify_pm(CBONT *ont, PM *pm) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	pthread_mutex_lock(&mutex);
	if (closure == NULL)
		closure = caml_named_value("cb_ontology_classify_pm");
	v = caml_callback2_exn(*closure, ont->v, (value) pm);
	if (got_exception(v)) {
		pthread_mutex_unlock(&mutex);
		CAMLreturnT (int, 0);
	}
	pthread_mutex_unlock(&mutex);
	CAMLreturnT (int, 1);
}

/* Querying taxonomy */

//CBCE** cb_ontology_get_equivalent_classes(CBONT *ont, CBCE *ce) {
//	CAMLparam0();
//	CAMLlocal2(v,head);
//	static value *closure = NULL;
//	pthread_mutex_lock(&mutex);
//	if (closure == NULL)
//		closure = caml_named_value("cb_ontology_get_equivalent_classes");
//	v = caml_callback2_exn(*closure, ont->v, ce->v);
//	if (got_exception(v)) {
//		pthread_mutex_unlock(&mutex);
//		CAMLreturnT (CBCE**, 0);
//	}
//	CBCE* res[];
//	int i = 0;
//	while (v != Val_emptylist) {
//		head = Field(ml_list, 0); /* accessing the head */
//		res[i] = head;
//		ml_list = Field(ml_list, 1); /* point to the tail for next loop */
//	}
//	pthread_mutex_unlock(&mutex);
//	CAMLreturnT (CBCE**, res);
//}
