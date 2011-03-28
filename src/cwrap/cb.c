#include "cb.h"
#include <stdio.h>
#include <caml/callback.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#if defined (__PTHREAD__)
#include <pthread.h>
#elif defined (__WINDOWS__)
#include <windows.h>
#else
# warning "no thread library"
#endif


/* last happened exception and backtrace */
char *exception;
char *backtrace;

static char *exception_fmt = "CB exception in function %s";
static char *backtrace_first_fmt = "Raised from \"%s\":%s:%d\n";
static char *backtrace_other_fmt = "%sCalled from  \"%s\":%s:%d\n";

#if defined (__MSVC__)
#define snprintf sprintf_s
#endif

void cb_register_exception(const char *file, const char *function,
		const int line) {
	int len;
	char *old_backtrace;
	if (!exception) {
		len = snprintf(NULL, 0, exception_fmt, function);
		exception = malloc((len + 1) * sizeof(char));
		snprintf(exception, len + 1, exception_fmt, function);
	}
	if (!backtrace) {
		len = snprintf(NULL, 0, backtrace_first_fmt, file, line);
		backtrace = malloc((len + 1) * sizeof(char));
		snprintf(backtrace, len + 1, backtrace_first_fmt, file, line);
	} else {
		old_backtrace = backtrace;
		len
				= snprintf(NULL, 0, backtrace_other_fmt, old_backtrace, file, function, line);
		backtrace = malloc((len + 1) * sizeof(char));
		snprintf(backtrace, len + 1, backtrace_other_fmt, old_backtrace, file, function, line);
	}
}

#define CBregister_exception \
	cb_register_exception(__FILE__, __FUNCTION__, __LINE__);

#if defined (__PTHREAD__)
pthread_mutex_t __cb_mutex = PTHREAD_MUTEX_INITIALIZER;
#define CBmutex_lock pthread_mutex_lock(&__cb_mutex);
#define CBmutex_unlock pthread_mutex_unlock(&__cb_mutex);
#elif defined (__WINDOWS__)
#include <windows.h>
static HANDLE __cb_mutex;
void __cb_mutex_lock() {
	if (__cb_mutex == NULL)
	__cb_mutex = CreateMutex(0, FALSE, 0);
	WaitForSingleObject(__cb_mutex, INFINITE);
}
void __cb_mutex_unlock() {
	if (__cb_mutex == NULL) {
		CBregister_exception;
	}
	ReleaseMutex(__cb_mutex);
}
#define CBmutex_lock __cb_mutex_lock();
#define CBmutex_unlock __cb_mutex_unlock();
#endif

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

int is_exception(value v, const char *file, const char *function,
		const int line) {
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
		cb_register_exception(file, function, line);
		CAMLreturnT(int, 1);
	}
	CAMLreturnT(int, 0);
}

#define CB_exception(v) \
	is_exception(v, __FILE__, __FUNCTION__, __LINE__)

/* initialization */

int cb_startup(char **argv) {
	CAMLparam0();
	CAMLlocal2(res,exn);
	CBmutex_lock;
	caml_startup(argv);
	CBmutex_unlock;
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
	CBmutex_lock;
	caml_register_generational_global_root(&(wrapper->v));
	CBmutex_unlock;
	CAMLreturnT (CBPM*, wrapper);
}

void cb_pm_delete(CBPM *pm) {
	CBmutex_lock;
	caml_remove_generational_global_root(&(pm->v));
	CBmutex_unlock;
	free(pm);
}

CBPM * cb_pm_stderr_new(void) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_pm_stderr_new");
	v = caml_callback_exn(*closure, Val_unit);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBPM*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBPM*, cb_pm_alloc(v));
}

int cb_pm_stderr_start(CBPM *pm, const char *message) {
	CAMLparam0();
	CAMLlocal2(v,vmessage);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_pm_stderr_start");
	vmessage = caml_copy_string(message);
	v = caml_callback2_exn(*closure, pm->v, vmessage);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (int, 1);
	}
	CBmutex_unlock;
	CAMLreturnT (int, 1);;
}

int cb_pm_stderr_report(CBPM *pm, int state, int max) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_pm_stderr_report");
	v = caml_callback3_exn(*closure, pm->v, Val_int(state), Val_int(max));
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (int, 1);
	}
	CBmutex_unlock;
	CAMLreturnT (int, 1);;
}

int cb_pm_stderr_finish(CBPM *pm) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_pm_stderr_finish");
	v = caml_callback_exn(*closure, pm->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (int, 1);
	}
	CBmutex_unlock;
	CAMLreturnT (int, 1);;
}

/* object property expression */

CBOPE * object_property_expression_alloc(value v) {
	CAMLparam1(v);
	CBOPE * wrapper = malloc(sizeof(CBOPE));
	if (wrapper == NULL)
		return NULL;
	wrapper->v = v;
	CBmutex_lock;
	caml_register_generational_global_root(&(wrapper->v));
	CBmutex_unlock;
	CAMLreturnT (CBOPE*, wrapper);
}

void cb_object_property_expression_delete(CBOPE * ope) {
	CBmutex_lock;
	caml_remove_generational_global_root(&(ope->v));
	CBmutex_unlock;
	free(ope);
}

char * cb_object_property_get_iri (const CBOPE *ope) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_object_property_get_iri");
	v = caml_callback_exn(*closure, ope->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (char*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (char*, String_val(v));;
}

char * cb_object_inverse_of_get_iri (const CBOPE *ope) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_object_inverse_of_get_iri");
	v = caml_callback_exn(*closure, ope->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (char*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (char*, String_val(v));;
}

int cb_object_property_expression_print(const CBOPE *ope) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_object_property_expression_print");
	v = caml_callback_exn(*closure, ope->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (int, 0);
	}
	CBmutex_unlock;
	CAMLreturnT (int, 1);;
}

CBOPE * cb_top_object_property_get() {
	CAMLparam0();
	CAMLlocal2(v, viri);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_top_object_property_get");
	v = caml_callback_exn(*closure, Val_unit);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBOPE*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBOPE*, object_property_expression_alloc(v));
}

CBOPE * cb_bottom_object_property_get() {
	CAMLparam0();
	CAMLlocal2(v, viri);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_bottom_object_property_get");
	v = caml_callback_exn(*closure, Val_unit);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBOPE*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBOPE*, object_property_expression_alloc(v));
}

CBOPE * cb_object_property_new(const char *iri) {
	CAMLparam0();
	CAMLlocal2(v, viri);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_object_property_get");
	viri = caml_copy_string(iri);
	v = caml_callback_exn(*closure, viri);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBOPE*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBOPE*, object_property_expression_alloc(v));
}

CBOPE * cb_object_inverse_of_new(const char *iri) {
	CAMLparam0();
	CAMLlocal2(v, viri);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_object_inverse_of_get");
	viri = caml_copy_string(iri);
	v = caml_callback_exn(*closure, viri);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBOPE*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBOPE*, object_property_expression_alloc(v));
}

CBOPET cb_object_property_expression_get_case(const CBOPE *ope) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_object_property_expression_get_case");
	v = caml_callback_exn(*closure, ope->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		/* TODO: improve error reporting */
		CAMLreturnT (CBOPET, -1);
	}
	CBmutex_unlock;
	CAMLreturnT (CBOPET, Int_val(v));
}

/* class expression */

CBCE * class_expression_alloc(value v) {
	CAMLparam1(v);
	CBCE * wrapper = malloc(sizeof(CBCE));
	if (wrapper == NULL)
		return NULL;
	wrapper->v = v;
	CBmutex_lock;
	caml_register_generational_global_root(&(wrapper->v));
	CBmutex_unlock;
	CAMLreturnT (CBCE*, wrapper);
}

void cb_class_expression_delete(CBCE * ce) {
	CBmutex_lock;
	caml_remove_generational_global_root(&(ce->v));
	CBmutex_unlock;
	free(ce);
}

char * cb_class_get_iri(const CBCE *ce) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_class_get_iri");
	v = caml_callback_exn(*closure, ce->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (char*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (char*, String_val(v));;
}

int cb_class_expression_print(const CBCE *ce) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_class_expression_print");
	v = caml_callback_exn(*closure, ce->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (int, 0);
	}
	CBmutex_unlock;
	CAMLreturnT (int, 1);;
}

CBCE ** cb_object_intersection_of_get_operands(const CBCE *ce, int *sp) {
	CAMLparam0();
	CAMLlocal2(v, vce);
	int i, len;
	CBCE ** operands;
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_object_intersection_of_get_operands");
	v = caml_callback_exn(*closure, ce->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBCE**, NULL);
	}
	len = Wosize_val(v);
	CBmutex_unlock;
	operands = malloc(len * sizeof(CBCE*));
	for (i = 0; i < len; i++) {
		vce = Field(v, i);
		operands[i] = class_expression_alloc(vce);
	}
	*sp = len;
	CAMLreturnT (CBCE**, operands);
}

CBOPE * cb_object_some_values_from_get_property(const CBCE *ce) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_object_some_values_from_get_property");
	v = caml_callback_exn(*closure, ce->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBOPE*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBOPE*, object_property_expression_alloc(v));
}

CBCE * cb_object_some_values_from_get_filler(const CBCE *ce) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_object_some_values_from_get_filler");
	v = caml_callback_exn(*closure, ce->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBCE*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBCE*, class_expression_alloc(v));
}


CBCE * cb_thing_get() {
	CAMLparam0();
	CAMLlocal2(v, viri);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_thing_get");
	v = caml_callback_exn(*closure, Val_unit);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBCE*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBCE*, class_expression_alloc(v));
}

CBCE * cb_nothing_get() {
	CAMLparam0();
	CAMLlocal2(v, viri);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_nothing_get");
	v = caml_callback_exn(*closure, Val_unit);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBCE*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBCE*, class_expression_alloc(v));
}

CBCE * cb_class_new(const char *iri) {
	CAMLparam0();
	CAMLlocal2(v, viri);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_class_get");
	viri = caml_copy_string(iri);
	v = caml_callback_exn(*closure, viri);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBCE*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBCE*, class_expression_alloc(v));
}

CBCE * cb_object_intersection_of_new(int len, CBCE * const * cearr) {
	CAMLparam0();
	CAMLlocal2(va, v);
	int i;
	static value *closure = NULL;
	static value *closure_rec = NULL;
	if (len >= 2) {
		CBmutex_lock;
		if (closure == NULL)
			closure = caml_named_value("cb_object_intersection_of_get");
		if (closure_rec == NULL)
			closure_rec = caml_named_value("cb_object_intersection_of_rec_get");
		v = cearr[1]->v;
		for (i = 2; i < len; i++) {
			v = caml_callback2_exn(*closure_rec, v, cearr[i]->v);
			if (CB_exception(v)) {
				CBmutex_unlock;
				CAMLreturnT (CBCE*, NULL);
			}
		}
		v = caml_callback2_exn(*closure, cearr[0]->v, v);
		if (CB_exception(v)) {
			CBmutex_unlock;
			CAMLreturnT (CBCE*, NULL);
		}
		CBmutex_unlock;
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
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_object_some_values_from_get");
	v = caml_callback2_exn(*closure, ope->v, ce->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBCE*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBCE*, class_expression_alloc(v));
}

CBCET cb_class_expression_get_case(const CBCE *ce) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_class_expression_get_case");
	v = caml_callback_exn(*closure, ce->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		/* TODO: improve error reporting */
		CAMLreturnT (CBCET, -1);
	}
	CBmutex_unlock;
	CAMLreturnT (CBCET, Int_val(v));
}

/* declarations */

CBDAX * declaration_axiom_alloc(value v) {
	CAMLparam1(v);
	CBDAX * wrapper = malloc(sizeof(CBDAX));
	if (wrapper == NULL)
		return NULL;
	wrapper->v = v;
	CBmutex_lock;
	caml_register_generational_global_root(&(wrapper->v));
	CBmutex_unlock;
	CAMLreturnT (CBDAX*, wrapper);
}

void cb_declaration_axiom_delete(CBDAX * ax) {
	CBmutex_lock;
	caml_remove_generational_global_root(&(ax->v));
	CBmutex_unlock;
	free(ax);
}

int cb_declaration_axiom_print(const CBDAX *ax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_declaration_axiom_print");
	v = caml_callback_exn(*closure, ax->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (int, 0);
	}
	CBmutex_unlock;
	CAMLreturnT (int, 1);;
}

CBDAX * cb_class_declaration_axiom_new(const char *iri) {
	CAMLparam0();
	CAMLlocal2(v, viri);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_class_declaration_axiom_get");
	viri = caml_copy_string(iri);
	v = caml_callback_exn(*closure, viri);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBDAX*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBDAX*, declaration_axiom_alloc(v));
}


CBDAX * cb_object_property_declaration_axiom_new(const char *iri) {
	CAMLparam0();
	CAMLlocal2(v, viri);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_object_property_declaration_axiom_get");
	viri = caml_copy_string(iri);
	v = caml_callback_exn(*closure, viri);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBDAX*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBDAX*, declaration_axiom_alloc(v));
}

CBDAXT cb_declaration_axiom_get_case(const CBDAX *ax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_declaration_axiom_get_case");
	v = caml_callback_exn(*closure, ax->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		/* TODO: improve error reporting */
		CAMLreturnT (CBDAXT, -1);
	}
	CBmutex_unlock;
	CAMLreturnT (CBDAXT, Int_val(v));
}


/* class axiom */

CBCAX * class_axiom_alloc(value v) {
	CAMLparam1(v);
	CBCAX * wrapper = malloc(sizeof(CBCAX));
	if (wrapper == NULL)
		return NULL;
	wrapper->v = v;
	CBmutex_lock;
	caml_register_generational_global_root(&(wrapper->v));
	CBmutex_unlock;
	CAMLreturnT (CBCAX*, wrapper);
}

void cb_class_axiom_delete(CBCAX * ax) {
	CBmutex_lock;
	caml_remove_generational_global_root(&(ax->v));
	CBmutex_unlock;
	free(ax);
}

CBCE * cb_sub_class_of_axiom_get_sub_class(const CBCAX *ax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_sub_class_of_axiom_get_sub_class");
	v = caml_callback_exn(*closure, ax->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBCE*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBCE*, class_expression_alloc(v));
}

CBCE * cb_sub_class_of_axiom_get_super_class(const CBCAX *ax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_sub_class_of_axiom_get_super_class");
	v = caml_callback_exn(*closure, ax->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBCE*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBCE*, class_expression_alloc(v));
}

CBCE ** cb_equivalent_classes_axiom_get_class_expressions(const CBCAX *ax, int *sp) {
	CAMLparam0();
	CAMLlocal2(v, vce);
	int i, len;
	CBCE ** ces;
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_equivalent_classes_axiom_get_class_expressions");
	v = caml_callback_exn(*closure, ax->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBCE**, NULL);
	}
	len = Wosize_val(v);
	CBmutex_unlock;
	ces = malloc(len * sizeof(CBCE*));
	for (i = 0; i < len; i++) {
		vce = Field(v, i);
		ces[i] = class_expression_alloc(vce);
	}
	*sp = len;
	CAMLreturnT (CBCE**, ces);
}

int cb_class_axiom_print(const CBCAX *ax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_class_axiom_print");
	v = caml_callback_exn(*closure, ax->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (int, 0);
	}
	CBmutex_unlock;
	CAMLreturnT (int, 1);;
}

CBCAX * cb_sub_class_of_axiom_new(const CBCE *cea, const CBCE *ceb) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_sub_class_of_axiom_get");
	v = caml_callback2_exn(*closure, cea->v, ceb->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBCAX*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBCAX*, class_axiom_alloc(v));
}

CBCAX * cb_equivalent_classes_axiom_new(int len, CBCE * const *cearr) {
	CAMLparam0();
	CAMLlocal3(v, cli, cons);
	static value *closure = NULL;
	int i;
	if (len > 1) {
		CBmutex_lock;
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
		if (CB_exception(v)) {
			CBmutex_unlock;
			CAMLreturnT (CBCAX*, NULL);
		}
		CBmutex_unlock;
		CAMLreturnT (CBCAX*, class_axiom_alloc(v));
	} else {
		CBregister_exception;
		CAMLreturnT (CBCAX*, NULL);
	}
}

CBCAXT cb_class_axiom_get_case(const CBCAX *ax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_class_axiom_get_case");
	v = caml_callback_exn(*closure, ax->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		/* TODO: improve error reporting */
		CAMLreturnT (CBCAXT, -1);
	}
	CBmutex_unlock;
	CAMLreturnT (CBCAXT, Int_val(v));
}


/* object property axiom */

CBOPAX * object_property_axiom_alloc(value v) {
	CAMLparam1(v);
	CBOPAX * wrapper = malloc(sizeof(CBOPAX));
	if (wrapper == NULL)
		return NULL;
	wrapper->v = v;
	CBmutex_lock;
	caml_register_generational_global_root(&(wrapper->v));
	CBmutex_unlock;
	CAMLreturnT (CBOPAX*, wrapper);
}

void cb_object_property_axiom_delete(CBOPAX * ax) {
	CBmutex_lock;
	caml_remove_generational_global_root(&(ax->v));
	CBmutex_unlock;
	free(ax);
}

CBOPE ** cb_sub_object_property_of_axiom_get_sub_property_chain(const CBOPAX *ax, int *sp) {
	CAMLparam0();
	CAMLlocal2(v, vope);
	int i, len;
	CBOPE ** opes;
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_sub_object_property_of_axiom_get_sub_property_chain");
	v = caml_callback_exn(*closure, ax->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBOPE**, NULL);
	}
	len = Wosize_val(v);
	CBmutex_unlock;
	opes = malloc(len * sizeof(CBOPE*));
	for (i = 0; i < len; i++) {
		vope = Field(v, i);
		opes[i] = object_property_expression_alloc(vope);
	}
	*sp = len;
	CAMLreturnT (CBOPE**, opes);
}

CBOPE * cb_sub_object_property_of_axiom_get_super_property(const CBOPAX *ax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_sub_object_property_of_axiom_get_super_property");
	v = caml_callback_exn(*closure, ax->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBOPE*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBOPE*, object_property_expression_alloc(v));
}

CBOPE * cb_inverse_object_properties_axiom_get_first_property(const CBOPAX *ax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_inverse_object_properties_axiom_get_first_property");
	v = caml_callback_exn(*closure, ax->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBOPE*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBOPE*, object_property_expression_alloc(v));
}

CBOPE * cb_inverse_object_properties_axiom_get_second_property(const CBOPAX *ax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_inverse_object_properties_axiom_get_second_property");
	v = caml_callback_exn(*closure, ax->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBOPE*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBOPE*, object_property_expression_alloc(v));
}

CBOPE * cb_functional_object_property_axiom_get_property(const CBOPAX *ax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_functional_object_property_axiom_get_property");
	v = caml_callback_exn(*closure, ax->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBOPE*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBOPE*, object_property_expression_alloc(v));
}

CBOPE * cb_inverse_functional_object_property_axiom_get_property(const CBOPAX *ax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_inverse_functional_object_property_axiom_get_property");
	v = caml_callback_exn(*closure, ax->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBOPE*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBOPE*, object_property_expression_alloc(v));
}

CBOPE * cb_transitive_object_property_axiom_get_property(const CBOPAX *ax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_transitive_object_property_axiom_get_property");
	v = caml_callback_exn(*closure, ax->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBOPE*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBOPE*, object_property_expression_alloc(v));
}

int cb_object_property_axiom_print(const CBOPAX *ax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_object_property_axiom_print");
	v = caml_callback_exn(*closure, ax->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (int, 0);
	}
	CBmutex_unlock;
	CAMLreturnT (int, 1);;
}

CBOPAX * cb_sub_object_property_of_axiom_new(int len, CBOPE * const *opearr,
		const CBOPE *opeb) {
	CAMLparam0();
	CAMLlocal3(v, cli, cons);
	static value *closure = NULL;
	int i;
	if (len > 0) {
		CBmutex_lock;
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
		if (CB_exception(v)) {
			CBmutex_unlock;
			CAMLreturnT (CBOPAX*, NULL);
		}
		CBmutex_unlock;
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
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_inverse_object_properties_axiom_get");
	v = caml_callback2_exn(*closure, opea->v, opeb->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBOPAX*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBOPAX*, object_property_axiom_alloc(v));
}

CBOPAX * cb_functional_object_property_axiom_new(const CBOPE *ope) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_functional_object_property_axiom_get");
	v = caml_callback_exn(*closure, ope->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBOPAX*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBOPAX*, object_property_axiom_alloc(v));
}

CBOPAX * cb_inverse_functional_object_property_axiom_new(const CBOPE *ope) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value(
				"cb_inverse_functional_object_property_axiom_get");
	v = caml_callback_exn(*closure, ope->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBOPAX*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBOPAX*, object_property_axiom_alloc(v));
}

CBOPAX * cb_transitive_object_property_axiom_new(const CBOPE *ope) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_transitive_object_property_axiom_get");
	v = caml_callback_exn(*closure, ope->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBOPAX*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBOPAX*, object_property_axiom_alloc(v));
}

CBOPAXT cb_object_property_axiom_get_case(const CBOPAX *ax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_object_property_axiom_get_case");
	v = caml_callback_exn(*closure, ax->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		/* TODO: improve error reporting */
		CAMLreturnT (CBOPAXT, -1);
	}
	CBmutex_unlock;
	CAMLreturnT (CBOPAXT, Int_val(v));
}


/* ontology */

CBONT * ontology_alloc(value v) {
	CAMLparam1(v);
	CBONT * wrapper = malloc(sizeof(CBONT));
	if (wrapper == NULL)
		return NULL;
	wrapper->v = v;
	CBmutex_lock;
	caml_register_generational_global_root(&(wrapper->v));
	CBmutex_unlock;
	CAMLreturnT (CBONT*, wrapper);
}

void cb_ontology_delete(CBONT *ont) {
	CBmutex_lock;
	caml_remove_generational_global_root(&(ont->v));
	CBmutex_unlock;
	free(ont);
}

CBONT * cb_ontology_new(void) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_ontology_new");
	v = caml_callback_exn(*closure, Val_unit);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBONT*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBONT*, ontology_alloc(v));
}

int cb_ontology_add_declaration_axiom(CBONT *ont, const CBDAX *dax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_ontology_add_declaration_axiom");
	v = caml_callback2_exn(*closure, ont->v, dax->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (int, 1);
	}
	CBmutex_unlock;
	CAMLreturnT (int, 1);;
}

int cb_ontology_remove_declaration_axiom(CBONT *ont, const CBDAX *dax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_ontology_remove_declaration_axiom");
	v = caml_callback2_exn(*closure, ont->v, dax->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (int, 1);
	}
	CBmutex_unlock;
	CAMLreturnT (int, 1);;
}

int cb_ontology_add_class_axiom(CBONT *ont, const CBCAX *cax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_ontology_add_class_axiom");
	v = caml_callback2_exn(*closure, ont->v, cax->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (int, 1);
	}
	CBmutex_unlock;
	CAMLreturnT (int, 1);;
}

int cb_ontology_remove_class_axiom(CBONT *ont, const CBCAX *cax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_ontology_remove_class_axiom");
	v = caml_callback2_exn(*closure, ont->v, cax->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (int, 1);
	}
	CBmutex_unlock;
	CAMLreturnT (int, 1);;
}

int cb_ontology_add_object_property_axiom(CBONT *ont, const CBOPAX *opax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_ontology_add_object_property_axiom");
	v = caml_callback2_exn(*closure, ont->v, opax->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (int, 0);
	}
	CBmutex_unlock;
	CAMLreturnT (int, 1);;
}

int cb_ontology_remove_object_property_axiom(CBONT *ont, const CBOPAX *opax) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_ontology_remove_object_property_axiom");
	v = caml_callback2_exn(*closure, ont->v, opax->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (int, 0);
	}
	CBmutex_unlock;
	CAMLreturnT (int, 1);;
}

int cb_ontology_print_info(const CBONT *ont) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_ontology_print_info");
	v = caml_callback_exn(*closure, ont->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (int, 0);
	}
	CBmutex_unlock;
	CAMLreturnT (int, 1);;
}

int cb_ontology_classify(CBONT *ont) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_ontology_classify");
	v = caml_callback_exn(*closure, ont->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (int, 0);
	}
	CBmutex_unlock;
	CAMLreturnT (int, 1);
}

int cb_ontology_classify_pm(CBONT *ont, PM *pm) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_ontology_classify_pm");
	v = caml_callback2_exn(*closure, ont->v, (value) pm);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (int, 0);
	}
	CBmutex_unlock;
	CAMLreturnT (int, 1);
}

/* class axiom iterator */

CBCAXI * class_axiom_iterator_alloc(value v) {
	CAMLparam1(v);
	CBCAXI * wrapper = malloc(sizeof(CBCAXI));
	if (wrapper == NULL)
		return NULL;
	wrapper->v = v;
	CBmutex_lock;
	caml_register_generational_global_root(&(wrapper->v));
	CBmutex_unlock;
	CAMLreturnT (CBCAXI*, wrapper);
}

void cb_class_axiom_iterator_delete(CBCAXI *itr) {
	CBmutex_lock;
	caml_remove_generational_global_root(&(itr->v));
	CBmutex_unlock;
	free(itr);
}

CBCAXI * cb_class_axiom_iterator_new(const CBONT *ont) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_class_axiom_iterator_new");
	v = caml_callback_exn(*closure, ont->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBCAXI*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBCAXI*, class_axiom_iterator_alloc(v));
}


int cb_class_axiom_iterator_has_next(const CBCAXI *itr) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_class_axiom_iterator_has_next");
	v = caml_callback_exn(*closure, itr->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (int, -1);
	}
	CBmutex_unlock;
	CAMLreturnT (int, Bool_val(v));
}

CBCAX * cb_class_axiom_iterator_next(const CBCAXI *itr) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_class_axiom_iterator_next");
	v = caml_callback_exn(*closure, itr->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBCAX*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBCAX*, class_axiom_alloc(v));
}

/* class nodes */

CBCN * class_node_alloc(value v) {
	CAMLparam1(v);
	CBCN * wrapper = malloc(sizeof(CBCN));
	if (wrapper == NULL)
		return NULL;
	wrapper->v = v;
	CBmutex_lock;
	caml_register_generational_global_root(&(wrapper->v));
	CBmutex_unlock;
	CAMLreturnT (CBCN*, wrapper);
}

void cb_class_node_delete(CBCN *cn) {
	CBmutex_lock;
	caml_remove_generational_global_root(&(cn->v));
	CBmutex_unlock;
	free(cn);
}

CBCN * cb_class_node_get(const CBONT *ont, const CBCE *ce) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_class_node_get");
	v = caml_callback2_exn(*closure, ont->v, ce->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBCN*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBCN*, class_node_alloc(v));
}

CBCN * cb_class_node_top_get(const CBONT *ont) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_class_node_top_get");
	v = caml_callback_exn(*closure, ont->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBCN*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBCN*, class_node_alloc(v));
}

CBCN * cb_class_node_bot_get(const CBONT *ont) {
	CAMLparam0();
	CAMLlocal1(v);
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_class_node_bot_get");
	v = caml_callback_exn(*closure, ont->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBCN*, NULL);
	}
	CBmutex_unlock;
	CAMLreturnT (CBCN*, class_node_alloc(v));
}

CBCE ** cb_class_node_get_classes(const CBCN *cn, int *sp) {
	CAMLparam0();
	CAMLlocal2(v, vce);
	int i, len;
	CBCE ** classes;
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_class_node_get_classes");
	v = caml_callback_exn(*closure, cn->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBCE**, NULL);
	}
	len = Wosize_val(v);
	CBmutex_unlock;
	classes = malloc(len * sizeof(CBCE*));
	for (i = 0; i < len; i++) {
		vce = Field(v, i);
		classes[i] = class_expression_alloc(vce);
	}
	*sp = len;
	CAMLreturnT (CBCE**, classes);
}

CBCN ** cb_class_node_get_child_nodes(const CBCN *cn, int *sp) {
	CAMLparam0();
	CAMLlocal2(v, vcn);
	int i, len;
	CBCN * ch_cn;
	CBCN ** nodes;
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_class_node_get_child_nodes");
	v = caml_callback_exn(*closure, cn->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBCN**, NULL);
	}
	CBmutex_unlock;
	len = Wosize_val(v);
	nodes = malloc(len * sizeof(CBCN*));
	for (i = 0; i < len; i++) {
		vcn = Field(v, i);
		ch_cn = class_node_alloc(vcn);
		nodes[i] = ch_cn;
	}
	*sp = len;
	CAMLreturnT (CBCN**, nodes);
}

CBCN ** cb_class_node_get_parent_nodes(const CBCN *cn, int *sp) {
	CAMLparam0();
	CAMLlocal2(v, vcn);
	int i, len;
	CBCN * pt_cn;
	CBCN ** nodes;
	static value *closure = NULL;
	CBmutex_lock;
	if (closure == NULL)
		closure = caml_named_value("cb_class_node_get_parent_nodes");
	v = caml_callback_exn(*closure, cn->v);
	if (CB_exception(v)) {
		CBmutex_unlock;
		CAMLreturnT (CBCN**, NULL);
	}
	CBmutex_unlock;
	len = Wosize_val(v);
	nodes = malloc(len * sizeof(CBCN*));
	for (i = 0; i < len; i++) {
		vcn = Field(v, i);
		pt_cn = class_node_alloc(vcn);
		nodes[i] = pt_cn;
	}
	*sp = len;
	CAMLreturnT (CBCN**, nodes);
}

