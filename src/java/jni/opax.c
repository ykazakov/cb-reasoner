#include <cb.h>
#include "common.h"
#include "cb_ObjectPropertyAxiom.h"
#include "cb_SubObjectPropertyOfAxiom.h"
#include "cb_InverseObjectPropertiesAxiom.h"
#include "cb_FunctionalObjectPropertyAxiom.h"
#include "cb_InverseFunctionalObjectPropertyAxiom.h"
#include "cb_TransitiveObjectPropertyAxiom.h"

static jclass cls_ax;
static jfieldID ax_fid_ptr;

/* init */
JNIEXPORT void JNICALL Java_cb_ObjectPropertyAxiom_init
(JNIEnv *env, jclass cls) {
	cls_ax = cls;
	ax_fid_ptr = (*env)->GetFieldID(env, cls, "ptr", "J");
}

/* destruct */
JNIEXPORT void JNICALL Java_cb_ObjectPropertyAxiom_destruct
(JNIEnv *env, jobject self) {
	long ptr = (*env)->GetLongField(env, self, ax_fid_ptr);
	(*env)->SetLongField(env, self, ax_fid_ptr, 0);
	if (ptr != 0) {
		CBOPAX *ax = (CBOPAX *)(intptr_t)ptr;
		cb_object_property_axiom_delete(ax);
	}
}

/* constructors */

/* sub-object-property-of axiom */
JNIEXPORT void JNICALL Java_cb_SubObjectPropertyOfAxiom_create
(JNIEnv *env, jobject self, jobjectArray opearr, jobject opeb) {
	jsize len = (*env)->GetArrayLength(env, opearr);
	const CBOPE **opeptrs = malloc(len * sizeof(CBOPE *));
	int i;
	for (i = 0; i < len; i++) {
		opeptrs[i] = (CBOPE *) get_ptr(env,
				(*env)->GetObjectArrayElement(env, opearr, i));
	}
	CBOPE *opebptr = (CBOPE *)get_ptr(env, opeb);
	CBOPAX *ax = cb_sub_object_property_of_axiom_new(len, opeptrs, opebptr);
	if (!ax) throw_exception(env);
	(*env)->SetLongField(env, self, ax_fid_ptr, (intptr_t)ax);
}

/* inverse-object-properties axiom */
JNIEXPORT void JNICALL Java_cb_InverseObjectPropertiesAxiom_create
(JNIEnv *env, jobject self, jobject opea, jobject opeb) {
	CBOPE *opeaptr = (CBOPE *)get_ptr(env, opea);
	CBOPE *opebptr = (CBOPE *)get_ptr(env, opeb);
	CBOPAX *ax = cb_inverse_object_properties_axiom_new(opeaptr, opebptr);
	if (!ax) throw_exception(env);
	(*env)->SetLongField(env, self, ax_fid_ptr, (intptr_t)ax);
}

/* functional-object-property axiom */
JNIEXPORT void JNICALL Java_cb_FunctionalObjectPropertyAxiom_create
(JNIEnv *env, jobject self, jobject ope) {
	CBOPE *opeptr = (CBOPE *)get_ptr(env, ope);
	CBOPAX *ax = cb_functional_object_property_axiom_new(opeptr);
	if (!ax) throw_exception(env);
	(*env)->SetLongField(env, self, ax_fid_ptr, (intptr_t)ax);
}

/* inverse-functional-object-property axiom */
JNIEXPORT void JNICALL Java_cb_InverseFunctionalObjectPropertyAxiom_create
(JNIEnv *env, jobject self, jobject ope) {
	CBOPE *opeptr = (CBOPE *)get_ptr(env, ope);
	CBOPAX *ax = cb_inverse_functional_object_property_axiom_new(opeptr);
	if (!ax) throw_exception(env);
	(*env)->SetLongField(env, self, ax_fid_ptr, (intptr_t)ax);
}

/* transitive-object-property axiom */
JNIEXPORT void JNICALL Java_cb_TransitiveObjectPropertyAxiom_create
(JNIEnv *env, jobject self, jobject ope) {
	CBOPE *opeptr = (CBOPE *)get_ptr(env, ope);
	CBOPAX *ax = cb_transitive_object_property_axiom_new(opeptr);
	if (!ax) throw_exception(env);
	(*env)->SetLongField(env, self, ax_fid_ptr, (intptr_t)ax);
}

/* print */
JNIEXPORT void JNICALL Java_cb_ObjectPropertyAxiom_print
(JNIEnv *env, jobject self) {
	CBOPAX *ax = (CBOPAX *)(intptr_t)(*env)->GetLongField(env, self, ax_fid_ptr);
	if (!cb_object_property_axiom_print(ax)) throw_exception(env);
}
