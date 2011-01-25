#include <cb.h>
#include "common.h"
#include "cb_ClassAxiom.h"
#include "cb_SubClassOfAxiom.h"
#include "cb_EquivalentClassesAxiom.h"

static jclass cls_ax;
static jfieldID ax_fid_ptr;

/* init */
JNIEXPORT void JNICALL Java_cb_ClassAxiom_init
(JNIEnv *env, jclass cls) {
	cls_ax = cls;
	ax_fid_ptr = (*env)->GetFieldID(env, cls, "ptr", "J");
}

/* destruct */
JNIEXPORT void JNICALL Java_cb_ClassAxiom_destruct
(JNIEnv *env, jobject self) {
	long ptr = (*env)->GetLongField(env, self, ax_fid_ptr);
	(*env)->SetLongField(env, self, ax_fid_ptr, 0);
	if (ptr != 0) {
		CBCAX *ax = (CBCAX *)(intptr_t)ptr;
		cb_class_axiom_delete(ax);
	}
}

/* constructors */

/* sub-class-of axiom */
JNIEXPORT void JNICALL Java_cb_SubClassOfAxiom_create
(JNIEnv *env, jobject self, jobject cea, jobject ceb) {
	CBCE *ceaptr = (CBCE *)get_ptr(env, cea);
	CBCE *cebptr = (CBCE *)get_ptr(env, ceb);
	CBCAX *ax = cb_sub_class_of_axiom_new(ceaptr, cebptr);
	if (!ax) throw_exception(env);
	(*env)->SetLongField(env, self, ax_fid_ptr, (intptr_t)ax);
}

/* equivalent classes axiom */
JNIEXPORT void JNICALL Java_cb_EquivalentClassesAxiom_create
(JNIEnv *env, jobject self, jobjectArray cearr) {
	jsize len = (*env)->GetArrayLength(env, cearr);
	const CBCE **ceptrs = malloc(len * sizeof(CBCE *));
	int i;
	for (i = 0; i < len; i++) {
		ceptrs[i] = (CBCE *) get_ptr(env,
				(*env)->GetObjectArrayElement(env, cearr, i));
	}
	CBCAX *ax = cb_equivalent_classes_axiom_new(len, ceptrs);
	if (!ax) throw_exception(env);
	free(ceptrs);
	(*env)->SetLongField(env, self, ax_fid_ptr, (intptr_t)ax);
}

/* print */
JNIEXPORT void JNICALL Java_cb_ClassAxiom_print
(JNIEnv *env, jobject self) {
	CBCAX *ax = (CBCAX *)(intptr_t)(*env)->GetLongField(env, self, ax_fid_ptr);
	if (!cb_class_axiom_print(ax)) throw_exception(env);
}
