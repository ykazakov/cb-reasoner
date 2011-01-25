#include <cb.h>
#include "common.h"
#include "cb_Class.h"
#include "cb_ClassExpression.h"
#include "cb_ObjectIntersectionOf.h"

static jfieldID ce_fid_ptr;
static jclass cls_ce;

/* init */
JNIEXPORT void JNICALL Java_cb_ClassExpression_init
(JNIEnv *env, jclass cls) {
	cls_ce = cls;
	ce_fid_ptr = (*env)->GetFieldID(env, cls, "ptr", "J");
}

/* destruct */
JNIEXPORT void JNICALL Java_cb_ClassExpression_destruct
(JNIEnv *env, jobject self) {
	long ptr = (*env)->GetLongField(env, self, ce_fid_ptr);
	(*env)->SetLongField(env, self, ce_fid_ptr, 0);
	if (ptr != 0) {
		CBCE *ce = (CBCE *)(intptr_t)ptr;
		cb_class_expression_delete(ce);
	}
}

/* constructors */

/* class */
JNIEXPORT void JNICALL Java_cb_Class_create
(JNIEnv *env, jobject self, jstring iri) {
	jboolean icp;
	const char *tiri = (*env)->GetStringUTFChars(env, iri, &icp);
	CBCE *ce = cb_class_new(tiri);
	if (!ce) throw_exception(env);;
	if(icp) (*env)->ReleaseStringUTFChars(env, iri, tiri);
	(*env)->SetLongField(env, self, ce_fid_ptr, (intptr_t)ce);
}

/* object intersection of */
JNIEXPORT void JNICALL Java_cb_ObjectIntersectionOf_create
(JNIEnv *env, jobject self, jobjectArray cearr) {
	jsize len = (*env)->GetArrayLength(env, cearr);
	const CBCE **ceptrs = malloc(len * sizeof(CBCE *));
	int i;
	for (i = 0; i < len; i++) {
		ceptrs[i] = (CBCE *) get_ptr(env,
				(*env)->GetObjectArrayElement(env, cearr, i));
	}
	CBCE *ce = cb_object_intersection_of_new(len, ceptrs);
	if (!ce) throw_exception(env);
	free(ceptrs);
	(*env)->SetLongField(env, self, ce_fid_ptr, (intptr_t) ce);
}

/* object some values from */
JNIEXPORT void JNICALL Java_cb_ObjectSomeValuesFrom_create
(JNIEnv *env, jobject self, jobject ope, jobject cea) {
	CBOPE *opeptr = (CBOPE *) get_ptr(env, ope);
	CBCE *ceaptr = (CBCE *) get_ptr(env, cea);
	CBCE *ce = cb_object_some_values_from_new(opeptr, ceaptr);
	if (!ce) throw_exception(env);
	(*env)->SetLongField(env, self, ce_fid_ptr, (intptr_t) ce);
}

/* print */
JNIEXPORT void JNICALL Java_cb_ClassExpression_print
(JNIEnv *env, jobject self) {
	CBCE *ce = (CBCE *)(intptr_t)(*env)->GetLongField(env, self, ce_fid_ptr);
	if (!cb_class_expression_print(ce)) throw_exception(env);
}
