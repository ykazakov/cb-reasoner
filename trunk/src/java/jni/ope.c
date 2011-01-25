#include <cb.h>
#include "common.h"
#include "cb_ObjectPropertyExpression.h"
#include "cb_ObjectInverseOf.h"

static jclass cls_ope;
static jfieldID ope_fid_ptr;

/* init */
JNIEXPORT void JNICALL Java_cb_ObjectPropertyExpression_init
(JNIEnv *env, jclass cls) {
	cls_ope = cls;
	ope_fid_ptr = (*env)->GetFieldID(env, cls, "ptr", "J");
}

/* destruct */
JNIEXPORT void JNICALL Java_cb_ObjectPropertyExpression_destruct
(JNIEnv *env, jobject self) {
	long ptr = (*env)->GetLongField(env, self, ope_fid_ptr);
	(*env)->SetLongField(env, self, ope_fid_ptr, 0);
	if (ptr != 0) {
		CBOPE *ope = (CBOPE *)(intptr_t)ptr;
		cb_object_property_expression_delete(ope);
	}
}

/* constructors */

/* object property */
JNIEXPORT void JNICALL Java_cb_ObjectProperty_create
(JNIEnv *env, jobject self, jstring iri) {
	jboolean icp;
	const char *tiri = (*env)->GetStringUTFChars(env, iri, &icp);
	CBOPE *ope = cb_object_property_new(tiri);
	if (!ope) throw_exception(env);
	if(icp) (*env)->ReleaseStringUTFChars(env, iri, tiri);
	(*env)->SetLongField(env, self, ope_fid_ptr, (intptr_t)ope);
}

/* object-inverse-of property */
JNIEXPORT void JNICALL Java_cb_ObjectInverseOf_create
(JNIEnv *env, jobject self, jstring iri) {
	jboolean icp;
	const char *tiri = (*env)->GetStringUTFChars(env, iri, &icp);
	CBOPE *ope = cb_object_inverse_of_new(tiri);
	if (!ope) throw_exception(env);
	if(icp) (*env)->ReleaseStringUTFChars(env, iri, tiri);
	(*env)->SetLongField(env, self, ope_fid_ptr, (intptr_t)ope);
}

/* print */
JNIEXPORT void JNICALL Java_cb_ObjectPropertyExpression_print
(JNIEnv *env, jobject self) {
	CBOPE *ope = (CBOPE *)(intptr_t)(*env)->GetLongField(env, self, ope_fid_ptr);
	if (!cb_object_property_expression_print(ope)) throw_exception(env);
}
