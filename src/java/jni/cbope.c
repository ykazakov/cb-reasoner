#include <cb.h>
#include "config.h"
#include "org_semanticweb_cb_reasoner_CBObjectPropertyExpression.h"
#include "org_semanticweb_cb_reasoner_CBObjectProperty.h"
#include "org_semanticweb_cb_reasoner_CBObjectInverseOf.h"

/* destruct */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBObjectPropertyExpression_destruct
(JNIEnv *env, jobject self) {
	long ptr = get_ptr(env, self);
	set_ptr(env, self, 0);
	if (ptr != 0) {
		CBOPE *ope = (CBOPE *)(intptr_t)ptr;
		cb_object_property_expression_delete(ope);
	}
}

/* constructors */

/* object property */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBObjectProperty_create
(JNIEnv *env, jobject self, jstring iri) {
	jboolean icp;
	const char *tiri = (*env)->GetStringUTFChars(env, iri, &icp);
	CBOPE *ope = cb_object_property_new(tiri);
	if (!ope)
		CBthrow_exception(env);
	if(icp) (*env)->ReleaseStringUTFChars(env, iri, tiri);
	set_ptr(env, self, (intptr_t)ope);
}

/* object-inverse-of property */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBObjectInverseOf_create
(JNIEnv *env, jobject self, jstring iri) {
	jboolean icp;
	const char *tiri = (*env)->GetStringUTFChars(env, iri, &icp);
	CBOPE *ope = cb_object_inverse_of_new(tiri);
	if (!ope)
		CBthrow_exception(env);
	if(icp) (*env)->ReleaseStringUTFChars(env, iri, tiri);
	set_ptr(env, self, (intptr_t)ope);
}

/* methods */

/* create a new object for the subclass of CBObjectPropertyExpression that
 *  corresponds to the constructor type of the given object property expression */
jobject object_property_expression_new(JNIEnv *env, CBOPE *ope) {
	jclass cls;
	char *cls_str;
	switch (cb_object_property_expression_get_case(ope)) {
	case CB_ObjectProperty:
		cls_str = CLSCBObjectProperty;
		break;
	case CB_ObjectInverseOf:
		cls_str = CLSCBObjectInverseOf;
		break;
	default:
		CBthrow_exception(env)
		;
		return NULL;
	}
	cls = (*env)->FindClass(env, cls_str);
	return new_obj(env, cls, (intptr_t) ope);
}

JNIEXPORT jstring JNICALL Java_org_semanticweb_cb_reasoner_CBObjectProperty_getIRI
  (JNIEnv *env, jobject self) {
	CBOPE *ope = (CBOPE *) get_ptr(env, self);
	char *iri = cb_object_property_get_iri(ope);
	if (!iri)
		CBthrow_exception(env);
	return (*env)->NewStringUTF(env, iri);
}

JNIEXPORT jstring JNICALL Java_org_semanticweb_cb_reasoner_CBObjectInverseOf_getIRI
  (JNIEnv *env, jobject self) {
	CBOPE *ope = (CBOPE *) get_ptr(env, self);
	char *iri = cb_object_inverse_of_get_iri(ope);
	if (!iri)
		CBthrow_exception(env);
	return (*env)->NewStringUTF(env, iri);
}

/* print */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBObjectPropertyExpression_print
(JNIEnv *env, jobject self) {
	CBOPE *ope = (CBOPE *) get_ptr(env, self);
	if (!cb_object_property_expression_print(ope))
		CBthrow_exception(env);
}
