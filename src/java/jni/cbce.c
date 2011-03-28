#include <cb.h>
#include "config.h"
#include "org_semanticweb_cb_reasoner_CBClass.h"
#include "org_semanticweb_cb_reasoner_CBClassExpression.h"
#include "org_semanticweb_cb_reasoner_CBObjectIntersectionOf.h"
#include "org_semanticweb_cb_reasoner_CBObjectSomeValuesFrom.h"

/* destruct */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBClassExpression_destruct
(JNIEnv *env, jobject self) {
	long ptr = get_ptr(env, self);
	set_ptr(env, self, 0);
	if (ptr != 0) {
		CBCE *ce = (CBCE *)(intptr_t)ptr;
		cb_class_expression_delete(ce);
	}
}

/* constructors */

/* class */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBClass_create
(JNIEnv *env, jobject self, jstring iri) {
	jboolean icp;
	const char *tiri = (*env)->GetStringUTFChars(env, iri, &icp);
	CBCE *ce = cb_class_new(tiri);
	if (!ce)
		CBthrow_exception(env);
	if(icp) (*env)->ReleaseStringUTFChars(env, iri, tiri);
	set_ptr(env, self, (intptr_t)ce);
}

/* object intersection of */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBObjectIntersectionOf_create
(JNIEnv *env, jobject self, jobjectArray cearr) {
	jsize len = (*env)->GetArrayLength(env, cearr);
	CBCE *ce;
	CBCE **ceptrs = malloc(len * sizeof(CBCE *));
	int i;
	for (i = 0; i < len; i++) {
		ceptrs[i] = (CBCE *) get_ptr(env,
				(*env)->GetObjectArrayElement(env, cearr, i));
	}
	ce = cb_object_intersection_of_new(len, ceptrs);
	if (!ce)
		CBthrow_exception(env);
	free(ceptrs);
	set_ptr(env, self, (intptr_t)ce);
}

/* object some values from */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBObjectSomeValuesFrom_create
(JNIEnv *env, jobject self, jobject ope, jobject cea) {
	CBOPE *opeptr = (CBOPE *) get_ptr(env, ope);
	CBCE *ceaptr = (CBCE *) get_ptr(env, cea);
	CBCE *ce = cb_object_some_values_from_new(opeptr, ceaptr);
	if (!ce)
		CBthrow_exception(env);
	set_ptr(env, self, (intptr_t)ce);
}

/* methods */

/* create a new object for the subclass of CBClassExpression that corresponds to
 * the constructor type of the given class expression */
jobject class_expression_new(JNIEnv *env, CBCE *ce) {
	jclass cls;
	char *cls_str;
	switch (cb_class_expression_get_case(ce)) {
	case CB_Class:
		cls_str = CLSCBClass;
		break;
	case CB_ObjectIntersectionOf:
		cls_str = CLSCBObjectIntersectionOf;
		break;
	case CB_ObjectSomeValuesFrom:
		cls_str = CLSCBObjectSomeValuesFrom;
		break;
	default:
		CBthrow_exception(env)
		;
		return NULL;
	}
	cls = (*env)->FindClass(env, cls_str);
	return new_obj(env, cls, (intptr_t) ce);
}

JNIEXPORT jstring JNICALL Java_org_semanticweb_cb_reasoner_CBClass_getIRI(JNIEnv *env,
		jobject self) {
	CBCE *ce = (CBCE *) get_ptr(env, self);
	char *iri = cb_class_get_iri(ce);
	if (!iri)
		CBthrow_exception(env);
	return (*env)->NewStringUTF(env, iri);
}

/* object intersection of */
JNIEXPORT jobjectArray JNICALL Java_org_semanticweb_cb_reasoner_CBObjectIntersectionOf_getOperands(
		JNIEnv *env, jobject self) {
	int i, len;
	jobjectArray res;
	jclass cls = (*env)->FindClass(env, CLSCBClassExpression);
	CBCE *ce;
	CBCE *ceptr = (CBCE *) get_ptr(env, self);
	CBCE **operands = cb_object_intersection_of_get_operands(ceptr, &len);
	if (!operands)
		CBthrow_exception(env);
	res = (jobjectArray) (*env)->NewObjectArray(env, len, cls, NULL);
	for (i = 0; i < len; i++) {
		ce = operands[i];
		(*env)->SetObjectArrayElement(env, res, i,
				class_expression_new(env, ce));
	}
	free(operands);
	return (res);
}

/* object some values from */
JNIEXPORT jobject JNICALL Java_org_semanticweb_cb_reasoner_CBObjectSomeValuesFrom_getProperty
 (JNIEnv *env, jobject self) {
	CBCE *ceptr = (CBCE *) get_ptr(env, self);
	CBOPE *ope = cb_object_some_values_from_get_property(ceptr);
	if (!ope)
		CBthrow_exception(env);
	return object_property_expression_new(env, ope);
}

JNIEXPORT jobject JNICALL Java_org_semanticweb_cb_reasoner_CBObjectSomeValuesFrom_getFiller
(JNIEnv *env, jobject self) {
	CBCE *ceptr = (CBCE *) get_ptr(env, self);
	CBCE *cef = cb_object_some_values_from_get_filler(ceptr);
	if (!cef)
		CBthrow_exception(env);
	return class_expression_new(env, cef);
}


/* print */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBClassExpression_print
(JNIEnv *env, jobject self) {
	CBCE *ce = (CBCE *) get_ptr(env, self);
	if (!cb_class_expression_print(ce))
		CBthrow_exception(env);
}
