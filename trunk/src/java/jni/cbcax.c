#include <cb.h>
#include "config.h"
#include "org_semanticweb_cb_reasoner_CbClassAxiom.h"
#include "org_semanticweb_cb_reasoner_CbSubClassOfAxiom.h"
#include "org_semanticweb_cb_reasoner_CbEquivalentClassesAxiom.h"

/* destruct */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CbClassAxiom_destruct
(JNIEnv *env, jobject self) {
	long ptr = get_ptr(env, self);
	//set_ptr(env, self, 0);
	if (ptr != 0) {
		CBCAX *ax = (CBCAX *)(intptr_t)ptr;
		cb_class_axiom_delete(ax);
	}
}

/* constructors */

/* sub-class-of axiom */
JNIEXPORT jlong JNICALL Java_org_semanticweb_cb_reasoner_CbSubClassOfAxiom_getPtr
(JNIEnv *env, jobject self, jobject cea, jobject ceb) {
	CBCE *ceaptr = (CBCE *)get_ptr(env, cea);
	CBCE *cebptr = (CBCE *)get_ptr(env, ceb);
	CBCAX *ax = cb_sub_class_of_axiom_new(ceaptr, cebptr);
	if (!ax)
		CBthrow_exception(env);
	return (intptr_t)ax;
}

/* equivalent classes axiom */
JNIEXPORT jlong JNICALL Java_org_semanticweb_cb_reasoner_CbEquivalentClassesAxiom_getPtr
(JNIEnv *env, jobject self, jobjectArray cearr) {
	jsize len = (*env)->GetArrayLength(env, cearr);
	CBCAX *ax;
	CBCE **ceptrs = malloc(len * sizeof(CBCE *));
	int i;
	for (i = 0; i < len; i++) {
		ceptrs[i] = (CBCE *) get_ptr(env,
				(*env)->GetObjectArrayElement(env, cearr, i));
	}
	ax = cb_equivalent_classes_axiom_new(len, ceptrs);
	if (!ax)
		CBthrow_exception(env);
	free(ceptrs);
	return (intptr_t)ax;
}

/* methods */

/* create a new object for the subclass of CBClassAxiom that corresponds to
 * the constructor type of the given class axiom */
jobject class_axiom_new(JNIEnv *env, CBCAX *ax) {
	jclass cls;
	char *cls_str;
	switch (cb_class_axiom_get_case(ax)) {
	case CB_SubClassOf:
		cls_str = CLSCBSubClassOfAxiom;
		break;
	case CB_EquivalentClasses:
		cls_str = CLSCBEquivalentClassesAxiom;
		break;
	default:
		CBthrow_exception(env)
		;
		return NULL;
	}
	cls = (*env)->FindClass(env, cls_str);
	return new_obj(env, cls, (intptr_t) ax);
}

JNIEXPORT jobject JNICALL Java_org_semanticweb_cb_reasoner_CbSubClassOfAxiom_getSubClass
  (JNIEnv *env, jobject self) {
	CBCAX *ax = (CBCAX *) get_ptr(env, self);
	CBCE *ce = cb_sub_class_of_axiom_get_sub_class(ax);
	if (!ce)
		CBthrow_exception(env);
	return class_expression_new(env, ce);
}

JNIEXPORT jobject JNICALL Java_org_semanticweb_cb_reasoner_CbSubClassOfAxiom_getSuperClass
  (JNIEnv *env, jobject self) {
	CBCAX *ax = (CBCAX *) get_ptr(env, self);
	CBCE *ce = cb_sub_class_of_axiom_get_super_class(ax);
	if (!ce)
		CBthrow_exception(env);
	return class_expression_new(env, ce);
}

JNIEXPORT jobjectArray JNICALL Java_org_semanticweb_cb_reasoner_CbEquivalentClassesAxiom_getClassExpressions
  (JNIEnv *env, jobject self) {
	int i, len;
	jobjectArray res;
	jclass cls = (*env)->FindClass(env, CLSCBClassExpression);
	CBCE *ce;
	CBCAX *ax = (CBCAX *) get_ptr(env, self);
	CBCE **ces = cb_equivalent_classes_axiom_get_class_expressions(ax, &len);
	if (!ces)
		CBthrow_exception(env);
	res = (jobjectArray) (*env)->NewObjectArray(env, len, cls, NULL);
	for (i = 0; i < len; i++) {
		ce = ces[i];
		(*env)->SetObjectArrayElement(env, res, i,
				class_expression_new(env, ce));
	}
	free(ces);
	return (res);
}

/* print */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CbClassAxiom_print
(JNIEnv *env, jobject self) {
	CBCAX *ax = (CBCAX *) get_ptr(env, self);
	if (!cb_class_axiom_print(ax))
		CBthrow_exception(env);
}
