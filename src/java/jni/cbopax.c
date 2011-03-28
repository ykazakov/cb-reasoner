#include <cb.h>
#include "config.h"
#include "org_semanticweb_cb_reasoner_CBObjectPropertyAxiom.h"
#include "org_semanticweb_cb_reasoner_CBSubObjectPropertyOfAxiom.h"
#include "org_semanticweb_cb_reasoner_CBInverseObjectPropertiesAxiom.h"
#include "org_semanticweb_cb_reasoner_CBFunctionalObjectPropertyAxiom.h"
#include "org_semanticweb_cb_reasoner_CBInverseFunctionalObjectPropertyAxiom.h"
#include "org_semanticweb_cb_reasoner_CBTransitiveObjectPropertyAxiom.h"

/* destruct */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBObjectPropertyAxiom_destruct
(JNIEnv *env, jobject self) {
	long ptr = get_ptr(env, self);
	set_ptr(env, self, 0);
	if (ptr != 0) {
		CBOPAX *ax = (CBOPAX *)(intptr_t)ptr;
		cb_object_property_axiom_delete(ax);
	}
}

/* constructors */

/* sub-object-property-of axiom */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBSubObjectPropertyOfAxiom_create
(JNIEnv *env, jobject self, jobjectArray opearr, jobject opeb) {
	jsize len = (*env)->GetArrayLength(env, opearr);
	CBOPE **opeptrs = malloc(len * sizeof(CBOPE *));
	CBOPE *opebptr;
	CBOPAX *ax;
	int i;
	for (i = 0; i < len; i++) {
		opeptrs[i] = (CBOPE *) get_ptr(env,
				(*env)->GetObjectArrayElement(env, opearr, i));
	}
	opebptr = (CBOPE *)get_ptr(env, opeb);
	ax = cb_sub_object_property_of_axiom_new(len, opeptrs, opebptr);
	if (!ax)
		CBthrow_exception(env);
	set_ptr(env, self, (intptr_t)ax);
}

/* inverse-object-properties axiom */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBInverseObjectPropertiesAxiom_create
(JNIEnv *env, jobject self, jobject opea, jobject opeb) {
	CBOPE *opeaptr = (CBOPE *)get_ptr(env, opea);
	CBOPE *opebptr = (CBOPE *)get_ptr(env, opeb);
	CBOPAX *ax = cb_inverse_object_properties_axiom_new(opeaptr, opebptr);
	if (!ax)
		CBthrow_exception(env);
	set_ptr(env, self, (intptr_t)ax);
}

/* functional-object-property axiom */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBFunctionalObjectPropertyAxiom_create
(JNIEnv *env, jobject self, jobject ope) {
	CBOPE *opeptr = (CBOPE *)get_ptr(env, ope);
	CBOPAX *ax = cb_functional_object_property_axiom_new(opeptr);
	if (!ax)
		CBthrow_exception(env);
	set_ptr(env, self, (intptr_t)ax);
}

/* inverse-functional-object-property axiom */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBInverseFunctionalObjectPropertyAxiom_create
(JNIEnv *env, jobject self, jobject ope) {
	CBOPE *opeptr = (CBOPE *)get_ptr(env, ope);
	CBOPAX *ax = cb_inverse_functional_object_property_axiom_new(opeptr);
	if (!ax)
		CBthrow_exception(env);
	set_ptr(env, self, (intptr_t)ax);
}

/* transitive-object-property axiom */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBTransitiveObjectPropertyAxiom_create
(JNIEnv *env, jobject self, jobject ope) {
	CBOPE *opeptr = (CBOPE *)get_ptr(env, ope);
	CBOPAX *ax = cb_transitive_object_property_axiom_new(opeptr);
	if (!ax) CBthrow_exception(env);
	set_ptr(env, self, (intptr_t)ax);
}

/* methods */

/* create a new object for the subclass of CBObjectPropertyAxiom that corresponds
 * to the constructor type of the given class axiom */
jobject object_property_axiom_new(JNIEnv *env, CBOPAX *ax) {
	jclass cls;
	char *cls_str;
	switch (cb_object_property_axiom_get_case(ax)) {
	case CB_SubObjectPropertyOf:
		cls_str = CLSCBSubObjectPropertyOfAxiom;
		break;
	case CB_InverseObjectProperties:
		cls_str = CLSCBInverseObjectPropertiesAxiom;
		break;
	case CB_FunctionalObjectProperty:
		cls_str = CLSCBFunctionalObjectPropertyAxiom;
		break;
	case CB_InverseFunctionalObjectProperty:
		cls_str = CLSCBInverseFunctionalObjectPropertyAxiom;
		break;
	case CB_TransitiveObjectProperty:
		cls_str = CLSCBTransitiveObjectPropertyAxiom;
		break;
	default:
		CBthrow_exception(env)
		;
		return NULL;
	}
	cls = (*env)->FindClass(env, cls_str);
	return new_obj(env, cls, (intptr_t) ax);
}

JNIEXPORT jobjectArray JNICALL Java_org_semanticweb_cb_reasoner_CBSubObjectPropertyOfAxiom_getSubPropertyChain
  (JNIEnv *env, jobject self) {
	int i, len;
	jobjectArray res;
	jclass cls = (*env)->FindClass(env, CLSCBObjectPropertyExpression);
	CBOPE *ope;
	CBOPAX *ax = (CBOPAX *) get_ptr(env, self);
	CBOPE **opes = cb_sub_object_property_of_axiom_get_sub_property_chain(ax, &len);
	if (!opes)
		CBthrow_exception(env);
	res = (jobjectArray) (*env)->NewObjectArray(env, len, cls, NULL);
	for (i = 0; i < len; i++) {
		ope = opes[i];
		(*env)->SetObjectArrayElement(env, res, i,
				object_property_expression_new(env, ope));
	}
	free(opes);
	return (res);
}

JNIEXPORT jobject JNICALL Java_org_semanticweb_cb_reasoner_CBSubObjectPropertyOfAxiom_getSuperProperty
(JNIEnv *env, jobject self) {
	CBOPAX *ax = (CBOPAX *) get_ptr(env, self);
	CBOPE *ope = cb_sub_object_property_of_axiom_get_super_property(ax);
	if (!ope)
		CBthrow_exception(env);
	return object_property_expression_new(env, ope);
}

JNIEXPORT jobject JNICALL Java_org_semanticweb_cb_reasoner_CBInverseObjectPropertiesAxiom_getFirstProperty
(JNIEnv *env, jobject self) {
	CBOPAX *ax = (CBOPAX *) get_ptr(env, self);
	CBOPE *ope = cb_inverse_object_properties_axiom_get_first_property(ax);
	if (!ope)
		CBthrow_exception(env);
	return object_property_expression_new(env, ope);
}

JNIEXPORT jobject JNICALL Java_org_semanticweb_cb_reasoner_CBInverseObjectPropertiesAxiom_getSecondProperty
(JNIEnv *env, jobject self) {
	CBOPAX *ax = (CBOPAX *) get_ptr(env, self);
	CBOPE *ope = cb_inverse_object_properties_axiom_get_second_property(ax);
	if (!ope)
		CBthrow_exception(env);
	return object_property_expression_new(env, ope);
}

JNIEXPORT jobject JNICALL Java_org_semanticweb_cb_reasoner_CBFunctionalObjectPropertyAxiom_getProperty
(JNIEnv *env, jobject self) {
	CBOPAX *ax = (CBOPAX *) get_ptr(env, self);
	CBOPE *ope = cb_functional_object_property_axiom_get_property(ax);
	if (!ope)
		CBthrow_exception(env);
	return object_property_expression_new(env, ope);
}

JNIEXPORT jobject JNICALL Java_org_semanticweb_cb_reasoner_CBInverseFunctionalObjectPropertyAxiom_getProperty
(JNIEnv *env, jobject self) {
	CBOPAX *ax = (CBOPAX *) get_ptr(env, self);
	CBOPE *ope = cb_inverse_functional_object_property_axiom_get_property(ax);
	if (!ope)
		CBthrow_exception(env);
	return object_property_expression_new(env, ope);
}

JNIEXPORT jobject JNICALL Java_org_semanticweb_cb_reasoner_CBTransitiveObjectPropertyAxiom_getProperty
(JNIEnv *env, jobject self) {
	CBOPAX *ax = (CBOPAX *) get_ptr(env, self);
	CBOPE *ope = cb_transitive_object_property_axiom_get_property(ax);
	if (!ope)
		CBthrow_exception(env);
	return object_property_expression_new(env, ope);
}

/* print */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBObjectPropertyAxiom_print
(JNIEnv *env, jobject self) {
	CBOPAX *ax = (CBOPAX *) get_ptr(env, self);
	if (!cb_object_property_axiom_print(ax))
		CBthrow_exception(env);
}
