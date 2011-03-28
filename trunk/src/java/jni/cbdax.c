#include <cb.h>
#include "config.h"
#include "org_semanticweb_cb_reasoner_CBDeclarationAxiom.h"
#include "org_semanticweb_cb_reasoner_CBClassDeclarationAxiom.h"
#include "org_semanticweb_cb_reasoner_CBObjectPropertyDeclarationAxiom.h"

/* destruct */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBDeclarationAxiom_destruct
(JNIEnv *env, jobject self) {
	long ptr = get_ptr(env, self);
	set_ptr(env, self, 0);
	if (ptr != 0) {
		CBCAX *ax = (CBCAX *)(intptr_t)ptr;
		cb_class_axiom_delete(ax);
	}
}

/* constructors */

/* class declaration axiom */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBClassDeclarationAxiom_create
(JNIEnv *env, jobject self, jstring iri) {
	jboolean icp;
	const char *tiri = (*env)->GetStringUTFChars(env, iri, &icp);
	CBDAX *ax = cb_class_declaration_axiom_new(tiri);
	if (!ax)
		CBthrow_exception(env);
	if(icp) (*env)->ReleaseStringUTFChars(env, iri, tiri);
	set_ptr(env, self, (intptr_t)ax);
}

/* object property declaration axiom */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBObjectPropertyDeclarationAxiom_create
(JNIEnv *env, jobject self, jstring iri) {
	jboolean icp;
	const char *tiri = (*env)->GetStringUTFChars(env, iri, &icp);
	CBDAX *ax = cb_object_property_declaration_axiom_new(tiri);
	if (!ax)
		CBthrow_exception(env);
	if(icp) (*env)->ReleaseStringUTFChars(env, iri, tiri);
	set_ptr(env, self, (intptr_t)ax);
}

/* methods */

/* create a new object for the subclass of CBDeclarationAxiom that corresponds to
 * the constructor type of the given declaration axiom */
jobject declaration_axiom_new(JNIEnv *env, CBDAX *ax) {
	jclass cls;
	char *cls_str;
	switch (cb_declaration_axiom_get_case(ax)) {
	case CB_ClassDeclaration:
		cls_str = CLSCBClassDeclarationAxiom;
		break;
	case CB_ObjectPropertyDeclaration:
		cls_str = CLSCBObjectPropertyDeclarationAxiom;
		break;
	default:
		CBthrow_exception(env)
		;
		return NULL;
	}
	cls = (*env)->FindClass(env, cls_str);
	return new_obj(env, cls, (intptr_t) ax);
}

/* print */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBDeclarationAxiom_print
(JNIEnv *env, jobject self) {
	CBDAX *ax = (CBDAX *) get_ptr(env, self);
	if (!cb_declaration_axiom_print(ax))
		CBthrow_exception(env);
}
