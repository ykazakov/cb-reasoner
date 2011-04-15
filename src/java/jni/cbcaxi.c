#include <cb.h>
#include "config.h"
#include "org_semanticweb_cb_reasoner_CbClassAxiomIterator.h"

/* constructors */

JNIEXPORT jlong JNICALL Java_org_semanticweb_cb_reasoner_CbClassAxiomIterator_getPtr
  (JNIEnv *env, jobject self, jobject ont) {
	CBONT *ontptr = (CBONT *) get_ptr(env, ont);
	CBCAXI *itrptr = cb_class_axiom_iterator_new(ontptr);
	if (!itrptr)
		CBthrow_exception(env);
	return (intptr_t)itrptr;
}

/* destruct */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CbClassAxiomIterator_destruct
(JNIEnv *env, jobject self) {
	long ptr = get_ptr(env, self);
	//set_ptr(env, self, 0);
	if (ptr != 0) {
		CBCAXI *itr = (CBCAXI *) (intptr_t) ptr;
		cb_class_axiom_iterator_delete(itr);
	}
}

/* methods */
JNIEXPORT jboolean JNICALL Java_org_semanticweb_cb_reasoner_CbClassAxiomIterator_hasNext
(JNIEnv *env, jobject self) {
	CBCAXI *axitr = (CBCAXI *) get_ptr(env, self);
	int res = cb_class_axiom_iterator_has_next(axitr);
	if (res == -1)
		CBthrow_exception(env);
	return res;
}

JNIEXPORT jobject JNICALL Java_org_semanticweb_cb_reasoner_CbClassAxiomIterator_next
(JNIEnv *env, jobject self) {
	CBCAXI *axitr = (CBCAXI *) get_ptr(env, self);
	CBCAX *ax = cb_class_axiom_iterator_next(axitr);
	if (!ax)
		CBthrow_exception(env);
	// TODO: throw a proper exception
	return class_axiom_new(env, ax);
}
