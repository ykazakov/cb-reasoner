#include <cb.h>
#include "config.h"
#include "org_semanticweb_cb_reasoner_CBOntology.h"

/* destruct */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBOntology_destruct(JNIEnv *env, jobject self) {
	long ptr = get_ptr(env, self);
	set_ptr(env, self, 0);
	if (ptr != 0) {
		CBONT *ont = (CBONT *) (intptr_t) ptr;
		cb_ontology_delete(ont);
	}
}

/* constructors */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBOntology_create(JNIEnv *env, jobject self) {
	CBONT *ont = cb_ontology_new();
	if (!ont)
		CBthrow_exception(env);
	set_ptr(env, self, (intptr_t) ont);
}

/* adding and removing axioms */

JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBOntology_addDeclarationAxiom(JNIEnv *env, jobject self,
		jobject dax) {
	CBONT *ont = (CBONT *) get_ptr(env, self);
	CBDAX *daxptr = (CBDAX *) get_ptr(env, dax);
	if (!cb_ontology_add_declaration_axiom(ont, daxptr))
		CBthrow_exception(env);
	;
}

JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBOntology_removeDeclarationAxiom(JNIEnv *env, jobject self,
		jobject dax) {
	CBONT *ont = (CBONT *) get_ptr(env, self);
	CBDAX *daxptr = (CBDAX *) get_ptr(env, dax);
	if (!cb_ontology_remove_declaration_axiom(ont, daxptr))
		CBthrow_exception(env);
	;
}

JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBOntology_addClassAxiom(JNIEnv *env, jobject self,
		jobject cax) {
	CBONT *ont = (CBONT *) get_ptr(env, self);
	CBCAX *caxptr = (CBCAX *) get_ptr(env, cax);
	if (!cb_ontology_add_class_axiom(ont, caxptr))
		CBthrow_exception(env);
	;
}

JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBOntology_removeClassAxiom(JNIEnv *env, jobject self,
		jobject cax) {
	CBONT *ont = (CBONT *) get_ptr(env, self);
	CBCAX *caxptr = (CBCAX *) get_ptr(env, cax);
	if (!cb_ontology_remove_class_axiom(ont, caxptr))
		CBthrow_exception(env);
	;
}

JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBOntology_addObjectPropertyAxiom(JNIEnv *env, jobject self,
		jobject opax) {
	CBONT *ont = (CBONT *) get_ptr(env, self);
	CBOPAX *opaxptr = (CBOPAX *) get_ptr(env, opax);
	if (!cb_ontology_add_object_property_axiom(ont, opaxptr))
		CBthrow_exception(env);
}

JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBOntology_removeObjectPropertyAxiom(JNIEnv *env, jobject self,
		jobject opax) {
	CBONT *ont = (CBONT *) get_ptr(env, self);
	CBOPAX *opaxptr = (CBOPAX *) get_ptr(env, opax);
	if (!cb_ontology_remove_object_property_axiom(ont, opaxptr))
		CBthrow_exception(env);
}

/* print statistics */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBOntology_printInfo(JNIEnv *env, jobject self) {
	CBONT *ont = (CBONT *) get_ptr(env, self);
	if (!cb_ontology_print_info(ont))
		CBthrow_exception(env);
}

JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBOntology_classify(JNIEnv *env, jobject self) {
	CBONT *ont = (CBONT *) get_ptr(env, self);
	if (!cb_ontology_classify(ont))
		CBthrow_exception(env);
}

/* caching object relevant for java progress monitor */
typedef struct jpm_cache {
	JNIEnv *env;
	jobject obj;
	jmethodID start;
	jmethodID report;
	jmethodID finish;
} JPM;

JPM create_jpm(JNIEnv *env, jobject pm) {
	jobject pm_cls = (*env)->GetObjectClass(env, pm);
	JPM jpm;
	jpm.env = env;
	jpm.obj = pm;
	jpm.start = (*env)->GetMethodID(env, pm_cls, "start",
			"(Ljava/lang/String;)V");
	jpm.report = (*env)->GetMethodID(env, pm_cls, "report", "(II)V");
	jpm.finish = (*env)->GetMethodID(env, pm_cls, "finish", "()V");
	return jpm;
}

void call_pm_start(void * jpm, const char * message) {
	JPM * pm = (JPM*) jpm;
	JNIEnv * env = pm->env;
	jstring jmessage = (*env)->NewStringUTF(env, message);
	(*env)->CallVoidMethod(env, pm->obj, pm->start, jmessage);
}

void call_pm_report(void * jpm, int state, int max) {
	JPM * pm = (JPM*) jpm;
	JNIEnv * env = pm->env;
	(*env)->CallVoidMethod(env, pm->obj, pm->report, (jint) state, (jint) max);
}

void call_pm_finish(void * jpm) {
	JPM * pm = (JPM*) jpm;
	JNIEnv * env = pm->env;
	(*env)->CallVoidMethod(env, pm->obj, pm->finish);
}

JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBOntology_classifyPm(JNIEnv *env, jobject self,
		jobject jpmobj) {
	JPM jpm = create_jpm(env, jpmobj);
	PM pm;
	CBONT *ont;
	pm.pm = &jpm;
	pm.start = call_pm_start;
	pm.report = call_pm_report;
	pm.finish = call_pm_finish;
	ont = (CBONT *) get_ptr(env, self);
	if (!cb_ontology_classify_pm(ont, &pm))
		CBthrow_exception(env);
}

/* retrieving nodes */
JNIEXPORT jobject JNICALL Java_org_semanticweb_cb_reasoner_CBOntology_getClassTaxonomyNode(
		JNIEnv *env, jobject self, jobject ce) {
	jclass cls_cn = (*env)->FindClass(env, CLSCBClassTaxonomyNode);
	CBONT *ont = (CBONT *) get_ptr(env, self);
	CBCE *ceptr = (CBCE *) get_ptr(env, ce);
	CBCN *cn = cb_class_node_get(ont, ceptr);
	if (!cn)
		CBthrow_exception(env);
	return new_obj(env, cls_cn, (intptr_t) cn);
}
