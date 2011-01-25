#include <cb.h>
#include "common.h"
#include "cb_Ontology.h"

static jclass cls_ont;
static jfieldID ont_fid_ptr;

/* init */
JNIEXPORT void JNICALL Java_cb_Ontology_init(JNIEnv *env, jclass cls) {
	cls_ont = cls;
	ont_fid_ptr = (*env)->GetFieldID(env, cls, "ptr", "J");
}

/* destruct */
JNIEXPORT void JNICALL Java_cb_Ontology_destruct(JNIEnv *env, jobject self) {
	long ptr = (*env)->GetLongField(env, self, ont_fid_ptr);
	(*env)->SetLongField(env, self, ont_fid_ptr, 0);
	if (ptr != 0) {
		CBONT *ont = (CBONT *) (intptr_t) ptr;
		cb_ontology_delete(ont);
	}
}

/* constructors */
JNIEXPORT void JNICALL Java_cb_Ontology_create(JNIEnv *env, jobject self) {
	CBONT *ont = cb_ontology_new();
	if (!ont)
		throw_exception(env);
	(*env)->SetLongField(env, self, ont_fid_ptr, (intptr_t) ont);
}

/* adding axioms */
JNIEXPORT void JNICALL Java_cb_Ontology_addcax(JNIEnv *env, jobject self,
		jobject cax) {
	CBONT *ont = (CBONT *) (intptr_t) (*env)->GetLongField(env, self,
			ont_fid_ptr);
	CBCAX *caxptr = (CBCAX *) get_ptr(env, cax);
	if (!cb_ontology_add_class_axiom(ont, caxptr))
		throw_exception(env);
	;
}

JNIEXPORT void JNICALL Java_cb_Ontology_addopax(JNIEnv *env, jobject self,
		jobject opax) {
	CBONT *ont = (CBONT *) (intptr_t) (*env)->GetLongField(env, self,
			ont_fid_ptr);
	CBOPAX *opaxptr = (CBOPAX *) get_ptr(env, opax);
	if (!cb_ontology_add_object_property_axiom(ont, opaxptr))
		throw_exception(env);
}

/* print statistics */
JNIEXPORT void JNICALL Java_cb_Ontology_printInfo(JNIEnv *env, jobject self) {
	CBONT *ont = (CBONT *) (intptr_t) (*env)->GetLongField(env, self,
			ont_fid_ptr);
	if (!cb_ontology_print_info(ont))
		throw_exception(env);
}

JNIEXPORT void JNICALL Java_cb_Ontology_classify(JNIEnv *env, jobject self) {
	CBONT *ont = (CBONT *) (intptr_t) (*env)->GetLongField(env, self,
			ont_fid_ptr);
	if (!cb_ontology_classify(ont))
		throw_exception(env);
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

JNIEXPORT void JNICALL Java_cb_Ontology_classifyPm(JNIEnv *env, jobject self,
		jobject jpmobj) {
	JPM jpm = create_jpm(env, jpmobj);
	PM pm;
	pm.pm = &jpm;
	pm.start = call_pm_start;
	pm.report = call_pm_report;
	pm.finish = call_pm_finish;
	CBONT *ont = (CBONT *) (intptr_t) (*env)->GetLongField(env, self,
			ont_fid_ptr);
	if (!cb_ontology_classify_pm(ont, &pm))
		throw_exception(env);
}

/* Querying taxonomy */

//JNIEXPORT jobjectArray JNICALL Java_cb_Ontology_getEquivalentClasses(
//		JNIEnv *env, jobject self, jobject ceobj) {
//	int i;
//	static jclass cls_c;
//	if (cls_c == NULL)
//		cls_c = (*env)->FindClass(env, "cb/Class");
//	static jmethodID mid;
//	if (mid == NULL)
//		mid = (*env)->GetMethodID(env, cls_c, "Class", "(J)V");
//	CBONT *ont = (CBONT *) (intptr_t) (*env)->GetLongField(env, self,
//			ont_fid_ptr);
//	CBCE *ce = (CBCE *) get_ptr(env, ceobj);
//	CBCE **node = cb_ontology_get_equivalent_classes(ont, ce);
//	if (!node)
//		throw_exception(env);
//	int size = sizeof(*node) / sizeof(CBCE);
//	jobjectArray res = (jobjectArray) (*env)->NewObjectArray(env, size + 1,
//			cls_c, NULL);
//	(*env)->SetObjectArrayElement(env, res, 0, (*env)->NewObject(env, cls_c,
//			mid, (long) ce));
//	for (i = 0; i < size; i++)
//		(*env)->SetObjectArrayElement(env, res, i + 1, (*env)->NewObject(env,
//				cls_c, mid, (long) node[i]));
//	return res;
//}
