#include <cb.h>
#include "config.h"
#include "org_semanticweb_cb_reasoner_CbProgressMonitorStdErr.h"

/* progress monitors */

/* destruct */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CbProgressMonitorStdErr_destruct(JNIEnv *env,
		jobject self) {
	long ptr = get_ptr(env, self);
	//set_ptr(env, self, 0);
	if (ptr != 0) {
		CBPM *pm = (CBPM *) (intptr_t) ptr;
		cb_pm_delete(pm);
	}
}

/* constructors */
JNIEXPORT jlong JNICALL Java_org_semanticweb_cb_reasoner_CbProgressMonitorStdErr_getPtr(JNIEnv *env,
		jobject self) {
	CBPM *pm = cb_pm_stderr_new();
	if (!pm)
		CBthrow_exception(env);
	return (intptr_t) pm;
}

/* functions */

JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CbProgressMonitorStdErr_start(JNIEnv *env,
		jobject self, jstring message) {
	jboolean icp;
	CBPM *pm = (CBPM *) get_ptr(env, self);
	const char *tmessage = (*env)->GetStringUTFChars(env, message, &icp);
	if (!cb_pm_stderr_start(pm, tmessage))
		CBthrow_exception(env);
	if (icp)
		(*env)->ReleaseStringUTFChars(env, message, tmessage);
}

JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CbProgressMonitorStdErr_report(JNIEnv *env,
		jobject self, jint state, jint max) {
	CBPM *pm = (CBPM *) get_ptr(env, self);
	if (!cb_pm_stderr_report(pm, (int) state, (int) max))
		CBthrow_exception(env);
}

JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CbProgressMonitorStdErr_finish(JNIEnv *env,
		jobject self) {
	CBPM *pm = (CBPM *) get_ptr(env, self);
	if (!cb_pm_stderr_finish(pm))
		CBthrow_exception(env);
}
