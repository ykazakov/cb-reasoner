#include "common.h"
#include <cb.h>
#include "cb_CBProgressMonitorSTDERR.h"

/* progress monitors */

static jclass cls_pm;
static jfieldID pm_fid_ptr;

/* init */
JNIEXPORT void JNICALL Java_cb_CBProgressMonitor_init(JNIEnv *env,
		jclass cls) {
	cls_pm = cls;
	pm_fid_ptr = (*env)->GetFieldID(env, cls, "ptr", "J");
}

/* destruct */
JNIEXPORT void JNICALL Java_cb_CBProgressMonitorSTDERR_destruct(JNIEnv *env,
		jobject self) {
	long ptr = (*env)->GetLongField(env, self, pm_fid_ptr);
	(*env)->SetLongField(env, self, pm_fid_ptr, 0);
	if (ptr != 0) {
		CBPM *pm = (CBPM *) (intptr_t) ptr;
		cb_pm_delete(pm);
	}
}

/* constructors */
JNIEXPORT void JNICALL Java_cb_CBProgressMonitorSTDERR_create(JNIEnv *env,
		jobject self) {
	CBPM *pm = cb_pm_stderr_new();
	if (!pm)
		throw_exception(env);
	(*env)->SetLongField(env, self, pm_fid_ptr, (intptr_t) pm);
}

/* functions */

JNIEXPORT void JNICALL Java_cb_CBProgressMonitorSTDERR_start(JNIEnv *env,
		jobject self, jstring message) {
	jboolean icp;
	CBPM *pm = (CBPM *) (intptr_t) (*env)->GetLongField(env, self, pm_fid_ptr);
	const char *tmessage = (*env)->GetStringUTFChars(env, message, &icp);
	if (!cb_pm_stderr_start(pm, tmessage))
		throw_exception(env);
	if (icp)
		(*env)->ReleaseStringUTFChars(env, message, tmessage);
}

JNIEXPORT void JNICALL Java_cb_CBProgressMonitorSTDERR_report(JNIEnv *env,
		jobject self, jint state, jint max) {
	CBPM *pm = (CBPM *) (intptr_t) (*env)->GetLongField(env, self, pm_fid_ptr);
	if (!cb_pm_stderr_report(pm, (int) state, (int) max))
		throw_exception(env);
}

JNIEXPORT void JNICALL Java_cb_CBProgressMonitorSTDERR_finish(JNIEnv *env,
		jobject self) {
	CBPM *pm = (CBPM *) (intptr_t) (*env)->GetLongField(env, self, pm_fid_ptr);
	if (!cb_pm_stderr_finish(pm))
		throw_exception(env);
}
