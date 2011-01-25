#include "common.h"
#include <cb.h>

/* exceptions */

void throw_exception(JNIEnv *env) {
	/* TODO: insert backtrace into the JAVA backtrace */
	jclass cls;
	char *exception = cb_get_exception();
	char *backtrace = cb_get_backtrace();
	char *message;
	const char *fmt = "%s\n%s";
	int len = snprintf(NULL, 0, fmt, exception, backtrace);
	if (!(message = malloc((len + 1) * sizeof(char)))) {
		/* allocating a smaller buffer */
		len = 1023;
		message = malloc((len + 1) * sizeof(char));
	}
	snprintf(message, len + 1, fmt, exception, backtrace);
	cls = (*env)->FindClass(env, "cb/CBException");
	(*env)->ThrowNew(env, cls, message);
}

JNIEXPORT jint JNICALL JNI_OnLoad(JavaVM *jvm, void *reserved) {
	JNIEnv *env;
	if ((*jvm)->GetEnv(jvm, (void **) &env, JNI_VERSION_1_2)) {
		return JNI_ERR; /* JNI version not supported */
	}
	static char * argv[] = { };
	cb_startup(argv);
	return JNI_VERSION_1_2;
}

intptr_t get_ptr(JNIEnv *env, jobject obj) {
	jclass cls = (*env)->GetObjectClass(env, obj);
	jfieldID fid = (*env)->GetFieldID(env, cls, "ptr", "J");
	return (intptr_t) (*env)->GetLongField(env, obj, fid);
}
