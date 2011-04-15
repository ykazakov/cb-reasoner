#include <cb.h>
#include <signal.h>
#include <stdio.h>
#include "config.h"

#if defined (__MSVC__)
#define snprintf sprintf_s
#endif

static jfieldID fid_ptr;
static jmethodID mid_init;

/* initialisation */

JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CObject_init(
		JNIEnv *env, jclass cls) {
	fid_ptr = (*env)->GetFieldID(env, cls, "ptr", "J");
	if (fid_ptr == NULL)
		CBthrow_exception(env);
	mid_init = (*env)->GetMethodID(env, cls, "<init>", "(J)V");
	if (mid_init == NULL)
		CBthrow_exception(env);
}

/* setting of pointers and creation of objects */

intptr_t get_ptr(JNIEnv *env, jobject obj) {
	return (intptr_t) (*env)->GetLongField(env, obj, fid_ptr);
}

jobject new_obj(JNIEnv *env, jclass cls, long ptr) {
	return (*env)->NewObject(env, cls, mid_init, ptr);
}

/* exceptions */

static char *exception_fmt = "%s\n%sCalled from  \"%s\":%s:%d";

void throw_exception(JNIEnv *env, const char *function, const char *file,
		int line) {
	/* TODO: insert backtrace into the JAVA backtrace */
	jclass cls;
	char *exception = cb_get_exception();
	char *backtrace = cb_get_backtrace();
	char *message;
	int
			len =
					snprintf(NULL, 0, exception_fmt, exception, backtrace, file, function, line);
	if (!(message = malloc((len + 1) * sizeof(char)))) {
		/* allocating a smaller buffer */
		len = 1023;
		message = malloc((len + 1) * sizeof(char));
	}
	snprintf(message, len + 1, exception_fmt, exception, backtrace, file, function, line);
	cls = (*env)->FindClass(env, CLSCBException);
	(*env)->ThrowNew(env, cls, message);
}

JNIEXPORT jint JNICALL JNI_OnLoad(JavaVM *jvm, void *reserved) {
	JNIEnv *env;
	static char * argv[] = { NULL };
	if ((*jvm)->GetEnv(jvm, (void **) &env, JNI_VERSION_1_2)) {
		return JNI_ERR; /* JNI version not supported */
	}
	/* Java on linux and solaris use SIGSEGV for communication; we don't want ocaml
	 * to catch that
	 */
#if defined (__LINUX__) || defined (__SOLARIS__)
	struct sigaction oldHandler;
	sigaction(SIGSEGV, &oldHandler, &oldHandler);
#endif
	cb_startup(argv);
#if defined (__LINUX__) || defined (__SOLARIS__)
	sigaction(SIGSEGV, &oldHandler, NULL);
#endif
	return JNI_VERSION_1_2;
}
