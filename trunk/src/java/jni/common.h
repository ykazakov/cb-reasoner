#include <jni.h>
#include <stdint.h>

#define CLSOBJECT      "java/lang/Object"
#define CLSSTRING      "java/lang/String"
#define CLSLIST        "java/util/List"
#define CLSARRAYLIST   "java/util/ArrayList"
#define CLSMAP         "java/util/Map"
#define CLSHASHMAP     "java/util/HashMap"
#define CLSMAPENTRY    "java/util/Map$Entry"
#define CLSSET         "java/util/Set"
#define CLSITERATOR    "java/util/Iterator"
#define CLSEILLARG     "java/lang/IllegalArgumentException"
#define CLSEOUTMEM     "java/lang/OutOfMemoryError"

intptr_t get_ptr(JNIEnv *env, jobject obj);

void throw_exception(JNIEnv *env);
