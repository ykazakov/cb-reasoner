#include <cb.h>
#include "config.h"
#include "org_semanticweb_cb_reasoner_CBClassTaxonomyNode.h"

/* destruct */
JNIEXPORT void JNICALL Java_org_semanticweb_cb_reasoner_CBClassTaxonomyNode_destruct(JNIEnv *env,
		jobject self) {
	long ptr = get_ptr(env, self);
	set_ptr(env, self, 0);
	if (ptr != 0) {
		CBCN *cn = (CBCN *) (intptr_t) ptr;
		cb_class_node_delete(cn);
	}
}

/* constructors */

/* methods */
JNIEXPORT jobjectArray JNICALL Java_org_semanticweb_cb_reasoner_CBClassTaxonomyNode_getCBClasses(
		JNIEnv *env, jobject self) {
	int i, len;
	jobjectArray res;
	jclass cls_c = (*env)->FindClass(env, CLSCBClass);
	CBCE *ce;
	CBCN *cn = (CBCN *) get_ptr(env, self);
	CBCE **classes = cb_class_node_get_classes(cn, &len);
	if (!classes)
		CBthrow_exception(env);
	res = (jobjectArray) (*env)->NewObjectArray(env, len, cls_c, NULL);
	for (i = 0; i < len; i++) {
		ce = classes[i];
		(*env)->SetObjectArrayElement(env, res, i,
				new_obj(env, cls_c, (intptr_t) ce));
	}
	free(classes);
	return (res);
}

JNIEXPORT jobjectArray JNICALL Java_org_semanticweb_cb_reasoner_CBClassTaxonomyNode_getChildNodes(JNIEnv *env,
		jobject self) {
	int i, len;
	jobjectArray res;
	jclass cls_cn = (*env)->FindClass(env, CLSCBClassTaxonomyNode);
	CBCN *cn_ch;
	CBCN *cn = (CBCN *) get_ptr(env, self);
	CBCN **children = cb_class_node_get_child_nodes(cn, &len);
	if (!children)
		CBthrow_exception(env);
	res = (jobjectArray) (*env)->NewObjectArray(env, len, cls_cn, NULL);
	for (i = 0; i < len; i++) {
		cn_ch = children[i];
		(*env)->SetObjectArrayElement(env, res, i,
				new_obj(env, cls_cn, (intptr_t) cn_ch));
	}
	free(children);
	return (res);
}

JNIEXPORT jobjectArray JNICALL Java_org_semanticweb_cb_reasoner_CBClassTaxonomyNode_getParentNodes(JNIEnv *env,
		jobject self) {
	int i, len;
	jobjectArray res;
	jclass cls_cn = (*env)->FindClass(env, CLSCBClassTaxonomyNode);
	CBCN *cn_pt;
	CBCN *cn = (CBCN *) get_ptr(env, self);
	CBCN **parents = cb_class_node_get_parent_nodes(cn, &len);
	if (!parents)
		CBthrow_exception(env);
	res = (jobjectArray) (*env)->NewObjectArray(env, len, cls_cn, NULL);
	for (i = 0; i < len; i++) {
		cn_pt = parents[i];
		(*env)->SetObjectArrayElement(env, res, i,
				new_obj(env, cls_cn, (intptr_t) cn_pt));
	}
	free(parents);
	return (res);
}
