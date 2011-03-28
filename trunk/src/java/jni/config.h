#include <jni.h>
#include <stdint.h>

intptr_t get_ptr(JNIEnv *env, jobject obj);
void set_ptr(JNIEnv *env, jobject obj, long ptr);
jobject new_obj(JNIEnv *env, jclass cls, long ptr);

#define CBthrow_exception(env) \
	throw_exception(env, __FUNCTION__, __FILE__, __LINE__);


#define CBPREFIX 									"org/semanticweb/cb/reasoner/"

#define CLSCBObject									CBPREFIX "CBObject"

#define CLSCBClassExpression						CBPREFIX "CBClassExpression"
#define CLSCBClass									CBPREFIX "CBClass"
#define CLSCBObjectIntersectionOf					CBPREFIX "CBObjectIntersectionOf"
#define CLSCBObjectSomeValuesFrom					CBPREFIX "CBObjectSomeValuesFrom"

#define CLSCBObjectPropertyExpression				CBPREFIX "CBObjectPropertyExpression"
#define CLSCBObjectProperty							CBPREFIX "CBObjectProperty"
#define CLSCBObjectInverseOf						CBPREFIX "CBObjectInverseOf"

#define CLSCBDeclarationAxiom						CBPREFIX "CBDeclarationAxiom"
#define CLSCBClassDeclarationAxiom					CBPREFIX "CBClassDeclarationAxiom"
#define CLSCBObjectPropertyDeclarationAxiom			CBPREFIX "CBObjectPropertyDeclarationAxiom"

#define CLSCBClassAxiom								CBPREFIX "CBClassAxiom"
#define CLSCBEquivalentClassesAxiom					CBPREFIX "CBEquivalentClassesAxiom"
#define CLSCBSubClassOfAxiom						CBPREFIX "CBSubClassOfAxiom"

#define CLSCBObjectPropertyAxiom					CBPREFIX "CBObjectPropertyAxiom"
#define CLSCBFunctionalObjectPropertyAxiom			CBPREFIX "CBFunctionalObjectPropertyAxiom"
#define CLSCBInverseFunctionalObjectPropertyAxiom	CBPREFIX "CBInverseFunctionalObjectPropertyAxiom"
#define CLSCBInverseObjectPropertiesAxiom			CBPREFIX "CBInverseObjectPropertiesAxiom"
#define CLSCBSubObjectPropertyOfAxiom				CBPREFIX "CBSubObjectPropertyOfAxiom"
#define CLSCBTransitiveObjectPropertyAxiom			CBPREFIX "CBTransitiveObjectPropertyAxiom"

#define CLSCBClassNode								CBPREFIX "CBClassNode"
#define CLSCBClassTaxonomyNode						CBPREFIX "CBClassTaxonomyNode"
#define CLSCBAxiomIterator							CBPREFIX "CBAxiomIterator"
#define CLSCBException								CBPREFIX "CBException"

void throw_exception(JNIEnv *env, const char *function, const char *file, int line);


jobject class_expression_new(JNIEnv *, CBCE *);
jobject object_property_expression_new(JNIEnv *, CBOPE *);
jobject class_axiom_new(JNIEnv *, CBCAX *);
jobject object_property_axiom_new(JNIEnv *, CBOPAX *);
