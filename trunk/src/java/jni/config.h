#include <jni.h>
#include <stdint.h>

intptr_t get_ptr(JNIEnv *env, jobject obj);
jobject new_obj(JNIEnv *env, jclass cls, long ptr);

#define CBthrow_exception(env) \
	throw_exception(env, __FUNCTION__, __FILE__, __LINE__);


#define CBPREFIX 									"org/semanticweb/cb/reasoner/"

#define CLSCBObject									CBPREFIX "CbObject"

#define CLSCBClassExpression						CBPREFIX "CbClassExpression"
#define CLSCBClass									CBPREFIX "CbClass"
#define CLSCBObjectIntersectionOf					CBPREFIX "CbObjectIntersectionOf"
#define CLSCBObjectSomeValuesFrom					CBPREFIX "CbObjectSomeValuesFrom"

#define CLSCBObjectPropertyExpression				CBPREFIX "CbObjectPropertyExpression"
#define CLSCBObjectProperty							CBPREFIX "CbObjectProperty"
#define CLSCBObjectInverseOf						CBPREFIX "CbObjectInverseOf"

#define CLSCBDeclarationAxiom						CBPREFIX "CbDeclarationAxiom"
#define CLSCBClassDeclarationAxiom					CBPREFIX "CbClassDeclarationAxiom"
#define CLSCBObjectPropertyDeclarationAxiom			CBPREFIX "CbObjectPropertyDeclarationAxiom"

#define CLSCBClassAxiom								CBPREFIX "CbClassAxiom"
#define CLSCBEquivalentClassesAxiom					CBPREFIX "CbEquivalentClassesAxiom"
#define CLSCBSubClassOfAxiom						CBPREFIX "CbSubClassOfAxiom"

#define CLSCBObjectPropertyAxiom					CBPREFIX "CbObjectPropertyAxiom"
#define CLSCBFunctionalObjectPropertyAxiom			CBPREFIX "CbFunctionalObjectPropertyAxiom"
#define CLSCBInverseFunctionalObjectPropertyAxiom	CBPREFIX "CbInverseFunctionalObjectPropertyAxiom"
#define CLSCBInverseObjectPropertiesAxiom			CBPREFIX "CbInverseObjectPropertiesAxiom"
#define CLSCBSubObjectPropertyOfAxiom				CBPREFIX "CbSubObjectPropertyOfAxiom"
#define CLSCBTransitiveObjectPropertyAxiom			CBPREFIX "CbTransitiveObjectPropertyAxiom"

#define CLSCBClassNode								CBPREFIX "CbClassNode"
#define CLSCBClassTaxonomyNode						CBPREFIX "CbClassTaxonomyNode"
#define CLSCBAxiomIterator							CBPREFIX "CbAxiomIterator"
#define CLSCBException								CBPREFIX "CbException"

void throw_exception(JNIEnv *env, const char *function, const char *file, int line);


jobject class_expression_new(JNIEnv *, CBCE *);
jobject object_property_expression_new(JNIEnv *, CBOPE *);
jobject class_axiom_new(JNIEnv *, CBCAX *);
jobject object_property_axiom_new(JNIEnv *, CBOPAX *);
