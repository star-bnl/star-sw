
#ifndef lev_i_hh
#define lev_i_hh

#include <CORBA.h>

#include <string.h>


#include "tdm_i.hh"

class socObject_i;


#ifndef socObject_iForwH
#define socObject_iForwH
CORBA_ObjectRef socObject_i_getBase (void *);
void socObject_i_release (void *, CORBA_Environment &IT_env=CORBA_default_environment);
socObject_i* socObject_i_duplicate (void *, CORBA_Environment &IT_env=CORBA_default_environment);
#endif
#define socObject_i_IMPL "socObject_i"

class socFactory_i;


#ifndef socFactory_iForwH
#define socFactory_iForwH
CORBA_ObjectRef socFactory_i_getBase (void *);
void socFactory_i_release (void *, CORBA_Environment &IT_env=CORBA_default_environment);
socFactory_i* socFactory_i_duplicate (void *, CORBA_Environment &IT_env=CORBA_default_environment);
#endif
#define socFactory_i_IMPL "socFactory_i"

class socCatalog_i;


#ifndef socCatalog_iForwH
#define socCatalog_iForwH
CORBA_ObjectRef socCatalog_i_getBase (void *);
void socCatalog_i_release (void *, CORBA_Environment &IT_env=CORBA_default_environment);
socCatalog_i* socCatalog_i_duplicate (void *, CORBA_Environment &IT_env=CORBA_default_environment);
#endif
#define socCatalog_i_IMPL "socCatalog_i"

class tdmObject_i;


#ifndef tdmObject_iForwH
#define tdmObject_iForwH
CORBA_ObjectRef tdmObject_i_getBase (void *);
void tdmObject_i_release (void *, CORBA_Environment &IT_env=CORBA_default_environment);
tdmObject_i* tdmObject_i_duplicate (void *, CORBA_Environment &IT_env=CORBA_default_environment);
#endif
#define tdmObject_i_IMPL "tdmObject_i"

class tdmTable_i;


#ifndef tdmTable_iForwH
#define tdmTable_iForwH
CORBA_ObjectRef tdmTable_i_getBase (void *);
void tdmTable_i_release (void *, CORBA_Environment &IT_env=CORBA_default_environment);
tdmTable_i* tdmTable_i_duplicate (void *, CORBA_Environment &IT_env=CORBA_default_environment);
#endif
#define tdmTable_i_IMPL "tdmTable_i"

class tdmDataset_i;


#ifndef tdmDataset_iForwH
#define tdmDataset_iForwH
CORBA_ObjectRef tdmDataset_i_getBase (void *);
void tdmDataset_i_release (void *, CORBA_Environment &IT_env=CORBA_default_environment);
tdmDataset_i* tdmDataset_i_duplicate (void *, CORBA_Environment &IT_env=CORBA_default_environment);
#endif
#define tdmDataset_i_IMPL "tdmDataset_i"

class tdmFactory_i;


#ifndef tdmFactory_iForwH
#define tdmFactory_iForwH
CORBA_ObjectRef tdmFactory_i_getBase (void *);
void tdmFactory_i_release (void *, CORBA_Environment &IT_env=CORBA_default_environment);
tdmFactory_i* tdmFactory_i_duplicate (void *, CORBA_Environment &IT_env=CORBA_default_environment);
#endif
#define tdmFactory_i_IMPL "tdmFactory_i"


#ifndef _levFactory_i_defined
#define _levFactory_i_defined
class levFactory_i_dispatch : public virtual socFactory_i_dispatch {
public:

   levFactory_i_dispatch (void *IT_p, CORBA_Object* IT_o, const char *IT_m, 
        CORBA_LoaderClass *IT_l, char *IT_i, void* IT_im)
       : CORBA_PPTR (IT_p,IT_o,IT_m,IT_l,IT_i,IT_im) {}


   levFactory_i_dispatch (char *IT_OR, void *IT_p, CORBA_Object *IT_o)
       : CORBA_PPTR (IT_OR,IT_p,IT_o) {}


   levFactory_i_dispatch () {}

   levFactory_i_dispatch (void *IT_p, CORBA_Object *IT_o, const char *IT_m, 
        char *IT_i, CORBA_Object* IT_ob, void* IT_im)
       : CORBA_PPTR (IT_p,IT_o,IT_m,IT_i,IT_ob,IT_im) {}


   virtual unsigned char dispatch (CORBA_Request &IT_r, 
        unsigned char IT_isTarget, void* IT_pp=NULL);


};

class levFactory_i;


#ifndef levFactory_iForwH
#define levFactory_iForwH
CORBA_ObjectRef levFactory_i_getBase (void *);
void levFactory_i_release (void *, CORBA_Environment &IT_env=CORBA_default_environment);
levFactory_i* levFactory_i_duplicate (void *, CORBA_Environment &IT_env=CORBA_default_environment);
#endif
#define levFactory_i_IMPL "levFactory_i"


class levFactory_i;
#define levFactory_i_IR "levFactory_i"
#define levFactory_i_IMPL "levFactory_i"

typedef levFactory_i* levFactory_iRef;
typedef levFactory_i* levFactory_i_ptr;
class levFactory_i: public virtual socFactory_i {
public:
    levFactory_i (char *IT_OR);
    levFactory_i () : CORBA_Object (1) {}
    levFactory_i* _duplicate(
            CORBA_Environment &IT_env=CORBA_default_environment) {
       CORBA_Object::_duplicate (IT_env); return this; }
   static levFactory_i* _bind (const char* IT_markerServer, const char* host,
		const CORBA_Context &IT_c, 
		CORBA_Environment &IT_env=CORBA_default_environment);
   static levFactory_i* _bind (CORBA_Environment &IT_env);
   static levFactory_i* _bind (const char* IT_markerServer=NULL, const char* host=NULL,
                 CORBA_Environment &IT_env=CORBA_default_environment);
    static levFactory_i* _narrow (CORBA_Object* , CORBA_Environment &IT_env=CORBA_default_environment);
    virtual tdmTable_i* env (CORBA_Environment &IT_env=CORBA_default_environment);
    virtual tdmTable_i* versions (CORBA_Environment &IT_env=CORBA_default_environment);
    	virtual STAFCV_T registerVersion (const char * name, const char * type, const char * version, CORBA_Environment &IT_env=CORBA_default_environment);
    	virtual STAFCV_T timeStamp (const char * stamp, CORBA_Environment &IT_env=CORBA_default_environment);
};


#define TIE_levFactory_i(X) levFactory_i##X

#define DEF_TIE_levFactory_i(X) \
	class levFactory_i##X : public virtual levFactory_i {		\
	  X* m_obj;						\
	public:							\
								\
	   levFactory_i##X  (X *objp, const char* m="", CORBA_LoaderClass *l=nil)\
		: levFactory_i(), CORBA_Object (), m_obj(objp) {	\
		m_pptr = new levFactory_i_dispatch		\
			(( levFactory_i*)this,(CORBA_Object*)this,m,l,levFactory_i_IR,m_obj);	\
	   }								\
	   levFactory_i##X  (CORBA_Object *IT_p, const char* IT_m="", void *IT_q=nil)\
		: levFactory_i(), CORBA_Object () {	\
		m_pptr = new levFactory_i_dispatch		\
			(( levFactory_i*)this,(CORBA_Object*)this,IT_m,levFactory_i_IR,IT_p,IT_q);	\
		m_obj = (X*)(m_pptr->getImplObj ());			\
	   }								\
								\
	   virtual ~levFactory_i##X  () {				\
		if (_okToDeleteImpl ()) delete m_obj; }					\
								\
	   virtual void* _deref () {					\
		return m_obj; }					\
								\
virtual IDREF_T idRef (CORBA_Environment &IT_env) {\
return m_obj->idRef(IT_env); }\
	\
virtual char * name (CORBA_Environment &IT_env) {\
return m_obj->name(IT_env); }\
	\
virtual char * type (CORBA_Environment &IT_env) {\
return m_obj->type(IT_env); }\
	\
virtual SOC_PTR_T ptr (CORBA_Environment &IT_env) {\
return m_obj->ptr(IT_env); }\
	\
virtual char * soRef (CORBA_Environment &IT_env) {\
return m_obj->soRef(IT_env); }\
	\
virtual void locked (unsigned char locked, CORBA_Environment &IT_env) {\
    m_obj->locked(locked,IT_env); }\
	\
virtual unsigned char locked (CORBA_Environment &IT_env) {\
return m_obj->locked(IT_env); }\
	\
	virtual STAFCV_T attach (CORBA_Environment &IT_env) {\
return m_obj->attach (IT_env);\
}\
	\
	virtual STAFCV_T release (CORBA_Environment &IT_env) {\
return m_obj->release (IT_env);\
}\
virtual long count (CORBA_Environment &IT_env) {\
return m_obj->count(IT_env); }\
	\
virtual long maxCount (CORBA_Environment &IT_env) {\
return m_obj->maxCount(IT_env); }\
	\
	virtual STAFCV_T addEntry (IDREF_T idRef, CORBA_Environment &IT_env) {\
return m_obj->addEntry ( idRef,IT_env);\
}\
	\
	virtual STAFCV_T deleteEntry (IDREF_T idRef, CORBA_Environment &IT_env) {\
return m_obj->deleteEntry ( idRef,IT_env);\
}\
	\
	virtual char * list (CORBA_Environment &IT_env) {\
return m_obj->list (IT_env);\
}\
	\
	virtual IDREF_T entry (long n, CORBA_Environment &IT_env) {\
return m_obj->entry ( n,IT_env);\
}\
virtual tdmTable_i* env (CORBA_Environment &IT_env) {\
return m_obj->env(IT_env); }\
	\
virtual tdmTable_i* versions (CORBA_Environment &IT_env) {\
return m_obj->versions(IT_env); }\
	\
	virtual STAFCV_T registerVersion (const char * name, const char * type, const char * version, CORBA_Environment &IT_env) {\
return m_obj->registerVersion ( name, type, version,IT_env);\
}\
	\
	virtual STAFCV_T timeStamp (const char * stamp, CORBA_Environment &IT_env) {\
return m_obj->timeStamp ( stamp,IT_env);\
}\
								\
	};


#define QUALS_levFactory_i	\
virtual IDREF_T idRef (CORBA_Environment &IT_env) {\
return m_obj->idRef(IT_env); }\
	\
virtual char * name (CORBA_Environment &IT_env) {\
return m_obj->name(IT_env); }\
	\
virtual char * type (CORBA_Environment &IT_env) {\
return m_obj->type(IT_env); }\
	\
virtual SOC_PTR_T ptr (CORBA_Environment &IT_env) {\
return m_obj->ptr(IT_env); }\
	\
virtual char * soRef (CORBA_Environment &IT_env) {\
return m_obj->soRef(IT_env); }\
	\
virtual void locked (unsigned char locked, CORBA_Environment &IT_env) {\
    m_obj->locked(locked,IT_env); }\
	\
virtual unsigned char locked (CORBA_Environment &IT_env) {\
return m_obj->locked(IT_env); }\
	\
	virtual STAFCV_T attach (CORBA_Environment &IT_env) {\
return m_obj->attach (IT_env);\
}\
	\
	virtual STAFCV_T release (CORBA_Environment &IT_env) {\
return m_obj->release (IT_env);\
}\
virtual long count (CORBA_Environment &IT_env) {\
return m_obj->count(IT_env); }\
	\
virtual long maxCount (CORBA_Environment &IT_env) {\
return m_obj->maxCount(IT_env); }\
	\
	virtual STAFCV_T addEntry (IDREF_T idRef, CORBA_Environment &IT_env) {\
return m_obj->addEntry ( idRef,IT_env);\
}\
	\
	virtual STAFCV_T deleteEntry (IDREF_T idRef, CORBA_Environment &IT_env) {\
return m_obj->deleteEntry ( idRef,IT_env);\
}\
	\
	virtual char * list (CORBA_Environment &IT_env) {\
return m_obj->list (IT_env);\
}\
	\
	virtual IDREF_T entry (long n, CORBA_Environment &IT_env) {\
return m_obj->entry ( n,IT_env);\
}\
virtual tdmTable_i* env (CORBA_Environment &IT_env) {\
return m_obj->env(IT_env); }\
	\
virtual tdmTable_i* versions (CORBA_Environment &IT_env) {\
return m_obj->versions(IT_env); }\
	\
	virtual STAFCV_T registerVersion (const char * name, const char * type, const char * version, CORBA_Environment &IT_env) {\
return m_obj->registerVersion ( name, type, version,IT_env);\
}\
	\
	virtual STAFCV_T timeStamp (const char * stamp, CORBA_Environment &IT_env) {\
return m_obj->timeStamp ( stamp,IT_env);\
}\




class levFactory_iProxyFactoryClass : public virtual socFactory_iProxyFactoryClass {
public:
   levFactory_iProxyFactoryClass (unsigned char IT_p=0)
		: CORBA_ProxyFactory (levFactory_i_IR, IT_p) {}

    virtual void* New (char *IT_OR, CORBA_Environment&);

    virtual void* New2 (); 

    virtual void* IT_castUp (void *IT_p, char* IT_s);

    virtual CORBA_PPTR* pptr (void *IT_p);

    virtual void baseInterfaces (_IDL_SEQUENCE_string&);


};

extern levFactory_iProxyFactoryClass levFactory_iProxyFactory;



#endif


#endif
