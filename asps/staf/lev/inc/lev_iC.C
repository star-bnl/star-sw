
#include "lev_i.hh"

levFactory_i::levFactory_i (char *IT_OR) {
      m_pptr = new levFactory_i_dispatch (IT_OR, this,(CORBA_Object*)this);
} 

#ifndef levFactory_iForwC
#define levFactory_iForwC
CORBA_ObjectRef levFactory_i_getBase(void *IT_p){
    return (levFactory_i*)IT_p;}

void levFactory_i_release (void *IT_p, CORBA_Environment &IT_env) {
    ((levFactory_i*)IT_p)->_release(IT_env);}

levFactory_i* levFactory_i_duplicate (void *IT_p, CORBA_Environment &IT_env) {
    return ((levFactory_i*)IT_p)->_duplicate(IT_env); }
#endif



levFactory_i* levFactory_i:: _bind (const char* IT_markerServer, const char* host,
		const CORBA_Context &IT_c, 
		CORBA_Environment &IT_env) {
       levFactory_i*IT_p =
 (levFactory_i*)CORBA_Factory.New (IT_markerServer, IT_env, IT_c, host, 
		levFactory_i_IMPL, levFactory_i_IR);
       return IT_p ? IT_p->_duplicate () : NULL; }



levFactory_i* levFactory_i:: _bind (CORBA_Environment &IT_env) {
       return _bind (NULL,NULL,CORBA_Context(), IT_env); }


levFactory_i* levFactory_i:: _bind (const char* IT_markerServer, const char* host,
                CORBA_Environment &IT_env) {
       return _bind (IT_markerServer, host, CORBA_Context (), IT_env); }
levFactory_i* levFactory_i::_narrow (CORBA_Object* IT_obj, CORBA_Environment &IT_env) {
       levFactory_i* IT_p = (levFactory_i*)/*CORBA_Object::*/_castDown (IT_obj, levFactory_i_IR, IT_env);
        return IT_p ? IT_p->_duplicate(IT_env) : NULL;
   }

void* levFactory_iProxyFactoryClass::New (char *IT_OR, CORBA_Environment&) {
        return  new levFactory_i(IT_OR);}

void* levFactory_iProxyFactoryClass::New2 () {
        return  new levFactory_i();}

void* levFactory_iProxyFactoryClass::IT_castUp (void *IT_p, char* IT_s) {
      void *IT_l;
      if (!CORBA__interfaceCmp (IT_s,levFactory_i_IR))
         return IT_p;
      else if (IT_l=socFactory_iProxyFactoryClass::IT_castUp((socFactory_i*)((levFactory_i*)IT_p),IT_s))
          return IT_l;
      else return NULL;
    }


CORBA_PPTR* levFactory_iProxyFactoryClass::pptr (void *IT_p) {
       return ((levFactory_i*)IT_p)->_pptr ();}

void levFactory_iProxyFactoryClass::baseInterfaces (_IDL_SEQUENCE_string& seq) {
      add (seq, levFactory_i_IR);
      socFactory_iProxyFactoryClass::baseInterfaces (seq);
}

tdmTable_i* levFactory_i::env (CORBA_Environment &IT_env) {

    if (IT_env || m_isNull) return new tdmTable_i;

    CORBA_Request IT_r (this, "_get_env",IT_env,1,0);
    IT_r.invoke(CORBA_Flags(0), IT_env);
    if (!IT_r.isException (IT_env)) {
        tdmTable_i* env;
        env = (tdmTable_i*) IT_r.decodeObjRef (tdmTable_i_IR);
        if (env) env->_duplicate ();
        IT_r.checkEnv (IT_env);
        return env;
    }
    return new tdmTable_i;
}

tdmTable_i* levFactory_i::versions (CORBA_Environment &IT_env) {

    if (IT_env || m_isNull) return new tdmTable_i;

    CORBA_Request IT_r (this, "_get_versions",IT_env,1,0);
    IT_r.invoke(CORBA_Flags(0), IT_env);
    if (!IT_r.isException (IT_env)) {
        tdmTable_i* versions;
        versions = (tdmTable_i*) IT_r.decodeObjRef (tdmTable_i_IR);
        if (versions) versions->_duplicate ();
        IT_r.checkEnv (IT_env);
        return versions;
    }
    return new tdmTable_i;
}

STAFCV_T levFactory_i:: registerVersion(const char * name, const char * type, const char * version, CORBA_Environment &IT_env) {

    if (IT_env || m_isNull) return 0;
    CORBA_Request IT_r (this, "registerVersion",IT_env,1,0);
    if (!IT_r.isException (IT_env)) {
        IT_r.encodeStringOp (name);

        IT_r.encodeStringOp (type);

        IT_r.encodeStringOp (version);
    }

    IT_r.invoke (CORBA_Flags(0),IT_env);
    if (!IT_r.isException (IT_env)) {


        STAFCV_T IT_result;
        IT_r >> IT_result;
        IT_r.checkEnv (IT_env);
        return IT_result;
    }
    return 0;
}

STAFCV_T levFactory_i:: timeStamp(const char * stamp, CORBA_Environment &IT_env) {

    if (IT_env || m_isNull) return 0;
    CORBA_Request IT_r (this, "timeStamp",IT_env,1,0);
    if (!IT_r.isException (IT_env)) {
        IT_r.encodeStringOp (stamp);
    }

    IT_r.invoke (CORBA_Flags(0),IT_env);
    if (!IT_r.isException (IT_env)) {
        STAFCV_T IT_result;
        IT_r >> IT_result;
        IT_r.checkEnv (IT_env);
        return IT_result;
    }
    return 0;
}


levFactory_iProxyFactoryClass levFactory_iProxyFactory(1);


#ifndef levFactory_i_dispatch_impl

unsigned char levFactory_i_dispatch::dispatch (CORBA_Request &IT_r, 
    unsigned char, void *) {
      IT_r.makeRuntimeException1 ("levFactory_i");
      return 0;
}
 
#endif

