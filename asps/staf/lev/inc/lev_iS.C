
#include "lev_i.hh"


#define levFactory_i_dispatch_impl

unsigned char levFactory_i_dispatch::dispatch (CORBA_Request &IT_r, 
 unsigned char IT_isTarget, void *IT_pp) {
    if (!IT_pp)
       IT_pp = m_obj;
    const char *IT_s = IT_r.getOperation ();
    if (!strcmp (IT_s,"_get_env")) {
        tdmTable_i* env;
        CORBA_Environment IT_env (IT_r);
        CORBA_Filter* IT_f = CORBA_Orbix.getFilter ();
        if (!IT_r.tcAssert ("\
Ro~_get_env~>{O~tdmTable_i},N{}\
"))
            return 1;
        if (IT_f)
        	IT_f->inRequestPostM (IT_r, IT_env);
        if (!IT_r.isException (IT_env))
            env = ((levFactory_i*)IT_pp)->env(IT_env);

        if (!IT_r.isException (IT_env)) {
            if (!IT_r.convertToReply ("\
O~tdmTable_i\
", IT_env)) return 1;
            IT_r << (CORBA_Object*)env;
            if (env) env->_release ();
        }
        else IT_r.makeSystemException (IT_env);

        return 1;
    }

    else if (!strcmp (IT_s,"_get_versions")) {
        tdmTable_i* versions;
        CORBA_Environment IT_env (IT_r);
        CORBA_Filter* IT_f = CORBA_Orbix.getFilter ();
        if (!IT_r.tcAssert ("\
Ro~_get_versions~>{O~tdmTable_i},N{}\
"))
            return 1;
        if (IT_f)
        	IT_f->inRequestPostM (IT_r, IT_env);
        if (!IT_r.isException (IT_env))
            versions = ((levFactory_i*)IT_pp)->versions(IT_env);

        if (!IT_r.isException (IT_env)) {
            if (!IT_r.convertToReply ("\
O~tdmTable_i\
", IT_env)) return 1;
            IT_r << (CORBA_Object*)versions;
            if (versions) versions->_release ();
        }
        else IT_r.makeSystemException (IT_env);

        return 1;
    }

    else if (!strcmp(IT_s,"registerVersion")) {
        STAFCV_T IT_result;
        CORBA_Environment IT_env (IT_r);
        CORBA_Filter* IT_f = CORBA_Orbix.getFilter ();
        if (!IT_r.tcAssert ("\
Ro~registerVersion~+name{0},+type{0},+version{0},>{l},N{}\
"))
            return 1;
        char * name;
        IT_r.decodeStringOp(name);

        char * type;
        IT_r.decodeStringOp(type);

        char * version;
        IT_r.decodeStringOp(version);

        if (IT_f && !IT_r.isException (IT_env))
        	IT_f->inRequestPostM (IT_r, IT_env);
        if (!IT_r.isException (IT_env))
        	IT_result = ((levFactory_i*)IT_pp)->registerVersion ( name,  type,  version, IT_env);

        delete [] name;

        delete [] type;

        delete [] version;

        if (!IT_r.isException (IT_env)) {
            if (!IT_r.convertToReply ("\
l\
", IT_env)) return 1;


            IT_r << IT_result;
            }

        else IT_r.makeSystemException (IT_env);
        return 1;
    }

    else if (!strcmp(IT_s,"timeStamp")) {
        STAFCV_T IT_result;
        CORBA_Environment IT_env (IT_r);
        CORBA_Filter* IT_f = CORBA_Orbix.getFilter ();
        if (!IT_r.tcAssert ("\
Ro~timeStamp~+stamp{0},>{l},N{}\
"))
            return 1;
        char * stamp;
        IT_r.decodeStringOp(stamp);

        if (IT_f && !IT_r.isException (IT_env))
        	IT_f->inRequestPostM (IT_r, IT_env);
        if (!IT_r.isException (IT_env))
        	IT_result = ((levFactory_i*)IT_pp)->timeStamp ( stamp, IT_env);

        delete [] stamp;

        if (!IT_r.isException (IT_env)) {
            if (!IT_r.convertToReply ("\
l\
", IT_env)) return 1;
            IT_r << IT_result;
            }

        else IT_r.makeSystemException (IT_env);
        return 1;
    }

    else if (socFactory_i_dispatch::dispatch (IT_r, 0,
         (socFactory_i*)((levFactory_i*)IT_pp))) {
   return 1;
    }

    else if (IT_isTarget)
        IT_r.makeRuntimeException2 ();

    return 0;
}

#include "lev_iC.C"

