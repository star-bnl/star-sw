
#ifndef levEnv_hh
#define levEnv_hh

#include <CORBA.h>

#include <string.h>


#ifndef levEnv_defined
#define levEnv_defined

struct levEnv {
    char name[32];
    char value[128];

    void encodeOp (CORBA_Request &IT_r) const;
    void decodeOp (CORBA_Request &IT_r);
    void decodeInOutOp (CORBA_Request &IT_r);
};


#endif


#endif
