
#ifndef levVersions_hh
#define levVersions_hh

#include <CORBA.h>

#include <string.h>


#ifndef levVersions_defined
#define levVersions_defined

struct levVersions {
    char name[32];
    char type[32];
    char version[256];

    void encodeOp (CORBA_Request &IT_r) const;
    void decodeOp (CORBA_Request &IT_r);
    void decodeInOutOp (CORBA_Request &IT_r);
};


#endif


#endif
