
#include "levVersions.hh"


#ifndef levVersions_Ops
#define levVersions_Ops

void levVersions:: encodeOp (CORBA_Request &IT_r) const {
    {
        unsigned long IT_len =  32;
        char* IT_arr = (char*) name;
        IT_r.encodeCharArray (IT_arr,IT_len);
    }
    {
        unsigned long IT_len =  32;
        char* IT_arr = (char*) type;
        IT_r.encodeCharArray (IT_arr,IT_len);
    }
    {
        unsigned long IT_len =  256;
        char* IT_arr = (char*) version;
        IT_r.encodeCharArray (IT_arr,IT_len);
    }
}

void levVersions:: decodeOp (CORBA_Request &IT_r) {
    {
        unsigned long IT_len =  32;
        char* IT_arr = (char*) name;
        IT_r.decodeCharArray (IT_arr,IT_len);
    }
    {
        unsigned long IT_len =  32;
        char* IT_arr = (char*) type;
        IT_r.decodeCharArray (IT_arr,IT_len);
    }
    {
        unsigned long IT_len =  256;
        char* IT_arr = (char*) version;
        IT_r.decodeCharArray (IT_arr,IT_len);
    }
}

void levVersions:: decodeInOutOp (CORBA_Request &IT_r) {
    {
        unsigned long IT_len =  32;
        char* IT_arr = (char*) name;
        IT_r.decodeCharArray (IT_arr,IT_len);
    }
    {
        unsigned long IT_len =  32;
        char* IT_arr = (char*) type;
        IT_r.decodeCharArray (IT_arr,IT_len);
    }
    {
        unsigned long IT_len =  256;
        char* IT_arr = (char*) version;
        IT_r.decodeCharArray (IT_arr,IT_len);
    }
}


#endif
