
#include "levEnv.hh"


#ifndef levEnv_Ops
#define levEnv_Ops

void levEnv:: encodeOp (CORBA_Request &IT_r) const {
    {
        unsigned long IT_len =  32;
        char* IT_arr = (char*) name;
        IT_r.encodeCharArray (IT_arr,IT_len);
    }
    {
        unsigned long IT_len =  128;
        char* IT_arr = (char*) value;
        IT_r.encodeCharArray (IT_arr,IT_len);
    }
}

void levEnv:: decodeOp (CORBA_Request &IT_r) {
    {
        unsigned long IT_len =  32;
        char* IT_arr = (char*) name;
        IT_r.decodeCharArray (IT_arr,IT_len);
    }
    {
        unsigned long IT_len =  128;
        char* IT_arr = (char*) value;
        IT_r.decodeCharArray (IT_arr,IT_len);
    }
}

void levEnv:: decodeInOutOp (CORBA_Request &IT_r) {
    {
        unsigned long IT_len =  32;
        char* IT_arr = (char*) name;
        IT_r.decodeCharArray (IT_arr,IT_len);
    }
    {
        unsigned long IT_len =  128;
        char* IT_arr = (char*) value;
        IT_r.decodeCharArray (IT_arr,IT_len);
    }
}


#endif
