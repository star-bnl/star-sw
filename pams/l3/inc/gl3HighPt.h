//:>------------------------------------------------------------------
//: FILE:       gl3HighPt.h
//: HISTORY:
//:              1feb2000 version 1.00
//:<------------------------------------------------------------------
#include "gl3Analysis.h"

#ifndef GL3HIGHPT
#define GL3HIGHPT


class gl3HighPt: public gl3Analysis {
public:
    int init    ( l3List*  ) ;
    int process ( gl3Event* ) ;

};
#endif
