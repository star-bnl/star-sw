//:>------------------------------------------------------------------
//: FILE:       gl3dEdx.h
//: HISTORY:
//:              3feb2000 version 1.00
//:<------------------------------------------------------------------
#include "gl3Analysis.h"

#ifndef GL3DEDX  
#define GL3DEDX  


class gl3dEdx: public gl3Analysis {
public:
    int init    ( l3List*  ) ;
    int process ( gl3Event* ) ;

};
#endif
