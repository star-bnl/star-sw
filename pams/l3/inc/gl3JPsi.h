//:>------------------------------------------------------------------
//: FILE:       gl3JPsi.h
//: HISTORY:
//:              3feb2000 version 1.00
//:<------------------------------------------------------------------
#include "gl3Analysis.h"

#ifndef GL3JPSI  
#define GL3JPSI  


class gl3JPsi: public gl3Analysis {
public:
    int init    ( l3List*  ) ;
    int process ( gl3Event* ) ;

};
#endif
