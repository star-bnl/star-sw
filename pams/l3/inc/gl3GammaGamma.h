//:>------------------------------------------------------------------
//: FILE:       gl3GammaGamma.h
//: HISTORY:
//:              3Feb2000 version 1.00
//:<------------------------------------------------------------------
#include "gl3Analysis.h"

#ifndef GL3GAMMAGAMMA
#define GL3GAMMAGAMMA


class gl3GammaGamma: public gl3Analysis {
public:
    int init    ( l3List*  ) ;
    int process ( gl3Event* ) ;

};
#endif
