//:>------------------------------------------------------------------
//: FILE:       gl3Residuals.h
//: HISTORY:
//:             4may2000 version 1.00
//:<------------------------------------------------------------------
#include "gl3Analysis.h"


#ifndef GL3RESIDUALS
#define GL3RESIDUALS

class gl3Residuals: public gl3Analysis {

 gl3Histo *Resx;
 gl3Histo *Resz;
 
public:
    int init    ( l3List*  ) ;
    int process ( gl3Event* ) ;

};
#endif
