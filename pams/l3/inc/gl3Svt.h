//:>------------------------------------------------------------------
//: FILE:       gl3Svt.h
//: HISTORY:
//:             29feb2000 version 1.00
//:<------------------------------------------------------------------
#include "gl3Analysis.h"

#ifndef GL3SVT   
#define GL3SVT  


class gl3Svt: public gl3Analysis {

    gl3Histo* hx ;
    gl3Histo* hy ;
    gl3Histo* hz ;
public:
    int init    ( l3List*  ) ;
    int process ( gl3Event* ) ;

};
#endif
