//:>------------------------------------------------------------------
//: FILE:       gl3MomRes.h
//: HISTORY:
//:             4may2000 version 1.00
//:<------------------------------------------------------------------
#include "gl3Analysis.h"


#ifndef GL3MOMRES
#define GL3MOMRES


class gl3MomRes: public gl3Analysis {

  gl3Histo *hist;
 
 public:
    int init    ( l3List*  ) ;
    int process ( gl3Event* ) ;

};
#endif
