//:>------------------------------------------------------------------
//: FILE:       gl3Analysis.h
//: HISTORY:
//:              1feb2000 version 1.00
//:<------------------------------------------------------------------
#include <stdio.h>
#include <math.h>
#include "l3List.h"
#include "gl3Event.h"
#include "gl3Histo.h"

#ifndef GL3ANALYSIS 
#define GL3ANALYSIS  


class gl3Analysis {
public:
    gl3Analysis();
    virtual ~gl3Analysis();
    virtual int init    ( l3List* histos )=0 ;
    virtual int process ( gl3Event* event )=0 ;
};
#endif
