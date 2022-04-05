//:>------------------------------------------------------------------
//: FILE:       FtfGeneral.h
//: HISTORY:
//:             27jan2000 ppy Start recording history
//:             27jan2000 ppy FtfVolume added to FtfGeneral
//:             27jan2000 ppy VOLUME, ROW and AREA classes replaced with
//:                           FtfContainer
//:             19apr2000 ppy cs ppy  dEdxCalibrationConstant 
//:<------------------------------------------------------------------
#ifndef FTFGENERAL
#define FTFGENERAL

#include "Stl3Util/base/FtfLog.h"


#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <algorithm>


// needed for solaris cc5
using namespace std;
using std::min;
using std::max;

//
//    Some constants 
//
const float toDeg =57.29577951F ;
const float pi    =3.141592654F ;
const float twoPi =2.F*pi ; 
const float piHalf = 0.5 * pi ;
const double bFactor = 0.0029979 ;
//
//-->   Functions
//
//VP #define min(a,b)    ( ( (a) < (b) ) ? (a) : (b) )
//VP #define max(a,b)    ( ( (a) > (b) ) ? (a) : (b) )
#define seta(r,z)   (float)(3.0F * (z) / (fabs(z)+2.0F*(r)))
#define reta(eta,r) ((2.F*(r)*eta / ( 3 - fabs(eta)) )) 
#define sgn(a)      (float)( ( (a) > 0   ) ? (1) :(-1) )
#define square(a)   (float)( (a) * (a) )


//VP extern double fmod(double,double);
//VP extern double ::sqrt(double);
//VP extern double fabs(double);
//VP extern double atan2(double,double);


class FtfContainer{
   public:
     void *first; 
     void *last;  
} ;


#endif



