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
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <algorithm>

// needed for solaris cc5
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

//
//    Some constrats 
//
const float toDeg =57.29577951F ;
const float pi    =3.141592654F ;
const float twoPi =2.F*pi ; 
const float piHalf = 0.5 * pi ;
const double bFactor = 0.0029979 ;
//
//-->   Functions
//
//#define min(a,b)    ( ( (a) < (b) ) ? (a) : (b) )
//#define max(a,b)    ( ( (a) > (b) ) ? (a) : (b) )
#define seta(r,z)   (float)(3.0F * (z) / (fabs(z)+2.0F*(r)))
#define reta(eta,r) ((2.F*(r)*eta / ( 3 - fabs(eta)) )) 
#define sgn(a)      (float)( ( (a) > 0   ) ? (1) :(-1) )
#define square(a)   (float)( (a) * (a) )


extern double fmod(double,double);
extern double sqrt(double);
extern double fabs(double);
extern double atan2(double,double);


class FtfContainer{
   public:
     void *first; 
     void *last;  
} ;

#endif

