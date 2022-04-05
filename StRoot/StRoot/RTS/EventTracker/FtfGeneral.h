#ifndef L3GENERAL_H
#define L3GENERAL_H

#include <math.h>
//#include <stdio.h>
//#include <stdlib.h>
//#include <algorithm>

//#include "l3Log.h"

// needed for solaris cc5
#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif

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
#define seta(r,z)   (float)(3.0F * (z) / (fabs(z)+2.0F*(r)))
#define reta(eta,r) ((2.F*(r)*eta / ( 3 - fabs(eta)) )) 
#define sgn(a)      (float)( ( (a) > 0   ) ? (1) :(-1) )
#define square(a)   (float)( (a) * (a) )

class FtfContainer{
   public:
     void *first; 
     void *last;  
} ;


#endif



