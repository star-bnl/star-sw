#ifndef FTFGENERAL
#define FTFGENERAL
#include <math.h>
//
//    Some constrats 
//
const float To_deg=57.29577951F ;
const float Pi    =3.141592654F ;
const float Twopi =2.F*Pi ; 
//
//-->   Functions
//

#define min(a,b)    ( ( (a) < (b) ) ? (a) : (b) )
#define max(a,b)    ( ( (a) > (b) ) ? (a) : (b) )
//#define seta(r,z)   (float)(3.0F * (z) / (fabs(z)+2.0F*(r)))
#define seta(r,z)  (-log(r/(sqrt(r*r+z*z)+z)))
#define reta(eta,r) ((2.F*(r)*eta / ( 3 - fabs(eta)) )) 
#define sgn(a)      (float)( ( (a) > 0   ) ? (1) :(-1) )
#define square(a)   (float)( (a) * (a) )


extern double fmod(double,double);
extern double sqrt(double);
extern double fabs(double);
extern double atan2(double,double);
#endif

