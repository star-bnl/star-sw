#ifndef FTFGENERAL
#define FTFGENERAL
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
//
//    Some constrats 
//
const float toDeg =57.29577951F ;
const float pi    =3.141592654F ;
const float twoPi =2.F*pi ; 
const float piHalf = 0.5 * pi ;
//
//-->   Functions
//
#define min(a,b)    ( ( (a) < (b) ) ? (a) : (b) )
#define max(a,b)    ( ( (a) > (b) ) ? (a) : (b) )
#define seta(r,z)   (float)(3.0F * (z) / (fabs(z)+2.0F*(r)))
#define reta(eta,r) ((2.F*(r)*eta / ( 3 - fabs(eta)) )) 
#define sgn(a)      (float)( ( (a) > 0   ) ? (1) :(-1) )
#define square(a)   (float)( (a) * (a) )


extern double fmod(double,double);
extern double sqrt(double);
extern double fabs(double);
extern double atan2(double,double);
#endif

