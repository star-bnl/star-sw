#include <math.h>

#ifndef THL
#define THL
#include "l3Point.h"

extern double fmod(double,double);
extern double sqrt(double);
extern double fabs(double);
extern double atan2(double,double);
//
int hit2Cluster ( int nHits, l3Point* hit, int nBytes, int* buff ) ;
int cluster2Hit ( int nBytes, int* buff, int maxHit, l3Point* hit  ) ;
//
#endif
