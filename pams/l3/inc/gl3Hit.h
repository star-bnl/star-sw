//:>------------------------------------------------------------------
//: FILE:       gl3Hit.h
//: HISTORY:
//:              4apr2000 first version
//:<------------------------------------------------------------------
#include <stdio.h>
#include <math.h>
#include "daqFormats.h"
#include "L3Formats.h"
#include "sl3CoordinateTransform.h"

#ifndef GL3HIT
#define GL3HIT


class gl3Hit {
private:
   short rowSector ;
   char  mezzanninneCard ;
   char  readoutBoard ;
   unsigned short charge  ;
   unsigned short flags ;
   float x ;
   float y ;
   float z ;
public:
   int   trackId ;
   void* nextHit ;
   float getX ( ) { return x ; } ;
   float getY ( ) { return y ; } ;
   float getZ ( ) { return z ; } ;
   short getRowSector ( ) { return rowSector ; } ;
   int   getTrackId ( ) { return trackId ; } ;
   void* getNextHit ( ) { return nextHit ; } ;
   int   set ( int sector, l3_cluster* cluster ) ;
   void  print ( ) ;
};
#endif
