//:>------------------------------------------------------------------
//: FILE:       gl3Hit.h
//: HISTORY:
//:              4apr2000 first version
//:             28jun2000 add Coordinate Transformer class
//:<------------------------------------------------------------------
#ifndef GL3HIT
#define GL3HIT
#include <stdio.h>
#include <math.h>
#include "Stl3Util/base/St_l3_Coordinate_Transformer.h"
#include "daqFormats.h"
#include "L3Formats.h"



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
   unsigned short getCharge ( ) { return charge ; } ;
   unsigned short getFlags  ( ) { return flags  ; } ;
   int   getTrackId ( ) { return trackId ; } ;
   void* getNextHit ( ) { return nextHit ; } ;
   int   set ( St_l3_Coordinate_Transformer* transformer,
               int sector, l3_cluster* cluster ) ;
   void  print ( ) ;
};
#endif
