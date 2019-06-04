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
#include "l3CoordinateTransformer.h"


#ifndef TRG_VERSION
#define TRG_VERSION 0x32
#endif


#include "daqFormats.h"
#include "L3/L3Formats.h"



class gl3Hit {
public:
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
   int   set ( l3CoordinateTransformer* transformer,
               int sector, l3_cluster* cluster ) ;
   int setFlags(unsigned short in) { flags = in; return 0; };

   void setITPCHit(l3CoordinateTransformer* tranformer, int sector, int row, double pad, double tb, unsigned short charge, unsigned short flags);

   void  print ( ) ;
};
#endif
