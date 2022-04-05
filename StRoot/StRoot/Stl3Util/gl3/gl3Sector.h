//:>------------------------------------------------------------------
//: FILE:       gl3Sector.h
//: HISTORY:
//:              7jan2000 version 1.00
//:<------------------------------------------------------------------
#include <stdio.h>
#include <math.h>
#include "Stl3Util/gl3/gl3Track.h"
#include "Stl3Util/gl3/gl3Histo.h"
//#include "l3List.h"

#define toDeg 57.295779513

#ifndef GL3SECTOR
#define GL3SECTOR

class gl3Sector {
public:
   short    id ;
   short    filled ;
   unsigned int nHits ;
   unsigned int nTracks ;
   unsigned int cpuTime ;
   unsigned int realTime ;
   float    xVert ;
   float    yVert ;
   float    zVert ;
   float    rVert ;
   float    phiVert ;
//
   gl3Histo* hPt  ; 
   gl3Histo* hPsi ;
   gl3Histo* hEta ;
   gl3Histo* hR0  ;
   gl3Histo* hZ0  ; 
   gl3Histo* hPhi0 ;
   gl3Histo* hNHitsTrack ;
   gl3Histo* hNHitsSector ;
   gl3Histo* hNTracks ;
   gl3Histo* hRealTime ;
   gl3Histo* hCpuTime ;
   //
   void print ( ) {
      fprintf ( stdout, " filled %d nHits %d nTracks %d \n", filled, nHits, nTracks ) ;
      fprintf ( stdout, " Times: real %d (us) cpu %d (us) \n", realTime, cpuTime ) ; 
      fprintf ( stdout, " Vertex: (x,y,z)=(%f,%f,%f)\n", xVert, yVert, zVert ) ;
   };
};
#endif

