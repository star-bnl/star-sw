//:>------------------------------------------------------------------
//: FILE:       gl3Event.h
//: HISTORY:
//:              3dec1999 version 1.00
//:<------------------------------------------------------------------
#include <stdio.h>
#include <math.h>
#include "daqFormats.h"
#include "l3GeneralHeaders.h"
#include "gl3Tracks.h"

#ifndef GL3EVENT 
#define GL3EVENT 


class gl3Sector {
public:
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
   void print ( ) {
      fprintf ( stdout, " filled %d nHits %d nTracks %d \n", filled, nHits, nTracks ) ;
      fprintf ( stdout, " Times: real %d (us) cpu %d (us) \n", realTime, cpuTime ) ; 
      fprintf ( stdout, " Vertex: (x,y,z)=(%f,%f,%f)\n", xVert, yVert, zVert ) ;
   }
};

class gl3Event {

public:
   gl3Event(int mxTracks=50000 ):track(0){
      setup ( mxTracks ) ;
   } ;
   ~gl3Event( ){ if ( track != 0 ) delete []track ;  } ;

   gl3Track* getTrack ( unsigned int n ) {
      if ( n < 0 || n > nTracks ) {
         fprintf ( stderr, " %d track index out of range \n", n ) ; 
         return NULL ;
      }
      return &(track[n]) ;
   }
//
   unsigned int maxTracks ;
   unsigned int nTracks ;

   void addTracks ( short sector, int nTracks, type1_track* track1 ) ;
   void addTracks ( short sector, int nTracks, type2_track* track2 ) ;
   void addTracks ( int nTracks, type3_track* track3 ) ;

   int  readEvent  ( int maxLength, char* buffer ) ;
   int  readSector ( int maxLength, char* buffer ) ;
   //
   gl3Sector sectorInfo[NSECTORS];
   float     bField ;
private:
   int  setup ( int mTracks = 50000 ) ;
   gl3Track* track ;
} ;
#endif

