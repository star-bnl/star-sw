//:>------------------------------------------------------------------
//: FILE:       gl3Event.h
//: HISTORY:
//:              3dec1999 version 1.00
//:              3feb2000 add sector to addTracks of type 3
//:<------------------------------------------------------------------
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <unistd.h>
#include <fcntl.h>

#include "daqFormats.h"
#include "l3GeneralHeaders.h"
#include "FtfGeneral.h"
#include "gl3Tracks.h"
#include "gl3MergingTrack.h"
#include "gl3Sector.h"
#include "FtfPara.h"

#ifndef GL3EVENT 
#define GL3EVENT 


class gl3HistoContainer {
public:
   long nBytes ;
   long nHistos ;
   long buffer ;
};


class gl3Event {

public:
   gl3Event(int mxTracks=15000 ):track(0),busy(0){
      mergingTrack = 0 ;
      trackContainer = 0 ;
      setup ( mxTracks ) ;
   } ;
   ~gl3Event( ){ 
       if ( track          != 0 ) delete []track ;  
       if ( mergingTrack   != 0 ) delete []mergingTrack ;
       if ( trackContainer != 0 ) delete []trackContainer ;
   } ;


   short     getBusy  ( ) { return busy ; } ;
   gl3Track* getTrack ( int n ) {
      if ( n < 0 || n > nTracks ) {
         fprintf ( stderr, " %d track index out of range \n", n ) ; 
         return NULL ;
      }
      return &(track[n]) ;
   }
   gl3Sector* getSector ( int n ) {
      if ( n < 0 || n > NSECTORS ) {
         fprintf ( stderr, " %d sector index out of range \n", n ) ; 
         return NULL ;
      }
      return &(sectorInfo[n]) ;
   }

   void addTracks ( short sector, int nTracks, type1_track* track1 ) ;
   void addTracks ( short sector, int nTracks, type2_track* track2 ) ;
   void addTracks ( short sector, int nTracks, type3_track* track3 ) ;

   int getNTracks ( ) { return nTracks ; } ;

   int  readEvent  ( int maxLength, char* buffer ) ;
   int  readSector ( char* buffer ) ;
   int  resetEvent ( ) ;
   //
   float          bField ;
   //
   int  setup ( int mTracks = 20000, int maxMergingTracks=10000 ) ;
private:
 //
   gl3Track*        track ;
   int              busy ;
   int     maxTracks ;
   int     nTracks ;
   int     maxMergingTracks ;
   int     nMergingTracks ;
   gl3MergingTrack* mergingTrack ;
   //
   FtfPara          para ;
   FtfContainer*    trackContainer ;
   gl3Sector        sectorInfo[NSECTORS];
//
} ;
#endif

