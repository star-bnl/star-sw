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

#include "St_l3_Coordinate_Transformer.h"
#include "daqFormats.h"
#include "l3GeneralHeaders.h"
#include "FtfGeneral.h"
#include "gl3Track.h"
#include "gl3Hit.h"
#include "gl3Sector.h"
#include "FtfPara.h"

#ifndef GL3EVENT 
#define GL3EVENT 

#ifdef SL3ROOT
#include "Rtypes.h"
#else
#define ClassDef(a,b)
#endif


class gl3HistoContainer {
public:
   int nBytes ;
   int nHistos ;
   int buffer ;
};


class gl3Event {

public:
   gl3Event(int mxHits=500000, int mxTracks=20000 ):hit(0),track(0),busy(0){
      trackContainer = 0 ;
      trackIndex     = 0 ;
      hitProcessing  = 0 ;
      setup ( mxHits, mxTracks ) ;
   } ;
   ~gl3Event( ){ 
       if ( hit            != 0 ) delete []hit   ;  
       if ( track          != 0 ) delete []track ;  
       if ( trackContainer != 0 ) delete []trackContainer ;
       if ( trackIndex     != 0 ) delete []trackIndex     ;
   } ;


   short     getBusy  ( ) { return busy ; } ;
   gl3Track* getTrack ( int n ) {
      if ( n < 0 || n > nTracks ) {
         fprintf ( stderr, " %d track index out of range \n", n ) ; 
         return NULL ;
      }
      return &(track[n]) ;
   }
   gl3Hit* getHit ( int n ) {
      if ( n < 0 || n > nHits ) {
         fprintf ( stderr, " %d hit index out of range \n", n ) ; 
         return NULL ;
      }
      return &(hit[n]) ;
   }
   gl3Sector* getSector ( int n ) {
      if ( n < 0 || n > NSECTORS ) {
         fprintf ( stderr, " %d sector index out of range \n", n ) ; 
         return NULL ;
      }
      return &(sectorInfo[n]) ;
   }

   void addTracks ( short sector, int nTracks, local_track* track ) ;

   int getNTracks ( ) { return nTracks ; } ;
   int getNHits   ( ) { return nHits   ; } ;

   int  readEvent  ( int maxLength, char* buffer ) ;
   int  readSectorHits   ( char* buffer, int nSectorTracks ) ;
   int  readSectorTracks ( char* buffer ) ;
   int  resetEvent ( ) ;
   void setHitProcessing ( int hitPro ) { hitProcessing = hitPro ; } ;

   int fillTracks ( int maxBytes, char* buffer, unsigned int token ) ;
   //
   float          bField ;
   //
   int  setup ( int mHits=600000, int mTracks = 20000 ) ;
private:
   //
   int           hitProcessing ; // 0=does read hits
                                 // 1=reassigns trackId in hits to
                                 // pass that info downstream
                                 // 2=full hit unpacking for module use
   gl3Hit*          hit   ;
   gl3Track*        track ;
   int*             trackIndex ; // to keep track of relation ship between orig. tracks
   int              busy ;       // and final tracks
   int     maxTracks ;
   int     maxTracksSector ;
   int     nTracks ;
   int     maxHits ;
   int     nHits ;
   int     nMergedTracks ;
   //
   FtfPara          para ;
   FtfContainer*    trackContainer ;
   gl3Sector        sectorInfo[NSECTORS];

   ClassDef(gl3Event,1)
//
} ;
#endif

