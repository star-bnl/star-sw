#ifndef FTFINDER
#define FTFINDER
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <iostream.h>

#include "FTF_general.h"
#include "FTF_Para.h"
#include "FTF_Hit.h"
#include "FTF_Track.h"
#include "FTF_Mc_Track.h"
#include "FTF_Volume.h"


class FTFinder {

public:
   FTFinder::FTFinder( ) ;
   float  FTF ( ) ;
   friend FTF_Track ;
	
   void   dEdx                    ( ) ;

   void   getTracks               ( ) ;
   void   mergePrimaryTracks      ( ) ;
   int    reset                   ( ) ;
   int    setConformalCoordinates ( ) ;
   int    setPointers             ( ) ;
   float  time                    ( ) ;
//
   void		printVols ( ) ;
   void		printRows ( ) ;
//
   int           n_hits     ;  
   FTF_Hit       *hit       ;  
   int           n_tracks   ; 
   FTF_Track     *track     ;  
   FTF_Para      para       ;
   int           max_tracks ;
   int           n_mc_tracks ;
   FTF_Mc_Track  *mc_track   ;

   VOLUME        *volume     ;
   ROW           *rowk       ;
   AREA          *track_area ;

   float init_time, total_time ;
private: 

   FTF_Track *current_track ;
    
} ;
#endif

