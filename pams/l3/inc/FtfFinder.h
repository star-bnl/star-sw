#ifndef FTFFINDER
#define FTFFINDER
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <iostream.h>

#include "FtfGeneral.h"
#include "FtfPara.h"
#include "FtfHit.h"
#include "FtfTrack.h"
#include "FtfMcTrack.h"
#include "FtfVolume.h"


class FtfFinder {

public:
   FtfFinder( ) ;
   float  FTF ( ) ;
   friend FtfTrack ;
	
	void   dEdx                    ( ) ;

	void   getTracks               ( ) ;
	void   mergePrimaryTracks      ( ) ;
	int    reset                   ( ) ;
	int    setConformalCoordinates ( ) ;
	int    setPointers             ( ) ;
	float  time                    ( ) ;


   void		printVols ( ) ;
	void		printRows ( ) ;

//
	int           n_hits     ;  
	FtfHit        *hit       ;  
	int           n_tracks   ; 
	FtfTrack      *track     ;  
	FtfPara       para       ;
	int           max_tracks ;
   int           n_mc_tracks ;
	FtfMcTrack    *mc_track   ;

	VOLUME        *volume     ;
   ROW           *rowk       ;
   AREA          *track_area ;

	float init_time, total_time ;
private: 

	FtfTrack      *current_track ;
    
} ;
#endif

