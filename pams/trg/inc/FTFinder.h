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
    FTFinder( ) ;
    int    FTF ( ) ;
    friend FTF_Track ;
	
    void   dEdx                      ( ) ;
    int    Event_Reset               ( ) ;

    void   Get_Tracks                ( ) ;
    void   Merge_Primary_Tracks      ( ) ;
    int    Reset                     ( ) ;
    int    Set_Conformal_Coordinates ( ) ;
    int    Set_Pointers              ( ) ;
    float  Time                      ( ) ;


    void		print_vols ( ) ;
    void		print_rows ( ) ;
//
    int           n_hits  ;  
    FTF_Hit       *hit    ;  
    int           n_tracks; 
    FTF_Track     *track  ;  
    FTF_Para      *para   ;
    int           max_tracks ;
    int           n_mc_tracks ;
    FTF_Mc_Track  *mc_track ;

    VOLUME        *volume     ;
    ROW           *rowk       ;
    AREA          *track_area ;

    float init_time, load_time, trak_time, dedx_time, total_time ;
private: 
    FTF_Track *current_track ;
    
} ;
#endif

