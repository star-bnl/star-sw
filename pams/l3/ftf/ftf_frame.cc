//:>------------------------------------------------------------------
//: FILE:       ftf_frame.cpp
//: HISTORY:
//:             28oct1996 version 1.00
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//:	FUNCTIONS  : frame_init, frame_end
//: DESCRIPTION: Initialize and end ftf from functions callable from c or fortran
//: AUTHOR:      ppy - Pablo Yepes, yepes@physics.rice.edu
//:>------------------------------------------------------------------
#include "FTFinder.h"
#include "TrackerFrame.h"

FTFinder      tracker ;
TrackerFrame  frame   ;


/*********************************************************************************
   Initializes the package
*********************************************************************************/
#ifndef NT
extern "C" void frame_init_ ( )
#else
extern "C" void frame_init ( )
#endif
{
    TrackerFrame *this_frame   = new TrackerFrame[1] ;
    FTFinder     *this_tracker = new FTFinder[1]    ;
    FTF_Para     *para         = new FTF_Para[1]    ;
	
    frame           = *this_frame ;	
    tracker         = *this_tracker ;

    tracker.para = para ;
    tracker.para->Set_Defaults ( ) ;
    frame.Init ( ) ;
}


#ifndef NT
extern "C" void frame_end_ ( ) 
#else
extern "C" void frame_end ( ) 
#endif
{
	frame.Done ( ) ;
}
