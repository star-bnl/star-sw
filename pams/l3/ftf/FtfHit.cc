//:>------------------------------------------------------------------
//: FILE:       FtfHit.cpp
//: HISTORY:
//:             28oct1996 version 1.00
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: CLASS:       FtfHit
//: DESCRIPTION: Functions associated with this class
//: AUTHOR:      ppy - Pablo Yepes, yepes@physics.rice.edu
//:>------------------------------------------------------------------
#include <math.h>
#include <string.h>
#include <stdio.h>
#include "FtfHit.h"
#include "FtfTrack.h"

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//  
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void FtfHit::print ( int point_level ) 
{
//--
//--     print hit info
//  

   if ( point_level > 9 ) 
      printf ( " \n hit ir iphi ieta   phi   eta      x      y     z" ) ;

   if ( fmod((double)point_level,10.) > 0 )
        printf ( " \n %3d %2d %3d  %3d  %6.2f %5.2f  %6.2f %6.2f %6.2f ", 
                  id, row, phiIndex, etaIndex, phi*toDeg, eta, x, y, z ) ;
   int vhit ;
   if ( nextVolumeHit != 0 ) vhit = nextVolumeHit->id ;
   else vhit = -1 ;
   int rhit ;
   if ( nextRowHit != 0 ) rhit = nextRowHit->id ;
   else rhit = -1 ;
   int thit ;
   if ( nextTrackHit != 0 ) thit = nextTrackHit->id ;
   else thit = -1 ;
   int mhit ;
   if ( nextMcTrackHit != 0 ) mhit = nextMcTrackHit->id ;
   else mhit = -1 ;

   if ( fmod((double)point_level,10.) > 1 ) 
      printf ( "\n pointers:vol,row,tr,mtr,mirror (%4d,%4d,%4d,%4d) ",
		    vhit, rhit, thit, mhit ) ; 
   int tid ;
   if ( track != 0 ) tid = track->id ;
   else tid = -1 ;
   if ( fmod((double)point_level,10.) > 2 )
      printf ( "\n Tracks  :reco, mc            (%4d,%4d) ",
                               tid, mcTrackId ) ;

/*   if ( fmod((double)point_level,10.)  printf ( "\n  " ) ; */
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//    This function assigns this hit and all its 
//    mirror hits to a given track
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void FtfHit::setStatus ( FtfTrack* this_track, FtfPara *para ) {
//
//   Set the flag for this hit
//
   track = this_track ;
/*
   if ( !para->ghost_flag ) return ;
//
//   Set the flag for mirror hits
//
   const int nmax = 100 ;
   int n = 0 ;
   for ( FtfHit* this_hit=nxghit ; 
      this_hit != NULL && this_hit!=this && n<nmax ; 
	  this_hit=this_hit->nxghit ){ 
      this_hit->track = this_track ;
      n++ ;
   }
   if ( n >= nmax ) printf ( " \n Set_Status: Going into an infinite loop " ) ;
        */
}
/*
void FtfHit::Show ( int color ) {
	printf ( " \n You better include the graphic package gft " ) ;
}
*/
