//:>------------------------------------------------------------------
//: FILE:       FtfHit.cxx
//: HISTORY:
//:             28oct1996 version 1.00
//:             23aug1999 ppy print format changed
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: CLASS:       FtfHit
//: DESCRIPTION: Functions associated with this class
//: AUTHOR:      ppy - Pablo Yepes, yepes@physics.rice.edu
//:>------------------------------------------------------------------
#include "FtfHit.h"

#include "FtfGeneral.h"

#include "rtsLog.h"

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//  
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void FtfHit::printLinks (  ) { print (11) ; } ; 
void FtfHit::printLinks ( int point_level ) 
{
//--
//--     print hit info
//  

   if ( point_level > 9 ) 
     LOG(NOTE, "hit ir iphi ieta   phi   eta      x      y     z\n" ) ;

   if ( fmod((double)point_level,10) > 0 )
       LOG(NOTE, "%3d %3d %3d  %3d  %6.2f %5.2f  %6.2f %6.2f %6.2f \n", 
	       (int)id, (int)row, (int)phiIndex, (int)etaIndex, 
	       phi*toDeg, eta, x, y, z ) ;
   int vhit ;
   if ( nextVolumeHit != 0 ) vhit = ((FtfHit *)nextVolumeHit)->id ;
   else vhit = -1 ;
   int rhit ;
   if ( nextRowHit != 0 ) rhit = ((FtfHit *)nextRowHit)->id ;
   else rhit = -1 ;
   int thit ;
   if ( nextTrackHit != 0 ) thit = ((FtfBaseHit *)nextTrackHit)->id ;
   else thit = -1 ;
   int mhit ;
   if ( nextMcTrackHit != 0 ) mhit = ((FtfBaseHit *)nextMcTrackHit)->id ;
   else mhit = -1 ;

   if ( fmod((double)point_level,10) > 1 ) 
      LOG(NOTE, "pointers:vol,row,tr,mtr,mirror (%4d,%4d,%4d,%4d)\n ",
		    vhit, rhit, thit, mhit ) ; 
   int tid = 0;
//   if ( track != 0 ) tid = track->id ;
//   else tid = -1 ;
   if ( fmod((double)point_level,10) > 2 )
      LOG(NOTE, "\n Tracks  :reco            (%4d) ", tid ) ;

/*   if ( fmod((double)point_level,10)  l3Log ( "\n  " ) ; */
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//    This function assigns this hit and all its 
//    mirror hits to a given track
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void FtfHit::setStatus ( FtfTrack* this_track ) {
//
//   Set the flag for this hit
//
   track = (FtfBaseTrack *)this_track ;
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
   if ( n >= nmax ) l3Log ( " \n Set_Status: Going into an infinite loop " ) ;
        */
}
/*
void FtfHit::Show ( int color ) {
	l3Log ( " \n You better include the graphic package gft " ) ;
}
*/
