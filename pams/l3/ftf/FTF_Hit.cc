//:>------------------------------------------------------------------
//: FILE:       FTF_Hit.cpp
//: HISTORY:
//:             28oct1996 version 1.00
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: CLASS:       FTF_Hit
//: DESCRIPTION: Functions associated with this class
//: AUTHOR:      ppy - Pablo Yepes, yepes@physics.rice.edu
//:>------------------------------------------------------------------
#include <math.h>
#include <string.h>
#include <stdio.h>
#include "FTF_Hit.h"
#include "FTF_Track.h"

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//  
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void FTF_Hit::Print ( int point_level ) 
{
//--
//--     print hit info
//  

   if ( point_level > 9 ) 
      printf ( " \n hit ir iphi ieta   phi   eta      x      y     z" ) ;

   if ( fmod(point_level,10) > 0 )
        printf ( " \n %3d %2d %3d  %3d  %6.2f %5.2f  %6.2f %6.2f %6.2f ", 
                  id, i_r, i_phi, i_eta, phi*To_deg, eta, x, y, z ) ;
   int vhit ;
   if ( nxvhit != 0 ) vhit = nxvhit->id ;
   else vhit = -1 ;
   int rhit ;
   if ( nxrhit != 0 ) rhit = nxrhit->id ;
   else rhit = -1 ;
   int thit ;
   if ( nxthit != 0 ) thit = nxthit->id ;
   else thit = -1 ;
   int mhit ;
   if ( nxmhit != 0 ) mhit = nxmhit->id ;
   else mhit = -1 ;
   int ghit ;
   if ( nxghit != 0 ) ghit = nxghit->id ;
   else ghit = -1 ;

   if ( fmod(point_level,10) > 1 ) 
      printf ( "\n pointers:vol,row,tr,mtr,mirror (%4d,%4d,%4d,%4d,%4d) ",
		    vhit, rhit, thit, mhit,ghit ) ; 
   int tid ;
   if ( track != 0 ) tid = track->id ;
   else tid = -1 ;
   if ( fmod(point_level,10) > 2 )
      printf ( "\n Tracks  :reco, mc            (%4d,%4d) ",
                               tid, mc_track ) ;

/*   if ( fmod(point_level,10)  printf ( "\n  " ) ; */
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//    This function assigns this hit and all its 
//    mirror hits to a given track
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void FTF_Hit::Set_Status ( FTF_Track* this_track, FTF_Para *para ) {
//
//   Set the flag for this hit
//
	
	track = this_track ;
	if ( !para->ghost_flag ) return ;
//
//   Set the flag for mirror hits
//
	const int nmax = 100 ;
	int n = 0 ;
	for ( FTF_Hit* this_hit=nxghit ; 
	      this_hit != NULL && this_hit!=this && n<nmax ; 
		  this_hit=this_hit->nxghit ){ 
			  this_hit->track = this_track ;
			  n++ ;
	}
		  if ( n >= nmax ) printf ( " \n Set_Status: Going into an infinite loop " ) ;
}
/*
void FTF_Hit::Show ( int color ) {
	printf ( " \n You better include the graphic package gft " ) ;
}
*/
