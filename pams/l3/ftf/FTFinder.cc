//:>------------------------------------------------------------------
//: FILE:       FTFinder.cpp
//: HISTORY:
//:             28oct1996 version 1.00
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: CLASS:       FTFinder, steers track finding
//: DESCRIPTION: Functions associated with this class
//: AUTHOR:      ppy - Pablo Yepes, yepes@physics.rice.edu
//:>------------------------------------------------------------------
#include "FTFinder.h"

//*********************************************************************
//      Initializes the package
//*********************************************************************
FTFinder::FTFinder ( ) 
{
//
    hit        = 0 ;  
    track      = 0 ;
    mc_track   = 0 ;
    volume     = 0 ;
    rowk       = 0 ;
    track_area = 0 ;
}
//*********************************************************************
//      Steers the tracking 
//*********************************************************************
float FTFinder::FTF (  ) {  
//-----------------------------------------------------------------
//        Make sure there is something to work with
//------------------------------------------------------------------ 
    if ( n_hits <= 0 ) {
         printf ( "fft: Hit structure is empty \n " ) ;
         return 1 ;
    }
//	init_time = time ( );
	init_time = time ( );
//
//        General initialization 
//
    if ( para.init == 0 ) {
        if ( reset ( ) ) return 1 ;
    }
//
//      Event reset and set pointers
//
   if ( para.event_reset  && setPointers ( ) ) return 1 ;   
//
//      Build primary tracks now
//
   short i ;
   para.primaries = 1 ;
   for ( i = 0 ; i < para.n_pass_primaries ; i++ )
        getTracks ( );
//
//      Look for secondaries    
//
   para.primaries = 0 ;
   for ( i = 0 ; i < para.n_pass_secondaries ; i++ )
        getTracks ( );

//   if ( para.dEdx ) dEdx ( ) ;

   total_time = time ( ) ;
   if ( para.infoLevel > 0 )
      printf ( "ftf time: %7.3f \n ", total_time ) ;
	return total_time ;
} 
//********************************************************************
//     Calculates deposited Energy
//********************************************************************
void FTFinder::dEdx ( ) {
   for ( int i = 0 ; i<n_tracks ; i++ ){
	track[i].dEdx( ) ;
   }
}

//********************************************************************** 
//	Recontruct primary tracks 
//**********************************************************************
void FTFinder::getTracks ( ) {
   short   n_hit_segment ;
//
//     Set conformal coordinates if we are working with primaries
//
   if ( para.primaries ) {
      setConformalCoordinates ( ) ;
      para.min_n_hits_for_fit = 1 ;
      n_hit_segment   = (short)para.n_hit_segm ;
   }
   else {
      para.min_n_hits_for_fit = 2 ;
      n_hit_segment   = (short)max(para.n_hit_segm,3) ;
   }
//
//               Loop over rows   
//
   for ( int ir = para.n_rp - 1 ; ir>=para.mn_hit_trk ; ir--) {
//
//           Loop over hits in this particular row
//
      for ( FTF_Hit *first_hit = rowk[ir].first_hit ;
		    first_hit != 0 ;
	    	first_hit = first_hit->nxrhit ) {
//
//     Check hit was not used before
//
         if ( first_hit->track != 0  ) continue ;
//
//     One more track 
//
		 n_tracks++ ;
//
//
         if ( n_tracks > max_tracks ){
            printf("\n fft_track: Max nr tracks reached !") ;
            n_tracks = max_tracks  ;
            return ;
         }
//
//     Initialize variables before going into track hit loop
//
         FTF_Track *this_track = &track[n_tracks-1];
	      this_track->para      = &para ;
	      this_track->id        = n_tracks - 1 ;
         this_track->first_hit = this_track->last_hit = this_track->ref_hit = first_hit ;
#ifdef TRDEBUG
         this_track->Debug_New ( ) ;
#endif
//
//              Set fit parameters to zero
//
        this_track->Reset ( ) ;
//
//      Go into hit looking loop
//
		if ( this_track->Build_Track ( first_hit, volume ) ) {
//
//    Merge Tracks if requested
//
          if ( para.primaries &&
               para.merge_primaries == 1 &&
               this_track->Merge_Primary( track_area )  ) n_tracks-- ;
		}
	    else{
//
//      If track was not built delete candidate
//
            this_track->Delete_Candidate ( ) ;
		    n_tracks-- ;
		 }
//    
//       End loop over hits inside row               
//
      }
//       End loop over rows                           
//
   }
   return;
}
//********************************************************************
//
void FTFinder::mergePrimaryTracks ( ) {
//
//   Reset area keeping track pointers
// 
   memset ( track_area, 0, para.n_phit*para.n_etat*sizeof(AREA) ) ;  
//
//    Loop over tracks
//
   FTF_Track* currentTrack ;

   for ( int i = 0 ; i < n_tracks ; i++ ) {
      currentTrack = &(track[i]);
      if ( currentTrack->flag < 0 ) continue ;
//
//  reset link to following track
//
      currentTrack->nxatrk = 0 ;
//
//    Try to merge this track 
//    if track is not merged is added
//    to the track volume (area)
//
      if ( currentTrack->Merge_Primary ( track_area ) ) {
         currentTrack->flag = -1 ;
      }
   }
}
//********************************************************************
//      Resets program
//*********************************************************************
int FTFinder::reset (void)
{
   float phi_diff ;
//
//   Initialization flag in principle assume failure
//
   para.init = 0 ;
//----------------------------------------------------------------------------
//     Allocate volumes 
//---------------------------------------------------------------------------*/
   para.n_rp      = ( para.row_start - para.row_end ) / para.mod_row + 2 ;
   if ( para.n_rp < 1 ) {
       printf ( " \n =====> Error <===== " ) ;
       printf ( " \n Rows: start end %d % d ", para.row_start,  para.row_end ) ;
       return 1 ;
   }
   para.n_phip    = para.n_phi + 1 ;
   para.n_etap    = para.n_eta + 1 ;
   para.n_phietap = para.n_phip * para.n_etap ;
   if ( para.merge_primaries ) {
      para.n_phit = para.n_phi_track + 1 ;
      para.n_etat = para.n_eta_track + 1 ;
   }
//
//-->    Allocate volume memory
//
   if (volume != NULL) free ( (void *) volume ) ; 
#ifdef TRDEBUG
   printf("Allocating %d bytes of memory for volume\n",
               para->n_rp*para.n_phip*para.n_etap*sizeof(VOLUME));
#endif
   volume = (VOLUME *)malloc(para.n_rp*para.n_phip*para.n_etap*sizeof(VOLUME));
   if(volume == (VOLUME *)NULL) {
     printf ( "Problem with malloc... exiting\n" ) ;
     return 1 ;
   }
/*
 *-->   Allocate row memory
 */
   if (rowk != NULL) free ( (void *) rowk ) ;
#ifdef TRDEBUG
   printf("Allocating %d bytes of memory for rowk\n",
                              para->n_rp*sizeof(ROW));
#endif
   rowk = (ROW *)malloc(para.n_rp*sizeof(ROW));
   if ( rowk == ( ROW *)NULL) {
     printf ( "Problem with malloc... exiting\n" ) ;
     exit(0);
   }
/*
 *-->    Allocate track area memory
 */
   if ( para.merge_primaries ) {
      if (track_area != NULL) free ( (void *) track_area ) ;
#ifdef TRDEBUG
         printf("Allocating %d bytes of memory for track_area\n",
                       para.n_phip*para.n_etap*sizeof(AREA));
#endif
         track_area = (AREA *)malloc(para.n_phit*para.n_etat*sizeof(AREA));
         if(track_area == (AREA *)NULL) {
         printf ( "Problem with malloc... exiting\n" ) ;
         return 1 ;
      }
      else{
//
//   Check there is some memory allocated
//
         if ( track_area == 0 ){
            printf ( " You cannot repass with the merging option when \n " ) ; 
            printf ( " when you did not use it the first time         \n " ) ; 
            return 1 ;
         }
     }
   }
/*--------------------------------------------------------------------------
            Look whether the phi range is closed (< 5 degrees )
-------------------------------------------------------------------------- */
   phi_diff = para.phi_max - para.phi_min ;
   if ( phi_diff > 2. * Pi + 0.1 ) {
      printf ( " Wrong phi range %f7.2, %f7.2 ", 
                 para.phi_min*To_deg, para.phi_max*To_deg ) ;
      return 1 ;
   }
   if ( fabs(phi_diff-Twopi ) < Pi / 36. ) para.phi_closed = 1 ;
   else 
      para.phi_closed = 0 ;
/*--------------------------------------------------------------------------
            Calculate volume dimensions
-------------------------------------------------------------------------- */
   para.phi_slice   = (para.phi_max - para.phi_min)/para.n_phi ;
   para.eta_slice   = (para.eta_max - para.eta_min)/para.n_eta ;
/*--------------------------------------------------------------------------
            If needed calculate track area dimensions
-------------------------------------------------------------------------- */
   para.phi_slice_track   = (para.phi_max_track - para.phi_min_track)/para.n_phi_track ;
   para.eta_slice_track   = (para.eta_max_track - para.eta_min_track)/para.n_eta_track ;
//
//    Set vertex parameters
//
   if ( para.x_vertex != 0 || para.y_vertex != 0 ) { 
      para.r_vertex   = (float)sqrt (para.x_vertex*para.x_vertex +
	                                  para.y_vertex*para.y_vertex) ;
	  para.phi_vertex = (float)atan2(para.y_vertex,para.x_vertex);
   }
   else {
	  para.r_vertex   = 0.F ;
	  para.phi_vertex = 0.F ;
   }

   if ( para.dx_vertex != 0 || para.dy_vertex != 0 )
      para.xy_weight_vertex = 1.F / ((float)sqrt(para.dx_vertex*para.dx_vertex+
	                                              para.dy_vertex*para.dy_vertex) ) ;
   else para.xy_weight_vertex = 1.0F ;
//
//   Set # hits & tracks to zero
//
   n_hits   = 0 ;
   n_tracks = 0 ;
//
//    Set initialization flag to true
//
   para.init = 1 ;
   return 0 ;
}

//*********************************************************************
//	Set hit pointers
//*********************************************************************
int FTFinder::setConformalCoordinates ( )
{
/*-------------------------------------------------------------------------
        Loop over hits 
-------------------------------------------------------------------------*/
   FTF_Hit* this_hit ;
   float x, y, r2, inv_r2 ;
   for ( int ihit = 0 ; ihit<n_hits ; ihit++ )
   {
/*-------------------------------------------------------------------------
        Transform coordinates
-------------------------------------------------------------------------*/
      this_hit = &(hit[ihit]) ;

	  x             = this_hit->x - para.x_vertex ;
     y             = this_hit->y - para.y_vertex ;
	  r2            = x * x + y * y ;
     inv_r2        = 1.F / r2 ;

     this_hit->xp    =     x * inv_r2 ;
     this_hit->yp    =   - y * inv_r2 ;
	  this_hit->wxy   =   r2 * r2 /  ( square(para.xy_error_scale)
		                            * ( square(this_hit->dx) + square(this_hit->dy) ) ) ;
   } 

   return 0 ;
} 
//********************************************************************
//	Set hit pointers
//********************************************************************
int FTFinder::setPointers ( )
{
    int ihit, i_rr, i_phi, i_eta ;
    register int tmp_index;
    float r, r2, phi, eta ;
    FTF_Hit *this_hit ;
//
//   Set volumes to zero
//
   memset ( rowk,   0, para.n_rp*sizeof(ROW) ) ;
   int n = para.n_rp*para.n_etap*para.n_phip ;
   memset ( volume, 0, n*sizeof(VOLUME) ) ;
   if ( para.merge_primaries ) 
      memset ( track_area, 0, para.n_phit*para.n_etat*sizeof(AREA) ) ;  
/*-------------------------------------------------------------------------
        Loop over hits 
-------------------------------------------------------------------------*/

   for ( ihit = 0 ; ihit<n_hits ; ihit++ )
   {
/*-------------------------------------------------------------------------
        Check whether row is to be considered
-------------------------------------------------------------------------*/
      this_hit = &(hit[ihit]) ;
      i_rr = this_hit->i_r - para.row_end ;
      if ( fmod(i_rr,para.mod_row) != 0 ) continue ;

      if ( this_hit->i_r < para.row_end   ) continue ;
      if ( this_hit->i_r > para.row_start ) continue ;
/*
 *->    Get "renormalized" index
 */
      i_rr = i_rr / para.mod_row + 1 ;

/*-------------------------------------------------------------------------
        Transform coordinates
-------------------------------------------------------------------------*/
      r2            = this_hit->x * this_hit->x + this_hit->y * this_hit->y ;
      r             = (float)sqrt ( r2 ) ;
      phi           = (float)atan2(this_hit->y,this_hit->x) + para.phiShift ;
      if ( phi < 0 ) phi = phi + 2.F * Pi ;
      eta           = (float)seta(r,this_hit->z) ;

      if ( para.sz_fit_flag ) {
        this_hit->s  = 0.F ;
        this_hit->wz = (float)(1./ square ( para.sz_error_scale * this_hit->dz ));
      }

      this_hit->r   = r   ;
      this_hit->phi = phi ;
      this_hit->eta = eta ;
/*-------------------------------------------------------------------------
        Set pointers
-------------------------------------------------------------------------*/
      this_hit->nxvhit     = 
      this_hit->nxrhit     =
      this_hit->nxghit     =
      this_hit->nxthit     = 0 ;
      this_hit->track      = 0 ;


/*-------------------------------------------------------------------------
        Get phi index for hit
-------------------------------------------------------------------------*/

      i_phi = (int)( (this_hit->phi-para.phi_min)/para.phi_slice + 1);
      this_hit->i_phi = i_phi ; 
      if ( i_phi < 1 || i_phi > para.n_phi ) {
         if ( para.infoLevel > 0 ) {
	      printf ( " \n === > Hit %d has Phi = %f  ", this_hit->id, 
                                                     this_hit->phi ) ;
              printf ( " \n Phi index %d out of range ", i_phi ) ;
         }
	 continue ;
      } 

/*-------------------------------------------------------------------------
        Get eta index for hit
-------------------------------------------------------------------------*/

    i_eta = (int)((this_hit->eta - para.eta_min)/para.eta_slice + 1);
    this_hit->i_eta = i_eta ;
    if ( i_eta < 1 || i_eta > para.n_eta ) {
       if ( para.infoLevel > 0 ) {
          printf ( " \n === > Hit %d has Eta = %f  ", this_hit->id, 
                                                     this_hit->eta ) ;
          printf ( " \n Eta min/max %f %f ", para.eta_min, para.eta_max ) ;
          printf ( " \n Eta slice   %f    ", para.eta_slice ) ;
          printf ( " \n Eta index %d out of range ", i_eta ) ;	  
       }
       continue ;
    }


/* ------------------------------------------------------------------------- 
   Increase nr. of hits in small volume  WARNING! C-arrays go from 0
   Set Id of first hit in this vol. and link to next hit in previous
   hit in the same volume
-------------------------------------------------------------------------*/
      tmp_index = i_rr  * para.n_phietap + i_phi * para.n_etap + i_eta ;

      if (volume[tmp_index].first_hit == 0 ) 
	      volume[tmp_index].first_hit = this_hit ;
      else
         (volume[tmp_index].last_hit)->nxvhit = this_hit ;
      volume[tmp_index].last_hit = this_hit ;

/*-------------------------------------------------------------------------
     Set row pointers
-------------------------------------------------------------------------*/
      if ( rowk[i_rr].first_hit == NULL )
         rowk [i_rr].first_hit = this_hit ;
      else
         (rowk[i_rr].last_hit)->nxrhit = this_hit ;
      rowk[i_rr].last_hit = this_hit ;
   }
   return 0 ;
} 
//***********************************************************************
//     For timing
//***********************************************************************

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

static clock_t last_time ;
float FTFinder::time( void )

{
   clock_t now ;
   double  duration;

   now = clock();
   duration = (double)(now - last_time) / CLOCKS_PER_SEC;
   last_time = now ;

   return (float)duration ;
}
