//:>------------------------------------------------------------------
//: FILE:       FTF_Track.cpp
//: HISTORY:
//:             28oct1996 version 1.00
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: CLASS:       FTF_Track
//: DESCRIPTION: Functions associated with this class
//: AUTHOR:      ppy - Pablo Yepes, yepes@physics.rice.edu
//:>------------------------------------------------------------------
#include <memory.h>
#include <stdio.h>
#include <math.h>
#include "FTF_Hit.h"
#include "FTF_Track.h"
#include "FTF_Volume.h"

extern FTFinder     tracker ;

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//    Add hits to track
// Arguments:
//        this_hit:  hit pointer
//        way     :  >0 add at beginning, <0 at end
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void FTF_Track::Add ( FTF_Hit *this_hit, int way )
{
//
//      Increment # hits in this track
//
  n_hits++ ; 
//
//         Update pointers
//
  if ( way < 0 || n_hits == 1 ) {
     if ( n_hits > 1 ) last_hit->nxthit = this_hit ;
     last_hit = this_hit ;
  }
  else {
	 this_hit->nxthit = first_hit ; 
	 first_hit = this_hit ;
  }
//
//        Declare hit as used and fill chi2
//
  this_hit->Set_Status ( this, para ) ;
//
//    Check whether a fit update is needed
//
  if ( n_hits < para->min_n_hits_for_fit ) return ;
//
//    Include hit in xy fit parameter calculation
//
  
  s11_xy = s11_xy + this_hit->wxy ;
  s12_xy = s12_xy + this_hit->wxy * this_hit->xp ;
  s22_xy = s22_xy + this_hit->wxy * square(this_hit->xp) ;
  g1_xy  = g1_xy  + this_hit->wxy * this_hit->yp ;
  g2_xy  = g2_xy  + this_hit->wxy * this_hit->xp * this_hit->yp ;
  
 
  if ( n_hits > para->min_n_hits_for_fit  )
  {
     dd_xy  = s11_xy * s22_xy - square ( s12_xy ) ;
     a1_xy  = ( g1_xy * s22_xy - 
                g2_xy * s12_xy ) / dd_xy ;
     a2_xy  = ( g2_xy * s11_xy - 
                g1_xy * s12_xy ) / dd_xy ;
  }
//
//     Now in the sz plane
//
  if ( para->sz_fit_flag ) {
     s11_sz = s11_sz + this_hit->wz ;
     s12_sz = s12_sz + this_hit->wz * this_hit->s ;
     s22_sz = s22_sz + this_hit->wz * this_hit->s * this_hit->s ;
     g1_sz  = g1_sz  + this_hit->wz * this_hit->z ;
     g2_sz  = g2_sz  + this_hit->wz * this_hit->s * this_hit->z ;
  
	 if ( n_hits > para->min_n_hits_for_fit ) {
		
        dd_sz  = s11_sz * s22_sz -  s12_sz * s12_sz ;
#ifdef TRDEBUG
		if ( dd_sz != 0 ) {
#endif
           a1_sz  = ( g1_sz * s22_sz - 
                   g2_sz * s12_sz ) / dd_sz ;
           a2_sz  = ( g2_sz * s11_sz - 
                   g1_sz * s12_sz ) / dd_sz ;
#ifdef TRDEBUG
		}
		else
		{
			printf ( " \n Something strange going on " ) ;
			printf ( " \n Track %d ", id ) ;
		}
#endif
     }
  }
}
//****************************************************************************
//   Fill track information tables
//****************************************************************************
void FTF_Track::Add ( FTF_Track *piece ) 
{
//
//   Get cercle parameters
//
  s11_xy += piece->s11_xy  ;
  s12_xy += piece->s12_xy  ;
  s22_xy += piece->s22_xy  ;
  g1_xy  += piece->g1_xy   ;
  g2_xy  += piece->g2_xy   ;

  double dd_xy  =   s11_xy * s22_xy - square ( s12_xy ) ;
  double a1_xy  = ( g1_xy * s22_xy - g2_xy * s12_xy ) / dd_xy ;
  double a2_xy  = ( g2_xy * s11_xy - g1_xy * s12_xy ) / dd_xy ;
//
//     Now in the sz plane
//
  if ( para->sz_fit_flag ) {
     s11_sz += piece->s11_sz  ;
     s12_sz += piece->s12_sz  ;
     s22_sz += piece->s22_sz  ;
     g1_sz  += piece->g1_sz   ;
     g2_sz  += piece->g2_sz   ;

     double dd_sz  = s11_sz * s22_sz - square ( s12_sz ) ;
     double a1_sz  = ( g1_sz * s22_sz - g2_sz * s12_sz ) / dd_sz ;
     double a2_sz  = ( g2_sz * s11_sz - g1_sz * s12_sz ) / dd_sz ;
   }
//
//  Add space points to first track
//
    int counter ;
    if ( piece->first_hit->i_r < first_hit->i_r ){
	  counter = 0 ;
      last_hit->nxthit = piece->first_hit ;
      last_hit         = piece->last_hit ;
      for ( FTF_Hit *current_hit  = piece->first_hit ; 
                     current_hit != 0 && counter < piece->n_hits ;
                     current_hit  = current_hit->nxthit  ) {
        current_hit->track = this   ;
		counter++ ;
	  }
    }
	else {
	  counter = 0 ;
      for ( FTF_Hit *current_hit  = piece->first_hit ; 
                     current_hit != 0 && counter < piece->n_hits ;
                     current_hit  = current_hit->nxthit  ) {
        current_hit->track = this   ;
		counter++;
	  }

	  piece->last_hit->nxthit = first_hit ;
      first_hit               = piece->first_hit ;
      
	}
//
//
//
   n_hits  += piece->n_hits ;
   chi2[0] += piece->chi2[0] ;
   chi2[1] += piece->chi2[1] ;
//
//   Update track parameters
//
   Fill ( ) ;
//
//   Declare track 2 not to be used
//
   piece->flag    = -1 ;
}

//****************************************************************************
//   Control how the track gets built
//****************************************************************************
int FTF_Track::Build_Track ( FTF_Hit *first_hit, VOLUME *volume ) {
//
//   Add first hit to track
//
   Add ( first_hit, GO_DOWN ) ;
//
//    Try to build a segment first
//
   if ( !Segment ( volume, GO_DOWN ) ) return 0 ;
//
//    If segment build go for a real track with a fit
//
   int row_to_stop = para->row_end ;
   if ( !Follow ( volume, GO_DOWN, row_to_stop ) ) return 0 ;
//
//    Now to extent track the other direction if requested
//
   if ( para->go_backwards ) Follow ( volume, GO_UP, para->row_start ) ;

   Fill ( ) ;

   return 1 ;
}
//***************************************************************************
//   Calculates dEdx
//***************************************************************************
void FTF_Track::dEdx (  ){
   int i, j ;
   FTF_Hit *next_hit ;
   int n_truncate = max(1,
	            para->dEdx_n_truncate*n_hits/100) ;
   n_truncate = min(n_hits/2,n_truncate) ;
//
//   Define array to keep largest de's
//
   float *de = new float[n_truncate] ;
//
//    Reset
//
   dedx = 0.F ;
   memset ( de, 0, n_truncate*sizeof(float) ) ;
//
//
//
   for  ( next_hit = first_hit ; 
          next_hit != 0 ;
          next_hit = next_hit->nxthit) { 
    
      dedx += next_hit->q ;
	 
      if ( next_hit->q < de[0] ) continue ;

      for ( i = n_truncate-1 ; i>=0 ; i-- ){
         if ( next_hit->q > de[i] ){
            for ( j=0 ; j<i ; j++ ) de[j] = de[j+1] ;
            de[i] = next_hit->q ;
            break ;
	 }
      }
   }
//
//    Subtract largest de
//
   for ( i=0 ; i<n_truncate ; i++ ) dedx -= de[i] ;
   dedx = dedx / strack ;
/*   End track in required volume condition */
      
}
//*********************************************************************** 
//   Delete track candidate 
//***********************************************************************
void FTF_Track::Delete_Candidate(void)
{
  FTF_Hit *next_hit ;
  
#ifdef TRDEBUG
  Debug_Delete_Candidate ( ) ;
#endif
  while ( first_hit != 0 )
  {
    next_hit              = first_hit->nxthit;
    first_hit->nxthit     =  0 ;
    first_hit->chi2_xy    =
    first_hit->chi2_sz    =  
	first_hit->s          =  0.F ;

	first_hit->Set_Status ( 0, para ) ;
	first_hit = next_hit;
  }
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//    Fills track variables with or without fit
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void FTF_Track::Fill (  ) {
//
//   Get circle parameters
//
   double xc, yc ;
   double rc   = sqrt ( a2_xy * a2_xy + 1 ) / ( 2 * fabs(a1_xy) ) ;
   pt          = (float)(2.9979e-3 * para->bfield * rc );

   if ( pt > para->pt_min_helix_fit ) Fit_Helix ( ) ;
   else{
      if ( para->primaries ) Fill_Primary ( xc, yc, rc ) ;
      else
         Fill_Secondary ( xc, yc, rc ) ;
//
//    Get Errors
//
      if ( para->get_errors ) {
         Get_Errors_Circle_Fit (  (float)xc, (float)yc, (float)rc ) ;
         double det = s11_sz * s22_sz - s12_sz * s12_sz ;
         dtanl = (float) ( s11_sz / det );
         dz0   = (float) ( s22_sz / det );
      }
   }
}
//****************************************************************************     
//     Fill track information variables
//****************************************************************************
void FTF_Track::Fill_Primary (  double &xc, double &yc, double &rc  ) {
//
//   Get circle parameters
//
   double xcp = - a2_xy / ( 2. * a1_xy ) + para->x_vertex ;
   double ycp = - 1.   /  ( 2. * a1_xy ) + para->y_vertex ;

   xc = xcp + para->x_vertex ;
   yc = ycp + para->y_vertex ;
//
//   Get track parameters
//
   double angle_vertex  = atan2 ( -ycp, -xcp ) ;
   if ( angle_vertex < 0. ) angle_vertex = angle_vertex + 2. * Pi ;

   double dx_last    = last_hit->x - xc ;
   double dy_last    = last_hit->y - yc ;
   double angle_last = atan2 ( dy_last, dx_last ) ;
   if ( angle_last < 0. ) angle_last = angle_last + 2. * Pi ;
//
//       Get the rotation
//
   double d_angle = angle_last - angle_vertex ;

   if ( d_angle >  Pi ) d_angle =   d_angle - 2 * Pi  ;
   if ( d_angle < -Pi ) d_angle =   d_angle + 2 * Pi  ;

   q = ( ( d_angle < 0 ) ? 1 : -1 ) ;
   r0   = para->r_vertex ;
   phi0 = para->phi_vertex ;
   psi  = (float)(angle_vertex - q * 0.5F * Pi) ;
   if ( psi < 0        )  psi = (float)(psi + 2.F * Pi );
   if ( psi > 2. * Pi  )  psi = (float)(psi - 2.F * Pi );
//
//      Get z parameters if needed       
//
   if ( para->sz_fit_flag ){
      tanl = -(float)a2_sz ;
      z0   =  (float)(a1_sz + a2_sz * ( strack - rc * d_angle * q ) );
   }
   else{
      tanl = first_hit->z /
          (float)sqrt ( first_hit->x*first_hit->x + first_hit->y*first_hit->y ) ;
      z0      = 0.F ;
   }
//
//    Store some more track info
//
   eta     = seta(1.,tanl )   ;

}
//****************************************************************************
//   
//   Fill track information tables
//
//****************************************************************************
void FTF_Track::Fill_Secondary ( double &xc, double &yc, double &rc )
{
   xc = - a2_xy / ( 2. * a1_xy ) + ref_hit->x ;
   yc = - 1.   /  ( 2. * a1_xy ) + ref_hit->y ;
/*--------------------------------------------------------------------------
     Get angles for initial and final points
------------------------------------------------------------------------------*/
   double dx1    = first_hit->x - xc ;
   double dy1    = first_hit->y - yc ;
   double angle1 = atan2 ( dy1, dx1 ) ;
   if ( angle1 < 0. ) angle1 = angle1 + 2. * Pi ;

   double dx2    = last_hit->x - xc ;
   double dy2    = last_hit->y - yc ;
   double angle2 = atan2 ( dy2, dx2 ) ;
   if ( angle2 < 0. ) angle2 = angle2 + 2. * Pi ;
/*--------------------------------------------------------------------------
     Get the rotation
------------------------------------------------------------------------------*/
   double dangle = angle2 - angle1 ;
 //  if ( dangle >  Pi ) dangle =   dangle - 2. * Pi  ;
   if ( dangle < -Pi ) dangle =   dangle + 2. * Pi  ;

   q    = ( ( dangle > 0 ) ? 1 : -1 ) ;
   r0   = last_hit->r   ;
   phi0 = last_hit->phi ;
   psi  = (float)(angle2 - q * 0.5 * Pi );
   if ( psi < 0     ) psi = (float)(psi + 2 * Pi );
//
//      Get z parameters if needed       
//
   if ( para->sz_fit_flag ){
      tanl = -(float)a2_sz ;
      z0   =  (float)(a1_sz + a2_sz * strack  );
   }
   else{
      tanl = first_hit->z /
           (float)sqrt ( first_hit->x*first_hit->x + first_hit->y*first_hit->y ) ;
      z0      = 0.F ;
   }
//
//-->    Store some more track info
//
   eta     = seta(1., tanl )   ;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//        Adds hits to a track chosing the closest to fit
// Arguments:
//              volume:	      volume pointer
//              way   :       which way to procede in r (negative or positive)
//              row_to_stop:  row index where to stop
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int FTF_Track::Follow ( VOLUME *volume, int way, int ir_stop ) {

   FTF_Hit *next_hit ;
   
   if ( way < 0 )
      next_hit = last_hit ;
   else
      next_hit = first_hit ; 
#ifdef TRDEBUG
        if ( para->track_debug && para->debug_level >= 2 )
        printf ( " \n ===> Going into Track extension <=== " );
#endif
//
//     Define variables to keep total chi2
//
   float chi2_xy = chi2[0] ;
   float chi2_sz = chi2[1] ;
   
//
//    Loop as long a a hit is found and the segment
//    is shorter than n_hit_segm
//
   while ( way * next_hit->i_r < way * ir_stop ) {
//
//      Select next hit
//
	  chi2[0] = para->chi2_hit_cut ;

      next_hit = Seek_Next_Hit ( volume, next_hit, way*para->dr_track, USE_FOLLOW ) ;

#ifdef TRDEBUG
      if ( para.track_debug && para->debug_level >= 1 ){
         if ( next_hit != 0 ){
            printf ( " Follow : Search succesful, hit selected %d ", next_hit->id );
		    next_hit->Show ( para->color_track ) ;
         }
         else{
            printf ( " Follow : Search unsuccesful " );
            if ( chi2[0]+chi2[1] > para->chi2_hit_cut )
               printf ( " hit chi2 %f larger than cut %f ", chi2[0]+chi2[1], 
                                                para->chi2_hit_cut ) ;
         }
         Debug_Ask () ;
      }
#endif
//
//    Stop if nothing found
//
      if ( next_hit == 0 ) break ;
//
//   Keep total chi2
//
      float lchi2_xy = chi2[0]-chi2[1] ;
      chi2_xy += lchi2_xy ;
      next_hit->chi2_xy = lchi2_xy ;
//
//   if sz fit update track length
//
      if ( para->sz_fit_flag  ) {
         strack = next_hit->s ;
         chi2_sz += chi2[1]  ;
         next_hit->chi2_sz = chi2[1] ;
	  }
//
//     Add hit to track
//
	  Add ( next_hit, way ) ;

   } // End while
//
//    Check # hits
//
   if ( n_hits < para->mn_hit_trk ) return 0 ; 
//
//   Store track chi2
//
   chi2[0] = chi2_xy ;
   chi2[1] = chi2_sz ;
//
//        Check total chi2
//
   float normalized_chi2 = (chi2[0]+chi2[1])/n_hits ;
   if ( normalized_chi2 > para->chi2_track_cut ) return 0 ;
//
//  Otherwise go for it
//
//   Fill ( ) ;
#ifdef TRDEBUG
   Debug_Fill ( ) ;
#endif
//
   return 1 ;
}
/*******************************************************************************
        Reconstructs tracks
*********************************************************************************/
int FTF_Track::Follow_Hit_Selection ( FTF_Hit *base_hit, FTF_Hit *candidate_hit ){
//
	double lchi2_xy, lchi2_sz, lchi2 ;
	double slocal, deta, dphi ;
	double dx, dy, dxy, dsz, temp ;
//
//           Check delta eta 
//
   if ( base_hit->dz < 1000. && candidate_hit->dz < 1000 )
      deta = fabs((base_hit->eta)-(candidate_hit->eta)) ;

   if ( deta > para->deta ) return 0 ; 
     else deta = 0.F ;
//
//           Check delta phi
//
   dphi = fabs((base_hit->phi)-(candidate_hit->phi)) ;
   if ( dphi > para->dphi && dphi < Twopi-para->dphi ) return 0 ;
#ifdef TRDEBUG
   if ( para->debug_level >= 3 ) Debug_Follow_Candidate ( candidate_hit );
#endif
//
//      If looking for secondaries calculate conformal coordinates
//
   if ( para->primaries == 0 ){
      float xx = candidate_hit->x - ref_hit->x ;
      float yy = candidate_hit->y - ref_hit->y ;
      float rr = xx * xx + yy * yy ;
      candidate_hit->xp =   xx / rr ;
      candidate_hit->yp = - yy / rr ;

      candidate_hit->wxy  = rr * rr /
                        ( square(para->xy_error_scale)  *
                        ( square(candidate_hit->dx) + square(candidate_hit->dy) ) ) ;
   }
//
//      Calculate distance in x and y
//
   temp = (a2_xy * candidate_hit->xp - candidate_hit->yp + a1_xy) ;
   dxy  = temp * temp / ( a2_xy * a2_xy + 1.F ) ;
//
//    Calculate chi2
//
   lchi2_xy = (dxy * candidate_hit->wxy) ;
//
//      Now in the sz plane
//
   if ( para->sz_fit_flag ){
//
//        Get "s" and calculate distance hit-line
//
      dx     = base_hit->x - candidate_hit->x ;
      dy     = base_hit->y - candidate_hit->y ;
      slocal = strack + sqrt ( dx * dx + dy * dy ) ;

      temp = (a2_sz * slocal - candidate_hit->z + a1_sz) ;
      dsz  = temp * temp / ( a2_sz * a2_sz + 1 ) ;
//
//              Calculate chi2
//
      lchi2_sz = dsz * candidate_hit->wz ;

	  lchi2 = ( lchi2_xy + lchi2_sz ) ;
   } 
   else {
      lchi2    = lchi2_xy ;
	  lchi2_sz = 0.F ;
   }
#ifdef TRDEBUG
   if ( para->debug_level >= 2 ) 
	  Debug_Follow_Success ( (float)dxy, (float)dsz, (float)lchi2_xy, 
                             (float)lchi2_sz, chi2[0]+chi2[1], candidate_hit ) ;
#endif
//
//         Check whether the chi2 square is better than previous one
//
   if ( lchi2 < chi2[0] ) {
      chi2[0]       = (float)lchi2    ;
      chi2[1]       = (float)lchi2_sz ;
      
      if ( para->sz_fit_flag  ) candidate_hit->s = (float)slocal ;
//
//       if a good chi2 is found let's stop here
//
      if ( lchi2 < para->chi2_hit_good ) return 2 ;

      return 1 ;
   }
//
//     Return the selected hit
//
   return 0 ;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//    Merges tracks
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int FTF_Track::Merge_Primary ( AREA *track_area ){
   short  track_merged ;
   register int  tmp_index ;
   int    i_phi, i_eta ;
   FTF_Track *i_track ;
   int    ip, ie ;
   float  delta_psi ;
//-
//   Get track area       
//
   i_phi = (int)(( psi - para->phi_min_track ) / para->phi_slice_track + 1 );
   if ( i_phi < 0 ) {
       if ( para->infoLevel ) printf ( " Track phi index too low  %d \n", i_phi ) ;
       i_phi = 1 ;
   }
   if ( i_phi >= para->n_phit ) {
       if ( para->infoLevel) printf ( " Track phi index too high %d \n", i_phi ) ;
       i_phi = para->n_phit - 1 ;
   }
//
//     Now eta
//
   i_eta = (int)(( eta - para->eta_min_track ) / para->eta_slice_track + 1 );
   if ( i_eta <= 0 ) {
       if ( para->infoLevel ) printf ( " Track eta index too low  %d \n", i_eta ) ;
       i_eta = 1 ;
   }
   if ( i_eta >= para->n_etat ) {
       if ( para->infoLevel ) printf ( " Track eta index too high %d \n", i_eta ) ;
       i_eta = para->n_etat - 1 ;
   }
//
//     Loop around selected area
//
   track_merged = 0 ;
   for ( ip = max(i_phi-1,1) ; ip < min(i_phi+2,para->n_phit) ; ip++ ) {
      for ( ie = max(i_eta-1,1) ; ie < min(i_eta+2,para->n_etat) ; ie++ ) {
         tmp_index = ip * para->n_etat + ie ;
//
//    Loop over tracks
//
         for ( i_track = track_area[tmp_index].fst_track ; 
               i_track != 0 ;
               i_track = i_track->nxatrk  ) {
//
//    Reject track if it is not good
//
         if ( i_track->flag < 0 ) continue ; 
//
// Compare both tracks
//
//   No overlapping tracks
			short delta1 = i_track->first_hit->i_r - first_hit->i_r ;
			short delta2 = i_track->last_hit->i_r - last_hit->i_r ;
			if ( delta1 * delta2 <= 0 ) continue ;
//
//    Tracks close enough
//
            if ( fabs(eta-i_track->eta) > para->deta_merge ) continue ;
            delta_psi = (float)fabs(psi - i_track->psi) ;
            if ( delta_psi > para->dphi_merge && delta_psi < 2 * Pi - para->dphi_merge ) continue ;

            i_track->Add ( this ) ;
#ifdef TRDEBUG
			if ( para->debug_level > 1 )
                  printf ( " \n Track %d merge into %d ", this->id, i_track->id ) ;
#endif
            track_merged = 1 ;
            break ;
         }
      }
   }
//
//->  If track not matched add it
//
   if ( track_merged == 0 ) {
      tmp_index = i_phi * para->n_etat + i_eta ;
      if ( track_area[tmp_index].fst_track == 0 )
         track_area[tmp_index].fst_track = 
         track_area[tmp_index].lst_track = this  ;
      else {
         track_area[tmp_index].lst_track->nxatrk = this ; 
         track_area[tmp_index].lst_track = this ;
      }
   }
   return track_merged ;
}
/************************************************************************* 
	Recontruct primary tracks 
*************************************************************************/
void FTF_Track::Reset (void)
{
/*----------------------------------------------------------------------
                Set fit parameters to zero
----------------------------------------------------------------------*/

  flag     = para->primaries ;
  n_hits   = 0 ;
  s11_xy   = 
  s12_xy   = 
  s22_xy   = 
  g1_xy    = 
  g2_xy    = 
  chi2[0]  = 0.F ;
  nxatrk   = 0 ;
  if ( para->sz_fit_flag ) 
  {
     s11_sz =
     s12_sz =
     s22_sz =
     g1_sz  =
     g2_sz  =
     chi2[1]  = 
     strack         = 0.F ;
  }
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//     Function to look for next hit
// Input:	volume:         Volume pointer
//          base_hit:       Last point in track
//          n_r_steps:      How many rows search and which way (up or down)
//		    which_function: Function to be used to decide whether the hit is good
// Returns:	Selected hit
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
FTF_Hit *FTF_Track::Seek_Next_Hit ( VOLUME  *volume, 
								    FTF_Hit *base_hit,
									int     n_r_steps,
									int which_function ) {
#define N_LOOP 9 
   int loop_eta[N_LOOP] = { 0, 0, 0, 1, 1, 1,-1,-1,-1 } ;
   int loop_phi[N_LOOP] = { 0,-1, 1, 0,-1, 1, 0,-1, 1 };


   int ir, irp, ipp, itp, k;
   register int tmp_index ; 
   int result ;
   
//-------------------------------------------------------------------------------
//     Calculate limits on the volume loop
//-----------------------------------------------------------------------------*/
   int initial_ir, way ;
   if ( n_r_steps < 0 ) {
      initial_ir = max(1, (base_hit->i_r - para->row_end)/para->mod_row);
      n_r_steps  = min(initial_ir,-n_r_steps ) ;
	  way        = -1 ;
   }
   else {
	  initial_ir = max(1, (base_hit->i_r - para->row_end + 2)/para->mod_row);
	  n_r_steps  = min((para->row_start-initial_ir+1),n_r_steps) ;
	  way = 1 ;
   }

   
   FTF_Hit *selected_hit  = 0 ;
//
//      Loop over modules
//
   for ( k=0; k< N_LOOP; k++){ 
         ipp = base_hit->i_phi + loop_phi[k];
//
//--   Gymnastics if phi is closed
//
      if ( ipp < 1 ) {
         if ( para->phi_closed )
            ipp = para->n_phi + ipp ;
         else
            continue ;
      }
      else if ( ipp > para->n_phi ) {
         if ( para->phi_closed )
            ipp = ipp - para->n_phi ;
         else
            continue ;
      }
//--
//--     Now get eta index
//
      itp = base_hit->i_eta + loop_eta[k];
      if ( itp <     1      ) continue ;
      if ( itp > para->n_eta ) continue ;

      for ( ir = 0 ; ir < n_r_steps ; ir++ ){
         irp = initial_ir + way * ir ;
//
//       Now loop over hits in each volume 
//
         tmp_index = irp   * para->n_phietap + ipp * para->n_etap + itp ;
         for ( FTF_Hit *candidate_hit = volume[tmp_index].first_hit ; 
               candidate_hit != 0 ;
               candidate_hit = candidate_hit->nxvhit ){
#ifdef TRDEBUG
             Debug_in_Volume ( base_hit, candidate_hit ) ;
#endif
//----------------------------------------------------------------------------
//         Check whether the hit was used before
//--------------------------------------------------------------------------*/
             if ( candidate_hit->track != 0 ) continue ;
//--------------------------------------------------------------------------
//         If first points, just choose the closest hit
//-------------------------------------------------------------------------- */
			 if ( which_function == USE_SEGMENT ) 
			    result = Segment_Hit_Selection ( base_hit, candidate_hit ) ;
			 else 
				result = Follow_Hit_Selection  ( base_hit, candidate_hit ) ;
//
//     Check result
//
			 if ( result > 0 ) {
			    selected_hit = candidate_hit ;
                if ( result ==2  ) goto found ; 
			 }
//
//       End hit loop  
//
         }
//
//     End row loop      
//
      }
//
//   End volume loop inside cone      
//
   }
found: ;

   return selected_hit ;
}
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//   Forms segments
//   Arguments:
//             volume     :    volume pointer
//             way        :    whether to go to negative or positive ir
//             row_to_stop:    row index where to stop
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int FTF_Track::Segment( VOLUME *volume, int way ){
//
//   Define some variables
//
	float dx, dy, rr ;
	FTF_Hit* next_hit ;
//
//   Check which way to go
//
	if ( way < 0 ) 
	   next_hit	= last_hit ;
	else
	   next_hit = first_hit ;
#ifdef TRDEBUG
   if ( para->track_debug && para->debug_level >= 4 )
        printf ( " \n **** Trying to form segment **** " );
#endif
//
//    Loop as long a a hit is found and the segment
//    is shorter than n_hit_segm
//
   while ( next_hit != 0 && n_hits < para->n_hit_segm ) {

	  chi2[0] = para->max_dis_segm ; ;

      next_hit = Seek_Next_Hit ( volume, next_hit, way*para->dr_segm, USE_SEGMENT ) ;

#ifdef TRDEBUG
      if ( para->track_debug && para->debug_level > 0 ) {
         if ( next_hit != 0 ) {
            printf ( " \n SEGMENT: Search succesful, hit %d selected ",next_hit->id );
	        next_hit->Show ( para->color_track ) ;
      }
      else
         printf ( " \n SEGMENT: Search unsuccesful " );
         Debug_Ask () ;
      }
#endif
//
//     If sz fit update s
//
      if ( next_hit != 0 ){
//
//   Calculate track length if sz plane considered
//
	     if ( para->sz_fit_flag  ){
            dx = next_hit->x - last_hit->x ;
            dy = next_hit->y - last_hit->y ;
            strack          += (float)sqrt ( dx * dx + dy * dy ) ;
            next_hit->s      = strack ;
         }
//
//   Calculate conformal coordinates
//
         if ( para->primaries == 0 ){
            rr = square ( ref_hit->x - next_hit->x ) +
                 square ( ref_hit->y - next_hit->y ) ;

            next_hit->xp    =   ( next_hit->x - ref_hit->x ) / rr ;
            next_hit->yp    = - ( next_hit->y - ref_hit->y ) / rr ;
            next_hit->wxy   = rr * rr / ( square(para->xy_error_scale)  *
                                          square(next_hit->dx) + square(next_hit->dy) ) ;
         }
//
//     Add hit to track
//
	     Add ( next_hit, way ) ;
      }
   } // End while ( last_hit ...
//
//    If number of hits is as expected return 1 
//
   if ( n_hits == para->n_hit_segm )
      return 1 ;
   else
	  return 0 ;
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//     Routine to look for segments.
//	 Arguments:
//	 base_hit:       Hit from which track is being extrapolated
//   candidate_hit:  Hit being examined as a candidate to which extend track
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int FTF_Track::Segment_Hit_Selection ( FTF_Hit *base_hit, FTF_Hit *candidate_hit ){
   
   float dx, dy, dr, d3, dangle ;
   float dphi, deta ;
   float   angle ;
//
//   select hit with the
//   the smallest value of d3 (defined below)
//
   dphi  = (float)fabs((base_hit->phi) - (candidate_hit->phi)) ; 
   if ( dphi > Pi ) dphi = (float)fabs( Twopi - dphi ) ;
   if ( dphi > para->dphi && dphi < Twopi -para->dphi ) return 0 ;
//
//    Make sure we want to look at the difference in eta
//
   if ( base_hit->dz < 1000. && candidate_hit->dz < 1000. ){
        deta  = (float)fabs((base_hit->eta) - (candidate_hit->eta)) ; 
        if ( deta > para->deta ) return 0 ;
   }
   else deta = 0.F ;

   dr    = (float)fabs(base_hit->i_r - candidate_hit->i_r);
   d3    = (float)(To_deg * dr * ( dphi  + deta ) ) ;
//
//     If initial segment is longer than 2 store angle info in 
//     a1_xy and a1_sz
//
   if ( para->n_hit_segm > 2 && n_hits-1 < para->n_hit_segm ) {
	  dx = candidate_hit->x - base_hit->x ;
      dy = candidate_hit->y - base_hit->y ;
      angle = (float)atan2 ( dy, dx ) ;
      if ( angle < 0  ) angle = angle + Twopi ;
      last_xy_angle = angle ;
   }
#ifdef TRDEBUG
   if ( para->track_debug && para->debug_level >= 3 ) {
      printf ( " \n dr,dphi,deta  %7.2f %7.2f %7.2f ",dr,dphi,deta ) ;
      printf ( " \n 3d  distance  %7.2f ", d3 );
      printf ( " \n Min distance  %7.2f ", chi2[0] );
      if ( d3 < chi2[0] )
         printf ( " \n Best point, keep it !!! " );  
      else{
         printf ( "\n Worse than previous, reject !! " );
         candidate_hit->Show ( para->color_transparent );
      }
      Debug_Ask() ;
   }
#endif
   if ( d3 < chi2[0] ) {
//
//   For second hit onwards check the difference in angle 
//   between the last two track segments
      if ( n_hits > 1 ) {
	     dx     = candidate_hit->x - base_hit->x ;
         dy     = candidate_hit->y - base_hit->y ;
         angle  = (float)atan2 ( dy, dx ) ;
         if ( angle < 0  ) angle = angle + Twopi ;
	     dangle = (float)fabs ( last_xy_angle - angle );
	     last_xy_angle = angle ;
         if ( dangle > para->segment_max_angle ) return 0 ;
	  }
//
//    Check whether this is the "closest" hit
//
      chi2[0]          = d3 ;
      if ( d3 < para->good_distance ) return 2 ;
	  return 1 ;
   }
//
//    If hit does not fulfill criterai return 0
//
   return 0 ;
}
#ifdef TRDEBUG
//*****************************************************************************  
//    Ask for a character to keep going
//******************************************************************************/
void FTF_Track::Debug_Ask (void) 
{
      char cc;
	  
      printf ( "\n stop(s), continue (any other key) " );
      cc = getchar();
      if ( cc == 's' ) para->track_debug = 0 ;
	  if ( cc == '1' ) para->debug_level = 1 ;
	  if ( cc == '2' ) para->debug_level = 2 ;
	  if ( cc == '3' ) para->debug_level = 3 ;
	  if ( cc == '4' ) para->debug_level = 4 ;
	  if ( cc == '5' ) para->debug_level = 5 ;
	  if ( cc == '6' ) para->debug_level = 6 ;
	  if ( cc == '7' ) para->debug_level = 7 ;

	  printf ( " \n Debug Level %d ", para->track_debug ) ;
		  
	 
}
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//    Debug Delete Candidate
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void FTF_Track::Debug_Delete_Candidate(void)
{
  if ( para->track_debug == 0 || para->debug_level < 1 ) return ;
  
  for ( start_loop() ; done() ; next_hit() ) {
	  current_hit->Show ( para->color_back ) ;
  }
  printf ( "\n ==> Track %d has %d hits <==   "
                           ,id, n_hits );
  printf ( "\n -- Minimum is %d,  delete it -- ",para->mn_hit_trk  );
//  Print ( 31 ) ;
  Debug_Ask () ;
}
/*****************************************************************************  
     Fill track information tables
******************************************************************************/
void FTF_Track::Debug_Fill (  )
{
   if ( para->track_debug && para->debug_level >= 1 ) {
      printf ( "\n ===> Track %d added  <=== ",id+1 );
      Print ( 31 ) ;
   }
}
/*****************************************************************************
        Reconstructs tracks
******************************************************************************/
void FTF_Track::Debug_Follow_Candidate ( FTF_Hit* candidate_hit )
{
  if ( !para->track_debug || para->debug_level >= 4 ) return ;
//
//    Show the whole track and fit
//
  for ( start_loop() ; done() ; next_hit() ) {
	  current_hit->Show ( para->color_track ) ;
  }
  candidate_hit->Show ( para->color_candidate );
//
//        Print relevant information
//
  printf ( " \n ===> Extension in Follow <===" ) ;
 // Print ( 31 ) ;

  printf ( " \n Try hit %d  ", candidate_hit->id ) ;
  candidate_hit->Print ( 1 ) ;
//
//     If the hit already used say it and forget about it
//
  if ( candidate_hit->track != 0 )
  {
      printf ( " \n hit %d used in track %d ",
                    candidate_hit->id, id );
      Debug_Ask () ;
      candidate_hit->Show ( para->color_candidate ) ;
      candidate_hit->Print ( 3 ) ;
  }
}
/*******************************************************************************
        Reconstructs tracks
*********************************************************************************/
void FTF_Track::Debug_Follow_Success ( float dxy,      float dsz, float lchi2_xy, 
                                       float  lchi2_sz, float chi2_min,
                                       FTF_Hit *candidate_hit ) {
//
//     Check whether track needs to be debugged
//
   if ( !para->track_debug     ) return ;
   if (  para->debug_level < 2 ) return ;
//
//      Show first level of info
//
   float lchi2 = lchi2_xy + lchi2_sz ;
   
   printf ( " \n ------------------------------------- " ) ;
   if ( lchi2 < chi2_min ){
          printf ( " \n %f Best Chi2, keep point !!! ", lchi2 );
          if ( lchi2 < para->chi2_hit_good ){
              printf ( "\n This Chi2 is better than the good cut %f ",
                              lchi2, para->chi2_hit_good );
              printf ( "Stop search !!! " );
		  }
   }
   else{
      printf ( "\n Hit %d worse than previous, forget it !! ", candidate_hit->id );
      candidate_hit->Show ( para->color_track ) ;
   }
   
   
   printf ( " \n ------------------------------------- " ) ;
//
//   Show second level of info
//
   if ( para->debug_level > 2 ) {
	  printf ( "\n dis_xy dis_sz   %7.2e %7.2e ", dxy, dsz );
	  printf ( "\n Error xy   sz   %7.2e %7.2e ",  
                       candidate_hit->wxy, candidate_hit->wz );
      printf ( "\n xy:a1,a2;sz:a1,a2  %7.2f %7.2f %7.2f %7.2f ",
                                       a1_xy, a2_xy, a1_sz, a2_sz );
      printf ( "\n ch2:xy sz tot min  %7.2f %7.2f %7.2f %7.2f ", 
                                        lchi2_xy,lchi2_sz, lchi2, chi2_min );
   }
   Debug_Ask() ;
   candidate_hit->Show ( para->color_transparent ) ;
}
/*********************************************************************************
     Routine to look for segments.
     Segments are track starting chains
*******************************************************************************/
void FTF_Track::Debug_in_Volume ( FTF_Hit *base_hit, FTF_Hit *candidate_hit )
{

   if ( para->track_debug && para->debug_level >= 2 ) {
/*----------------------------------------------------------------------------
      Show the whole segment
----------------------------------------------------------------------------*/
	  for ( start_loop() ; done() ; next_hit() ) {
	     current_hit->Show ( para->color_track ) ;
      }
      
      candidate_hit->Show ( para->color_candidate ) ;
/*----------------------------------------------------------------------------
        Print relevant information
----------------------------------------------------------------------------*/
      if ( n_hits > para->n_hit_segm+1 ) Print ( 31 ) ;

      printf ( " \n Try hit %d  ", candidate_hit->id ) ; 
      candidate_hit->Print ( 1 ) ;
/*----------------------------------------------------------------------------
        If the hit already used say it and forget about it
----------------------------------------------------------------------------*/
      if ( candidate_hit->track != 0 ) {
         printf ( " \n hit %d used in track %d ", 
                              candidate_hit->id, id+1 );
         candidate_hit->Show ( 0 );
      }
      else {
         float dphi  = (float)fabs(base_hit->phi - candidate_hit->phi) ;
         float deta ;
         if ( base_hit->dz < 1000 && candidate_hit->dz < 1000 )
            deta  = (float)fabs(base_hit->eta - candidate_hit->eta) ;
         else
            deta  = 0.F ;

         if ( dphi > para->dphi )
            printf ( "Hits too far apart in phi: %f \n ", dphi ) ;
         if ( deta > para->deta )
            printf ( "Hits too far apart in eta: %f \n ", deta ) ;
      }
      Debug_Ask () ;
   }
}
/*****************************************************************************
     Fill track information tables
******************************************************************************/
void FTF_Track::Debug_New (  )
{

  if ( first_hit->id == para->debug_hit ) para->track_debug = 1 ;
  if ( para->track_debug && para->debug_level >= 1 )
  {
	 printf ( "\n ================================================ " );
     printf ( "\n Starting track %d from point %d ", id, first_hit->id );
	 printf ( "\n ================================================ " );

     first_hit->Show ( para->color_track ) ;
     Debug_Ask () ;
   }
}
#endif
