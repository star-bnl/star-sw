#include "gl3Track.h"


//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//    Merges tracks
// 
//   values are combined using recipe form particle data booklet
//   Phys. Rev. D. 50(1994) 1180
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
int gl3Track::addTrack ( gl3Track* piece ){

   double w1   = 1./ ( dpt * dpt ) ;
   double w2   = 1./ ( piece->dpt * piece->dpt ) ;
   double wsum = w1 + w2 ;
   pt          = ( w1 * pt + w2 * piece->pt ) / wsum ;
   dpt         = 1./sqrt(wsum);

   w1    = 1./ ( dpsi * dpsi ) ;
   w2    = 1./ ( piece->dpsi * piece->dpsi ) ;
   wsum  = w1 + w2 ;
   psi   = ( w1 * psi + w2 * piece->psi ) / wsum ;
   dpsi  = 1./sqrt(wsum) ;

   w1    = 1./ ( dtanl * dtanl ) ;
   w2    = 1./ ( piece->dtanl * piece->dtanl ) ;
   wsum  = w1 + w2 ;
   tanl  = ( w1 * tanl + w2 * piece->tanl ) / wsum ;
   dtanl = 1./sqrt(wsum);

   w1   = 1./ ( dz0 * dz0 ) ;
   w2   = 1./ ( piece->dz0 * piece->dz0 ) ;
   wsum = w1 + w2 ;
   z0   = ( w1 * z0 + w2 * piece->z0 ) / wsum ;
   dz0  = 1./sqrt(wsum);

   if ( piece->innerMostRow < innerMostRow ) innerMostRow = piece->innerMostRow ;
   if ( piece->outerMostRow > outerMostRow ) outerMostRow = piece->outerMostRow ;

   nHits += piece->nHits ;
//
//   Add chi2's
//
   chi2[0] += piece->chi2[0] ;
   chi2[1] += piece->chi2[1] ;
//
   return 0 ;
}
 //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//    Merges tracks
//        returns track to which this tracks was added
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
gl3Track* gl3Track::merge ( FtfContainer *trackArea ){
   gl3Track*  track_merged ;
   register int  areaIndex ;
   int    i_phi, i_eta ;
   gl3Track *i_track ;
   int    ip, ie ;
   double  delta_psi ;
   //
   //   Get track area       
   //
   i_phi = (int)(( psi - getPara()->phiMinTrack ) / getPara()->phiSliceTrack + 1 );
   if ( i_phi < 0 ) {
      printf ( " Track phi index too low  %d \n", i_phi ) ;
      i_phi = 1 ;
   }
   if ( i_phi >= getPara()->nPhiTrackPlusOne ) {
      printf ( " Track phi index too high %d \n", i_phi ) ;
      i_phi = getPara()->nPhiTrack ;
   }
   //
   //     Now eta
   //
   i_eta = (int)(( eta - getPara()->etaMinTrack ) / getPara()->etaSliceTrack + 1 );
   if ( i_eta <= 0 ) {
      printf ( " Track eta index too low  %d \n", i_eta ) ;
      i_eta = 1 ;
   }
   if ( i_eta >= getPara()->nEtaTrackPlusOne ) {
      printf ( " Track eta index too high %d \n", i_eta ) ;
      i_eta = getPara()->nEtaTrack ;
   }
   //
   //     Loop around selected area
   //
   track_merged = 0 ;

   for ( ip = max(i_phi-1,1) ; ip < min(i_phi+2,getPara()->nPhiTrackPlusOne) ; ip++ ) {
      for ( ie = max(i_eta-1,1) ; ie < min(i_eta+2,getPara()->nEtaTrackPlusOne) ; ie++ ) {
	 areaIndex = ip * getPara()->nEtaTrackPlusOne + ie ;
	 //
	 //    Loop over tracks
	 //
	 for ( i_track = (gl3Track *)trackArea[areaIndex].first ; 
	       i_track != 0 && !track_merged ;
	       i_track = i_track->getNextTrack()  ) {
	    //
	    //    Reject track if it is not good
	    //
	    if ( i_track->flag < 0 ) continue ; 

	    //
	    // Compare both tracks
	    //
	    //   No overlapping tracks
	    short delta1 = i_track->outerMostRow - outerMostRow ;
	    short delta2 = i_track->innerMostRow - innerMostRow ;
	    if ( delta1 * delta2 <= 0 ) continue ;
	    //
	    //    Tracks close enough
	    //
	    if ( fabs(eta-i_track->eta) > getPara()->detaMerge ) continue ;
	    delta_psi = fabs(psi - i_track->psi) ;
	    if ( delta_psi > getPara()->dphiMerge && delta_psi < twoPi - getPara()->dphiMerge ) continue ;
	    //
	    //   Check radious difference
	    //
	    double dr0 = fabs(r0-i_track->r0) ;

	    if ( dr0 < getPara()->distanceMerge ) {
	       double dx0 = fabs(r0*cos(phi0)-i_track->r0*cos(i_track->phi0));
	       if ( dx0 > getPara()->distanceMerge ) continue ;
	       double dy0 = fabs(r0*sin(phi0)-i_track->r0*sin(i_track->phi0));
	       if ( dy0 > getPara()->distanceMerge ) continue ;
	       double dz0 = fabs(z0-i_track->z0);
	       if ( dz0 > getPara()->distanceMerge ) continue ;
	       //
	       i_track->addTrack ( this ) ;
	       //printf ( "merged track \n" ) ;
	       //i_track->Print(20);
	       track_merged = i_track ;
	       break ;
	    }
/*
   comment out this case for the moment
   it needs some more testing

	    else { // if tracks too far away in r0 extrapolate the one with smallest r0 

	       if ( r0 < i_track->r0 ) updateToRadius ( i_track->r0 ) ;
	       else i_track->updateToRadius ( r0 ) ;

	       printf ( "dr0 too large, trying something else \n" ) ;

	       double dx0 = fabs(r0*cos(phi0)-i_track->r0*cos(i_track->phi0));
	       if ( dx0 > getPara()->distanceMerge ) continue ;
	       double dy0 = fabs(r0*sin(phi0)-i_track->r0*sin(i_track->phi0));
	       if ( dy0 > getPara()->distanceMerge ) continue ;
	       double dz0 = fabs(z0-i_track->z0);
	       if ( dz0 > getPara()->distanceMerge ) continue ;
	       printf ( "tracks should be added \n" ) ;
	       //
	       i_track->addTrack ( this ) ;
	       //printf ( "merged track \n" ) ;
	       //i_track->Print(20);
	       track_merged = i_track ;
	       break ;
	    }
*/
	 }
#ifdef TRDEBUG
	 if ( getPara()->debugLevel > 1 )
	    printf ( " \n Track %d merge into %d ", this->id, i_track->id ) ;
#endif
      }
   }
//
//->  If track not matched add it
//
// printf ( "track_merged %d \n", track_merged ) ;
   if ( !track_merged ) {
      areaIndex = i_phi * getPara()->nEtaTrackPlusOne + i_eta ;
      if ( trackArea[areaIndex].first == 0 )
	 trackArea[areaIndex].first = 
	    trackArea[areaIndex].last = (void *)this  ;
      else {
	 ((gl3Track *)trackArea[areaIndex].last)->nextTrack = this ; 
	 trackArea[areaIndex].last = (void *)this ;
      }
   }
   return track_merged ;
}

