#include "Stl3Util/gl3/gl3Track.h"

#include "Stl3Util/base/FtfLog.h"

#include <stdlib.h>
//:>------------------------------------------------------------------
//: FILE:       gl3Track.cc
//: HISTORY:
//:            14 jul 2000 changing no overlaping tracks code in merging method
//:            27 jul 2000 add methods to drop hits
//:<------------------------------------------------------------------



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
   dpt         = 1./::sqrt(wsum);

   w1    = 1./ ( dpsi * dpsi ) ;
   w2    = 1./ ( piece->dpsi * piece->dpsi ) ;
   wsum  = w1 + w2 ;
   psi   = ( w1 * psi + w2 * piece->psi ) / wsum ;
   dpsi  = 1./::sqrt(wsum) ;

   w1    = 1./ ( dtanl * dtanl ) ;
   w2    = 1./ ( piece->dtanl * piece->dtanl ) ;
   wsum  = w1 + w2 ;
   tanl  = ( w1 * tanl + w2 * piece->tanl ) / wsum ;
   dtanl = 1./::sqrt(wsum);

   w1   = 1./ ( dz0 * dz0 ) ;
   w2   = 1./ ( piece->dz0 * piece->dz0 ) ;
   wsum = w1 + w2 ;
   z0   = ( w1 * z0 + w2 * piece->z0 ) / wsum ;
   dz0  = 1./::sqrt(wsum);

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
// 
//    input:
//            rest:   0= drops even rows, 1=drops odd rows, negative=no effect
//            rowMin: drops rows < rowMin
//            rowMax: drops rows > rowMin
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void gl3Track::dropHits ( int rest, int rowMin, int rowMax ) {

   gl3Hit* previousHit = 0  ;
   gl3Hit* cHit = (gl3Hit *)firstHit ;
   gl3Hit* deleteHit ;

// ftfLog ( "Start dropping routine \n" ) ;
// Print(31);
   int counter = 0 ;
   int newCounter = 0 ;
   while ( counter < nHits && cHit ) {
       if ( ( rest > 0 && cHit->getRowSector()%2 == rest )  ||
            cHit->getRowSector()%100 < rowMin || 
            cHit->getRowSector()%100 > rowMax ) {

          deleteHit = cHit ;
          cHit = (gl3Hit *)(cHit->nextHit) ;
          if ( previousHit ) previousHit->nextHit = cHit ;
// 
          deleteHit->trackId = 0 ;
          deleteHit->nextHit = 0 ;
      }
      else {
         if ( newCounter == 0 ) firstHit = cHit ;
         previousHit = cHit ;
         cHit = (gl3Hit *)(cHit->nextHit) ;
         newCounter++ ;
      }
      counter++ ; 
      lastHit = cHit ;
   }

   nHits = newCounter ;
   if ( nHits == 0 ) {
      firstHit = 0 ;
   }
// ftfLog ( "End dropping routine \n" ) ;
// Print(31);
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
//    ftfLog ( " Track phi index too low  %d \n", i_phi ) ;
      i_phi = 1 ;
   }
   if ( i_phi >= getPara()->nPhiTrackPlusOne ) {
//    ftfLog ( " Track phi index too high %d \n", i_phi ) ;
      i_phi = getPara()->nPhiTrack ;
   }
   //
   //     Now eta
   //
   i_eta = (int)(( eta - getPara()->etaMinTrack ) / getPara()->etaSliceTrack + 1 );
   if ( i_eta <= 0 ) {
//    ftfLog ( " Track eta index too low  %d \n", i_eta ) ;
      i_eta = 1 ;
   }
   if ( i_eta >= getPara()->nEtaTrackPlusOne ) {
//    ftfLog ( " Track eta index too high %d \n", i_eta ) ;
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
            //
            short p1 = i_track->innerMostRow ;
            short p2 = i_track->outerMostRow ; 
            short t1 = innerMostRow ;
            short t2 = outerMostRow ; 
            if ( p1 < t1 && p2 > t1 ) continue ;
            if ( p1 < t2 && p2 > t2 ) continue ;
            if ( p1 > t1 && p2 < t2 ) continue ;
            if ( t1 < p1 && t2 > p1 ) continue ;
            if ( t1 < p2 && t2 > p2 ) continue ;
            if ( t1 > p1 && t2 < p2 ) continue ;
            //
	    //    Tracks close enough
	    //
	    double dr0 = fabs(r0-i_track->r0) ;
//
//   If tracks close extrapolation is not needed
//
	    if ( dr0 < getPara()->distanceMerge ) { 
	       if ( fabs(eta-i_track->eta) > getPara()->detaMerge ) continue ;

	       delta_psi = fabs(psi - i_track->psi) ;
	       if ( delta_psi > getPara()->dphiMerge && delta_psi < twoPi - getPara()->dphiMerge ) continue ;
	    //
	    //   Check radious difference
	    //
	       double dx0 = fabs(r0*cos(phi0)-i_track->r0*cos(i_track->phi0));
	       if ( dx0 > getPara()->distanceMerge ) continue ;
	       double dy0 = fabs(r0*sin(phi0)-i_track->r0*sin(i_track->phi0));
	       if ( dy0 > getPara()->distanceMerge ) continue ;
	       double dz0 = fabs(z0-i_track->z0);
	       if ( dz0 > getPara()->distanceMerge ) continue ;
	    //
//             ftfLog ( "gl3Track: close tracks merged !!! \n " ) ;
//             i_track->Print(30);
//             Print(30);

	       i_track->addTrack ( this ) ;
	       track_merged = i_track ;
	       break ;
	    }
	    else { 
	    //
	    // if tracks too far away in r0 extrapolate the one with smallest r0 
	    //
	       gl3Track originalTrack ;
	       short    whichTrack = 0 ;
	       if ( r0 > i_track->r0 ) {
	          originalTrack = *this ;
		  whichTrack    = 1 ;
	          q *= -1 ;// charge needs to be changed to extrapolate backwards 
	          updateToRadius ( i_track->r0 ) ;
	          q *= -1 ;
	       }
	       else {
	          originalTrack = *i_track ;
	          i_track->q *= -1 ;// charge needs to be changed to extrapolate backwards
	          i_track->updateToRadius ( r0 ) ;
	          i_track->q *= -1 ;
	       }
	    //   double dx0 = fabs(r0*cos(phi0)-i_track->r0*cos(i_track->phi0)) ;
	    //   double dy0 = fabs(r0*sin(phi0)-i_track->r0*sin(i_track->phi0));
	    //   double dz0 = fabs(z0-i_track->z0);
	       if ( fabs(r0*cos(phi0)-i_track->r0*cos(i_track->phi0)) < getPara()->distanceMerge 
	        &&  fabs(r0*sin(phi0)-i_track->r0*sin(i_track->phi0)) < getPara()->distanceMerge 
	        &&  fabs(z0-i_track->z0) < getPara()->distanceMerge ) {
	       //
	       //   Coarse eta/phi cuts
	       //
	          if ( fabs(eta-i_track->eta) < 0.9 && fabs(psi - i_track->psi) < 0.9 
		    && fabs(psi - i_track->psi) < 5.9 ) { 
	       //
//                   ftfLog ( "gl3Track: far apart tracks merged !!! \n " ) ;
//                   i_track->Print(30);
//                   Print(30);

	             i_track->addTrack ( this ) ;
	             track_merged = i_track ;
	             break ;
	          }
	       } // end checking merging conditions
	       //
	       // if no merging put original track parameters
	       //
	       if ( whichTrack ) 
	          *this = originalTrack ;
	       else 
	          *i_track = originalTrack ;
	    }
	 }
#ifdef TRDEBUG
	 if ( getPara()->debugLevel > 1 )
	    ftfLog ( " \n Track %d merge into %d ", this->id, i_track->id ) ;
#endif
      }
   }
//
//->  If track not matched add it
//
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
//*************************************************************************
//   Prints one track information
//*************************************************************************
void gl3Track::Print ( int level )
{
   double pmom, pz;
/*
----->   Print info
*/
   if ( level > 9 ) {
      pz   = pt * tanl ;
      pmom = (double)sqrt ( pz * pz + pt * pt  ) ;
      ftfLog ( " =======> Track      %d  <======== \n", id ) ;
      ftfLog ( " p,  pt, q         %7.2f  %7.2f  %2d \n", pmom, pt, q ) ;
   }
   if ( level > 19 ) {
      ftfLog ( " r0,   z0,  nHits  %7.2f  %7.2f %d  \n  ", r0, z0, nHits ) ;
      ftfLog ( " phi0, psi, tanl   %7.2f  %7.2f %7.2f \n", phi0, psi, tanl ) ;
   }
   else ftfLog ( "\n " ) ;

   if ( level > 29 ) {
      ftfLog ( " chi2 (s,z)        %6.2e  %6.2e \n", chi2[0], chi2[1] ) ;
   }
   else ftfLog ( "\n " ) ;


   if ( fmod((double)level,10.) > 0 ) {
      ftfLog ( " *** Clusters in this track *** \n" ) ;
      for ( startLoop() ; done() ; nextHit()  ) {
        ((gl3Hit *)currentHit)->print ( ) ;
      }
   }
   ftfLog ( "\n " ) ;
}
