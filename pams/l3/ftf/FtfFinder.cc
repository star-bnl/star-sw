//:>------------------------------------------------------------------
//: FILE:       FtfFinder.cpp
//: HISTORY:
//:             28oct1996 version 1.00
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: CLASS:       FtfFinder, steers track finding
//: DESCRIPTION: Functions associated with this class
//: AUTHOR:      ppy - Pablo Yepes, yepes@physics.rice.edu
//:>------------------------------------------------------------------
#include "FtfFinder.h"

//*********************************************************************
//      Initializes the package
//*********************************************************************
FtfFinder::FtfFinder ( ) 
{
//
    hit        = 0 ;  
    track      = 0 ;
    mcTrack    = 0 ;
    volume     = 0 ;
    rowk       = 0 ;
    trackArea  = 0 ;
}
//*********************************************************************
//      Steers the tracking 
//*********************************************************************
float FtfFinder::process (  ) {  
//-----------------------------------------------------------------
//        Make sure there is something to work with
//------------------------------------------------------------------ 
    if ( nHits <= 0 ) {
         printf ( "fft: Hit structure is empty \n " ) ;
         return 1 ;
    }
    initTime = time ( );
//
//        General initialization 
//
    if ( para.init == 0 ) {
        if ( reset ( ) ) return 1 ;
    }
//
//      Event reset and set pointers
//
   if ( para.eventReset  && setPointers ( ) ) return 1 ;   
//
//      Build primary tracks now
//
   short i ;
   para.primaries = 1 ;
   for ( i = 0 ; i < para.nPrimaryPasses ; i++ )
        getTracks ( );
//
//      Look for secondaries    
//
   para.primaries = 0 ;
   for ( i = 0 ; i < para.nSecondaryPasses ; i++ )
        getTracks ( );

//   if ( para.dEdx ) dEdx ( ) ;

   totalTime = time ( ) ;
// if ( para.infoLevel > 0 )
//    printf ( "ftf time: %7.3f \n ", totalTime ) ;
   return totalTime ;
} 
//********************************************************************
//     Calculates deposited Energy
//********************************************************************
void FtfFinder::dEdx ( ) {
   for ( int i = 0 ; i<nTracks ; i++ ){
	track[i].dEdx( ) ;
   }
}

//********************************************************************** 
//	Recontruct primary tracks 
//**********************************************************************
void FtfFinder::getTracks ( ) {
   short   nhitsSegment ;
//
//     Set conformal coordinates if we are working with primaries
//
   if ( para.primaries ) {
      setConformalCoordinates ( ) ;
      para.minHitsForFit = 1 ;
      nhitsSegment   = (short)para.nHitsForSegment;
   }
   else {
      para.minHitsForFit = 2 ;
      nhitsSegment   = (short)max(para.nHitsForSegment,3) ;
   }
//
//               Loop over rows   
//
   for ( int ir = para.nRowsPlusOne - 1 ; ir>=para.minHitsPerTrack ; ir--) {
//
//           Loop over hits in this particular row
//
      for ( FtfHit *firstHit = rowk[ir].firstHit ;
            firstHit != 0 ;
            firstHit = firstHit->nextRowHit ) {
//
//     Check hit was not used before
//
         if ( firstHit->track != 0  ) continue ;
//
//     One more track 
//
	 nTracks++ ;
//
//
         if ( nTracks > maxTracks ){
            printf("\n fft_track: Max nr tracks reached !") ;
            nTracks = maxTracks  ;
            return ;
         }
//
//     Initialize variables before going into track hit loop
//
         FtfTrack *thisTrack = &track[nTracks-1];
	 thisTrack->para     = &para ;
	 thisTrack->id       = nTracks - 1 ;
         thisTrack->firstHit = thisTrack->lastHit = thisTrack->refHit = firstHit ;
#ifdef TRDEBUG
         thisTrack->Debug_New ( ) ;
#endif
//
//              Set fit parameters to zero
//
        thisTrack->reset ( ) ;
//
//      Go into hit looking loop
//
        if ( thisTrack->buildTrack ( firstHit, volume ) ) {
//
//    Merge Tracks if requested
//
        if ( para.primaries &&
             para.mergePrimaries == 1 &&
             thisTrack->mergePrimary( trackArea )  ) nTracks-- ;
	}
       else{
//
//      If track was not built delete candidate
//
            thisTrack->deleteCandidate ( ) ;
	    nTracks-- ;
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
void FtfFinder::mergePrimaryTracks ( ) {
//
//   Reset area keeping track pointers
// 
   memset ( trackArea, 0, para.nPhiTrackPlusOne*para.nEtaTrackPlusOne*sizeof(AREA) ) ;  
//
//    Loop over tracks
//
// FtfTrack* currentTrack ;

   for ( int i = 0 ; i < nTracks ; i++ ) {
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
      if ( currentTrack->mergePrimary ( trackArea ) ) {
         currentTrack->flag = -1 ;
      }
   }
}
//********************************************************************
//      Resets program
//*********************************************************************
int FtfFinder::reset (void)
{
   float phiDiff ;
//
//   Initialization flag in principle assume failure
//
   para.init = 0 ;
//----------------------------------------------------------------------------
//     Allocate volumes 
//---------------------------------------------------------------------------*/
   para.nRowsPlusOne = ( para.rowOuterMost - para.rowInnerMost ) / para.modRow + 2 ;
   if ( para.nRowsPlusOne < 1 ) {
       printf ( " \n =====> Error <===== " ) ;
       printf ( " \n Rows: Outer Most Inner Most %d % d ", para.rowOuterMost,  para.rowInnerMost ) ;
       return 1 ;
   }
   para.nPhiPlusOne    = para.nPhi + 1 ;
   para.nEtaPlusOne    = para.nEta + 1 ;
   para.nPhiEtaPlusOne = para.nPhiPlusOne * para.nEtaPlusOne ;
   if ( para.mergePrimaries ) {
      para.nPhiTrackPlusOne = para.nPhiTrack + 1 ;
      para.nEtaTrackPlusOne = para.nEtaTrack + 1 ;
   }
//
//-->    Allocate volume memory
//
   if (volume != NULL) free ( (void *) volume ) ; 
#ifdef TRDEBUG
   printf("Allocating %d bytes of memory for volume\n",
               para->nRp*para.nPhip*para.nEtap*sizeof(VOLUME));
#endif
   volume = (VOLUME *)malloc(para.nRowsPlusOne*
                             para.nPhiPlusOne *
                             para.nEtaPlusOne*sizeof(VOLUME));
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
                              para->nRp*sizeof(ROW));
#endif
   rowk = (ROW *)malloc(para.nRowsPlusOne*sizeof(ROW));
   if ( rowk == ( ROW *)NULL) {
     printf ( "Problem with malloc... exiting\n" ) ;
     exit(0);
   }
/*
 *-->    Allocate track area memory
 */
   if ( para.mergePrimaries ) {
      if (trackArea != NULL) free ( (void *) trackArea ) ;
#ifdef TRDEBUG
         printf("Allocating %d bytes of memory for track_area\n",
                       para.n_phip*para.n_etap*sizeof(AREA));
#endif
         trackArea = (AREA *)malloc(para.nPhiTrackPlusOne*
                                    para.nEtaTrackPlusOne*sizeof(AREA));
         if(trackArea == (AREA *)NULL) {
         printf ( "Problem with malloc... exiting\n" ) ;
         return 1 ;
      }
      else{
//
//   Check there is some memory allocated
//
         if ( trackArea == 0 ){
            printf ( " You cannot repass with the merging option when \n " ) ; 
            printf ( " when you did not use it the first time         \n " ) ; 
            return 1 ;
         }
     }
   }
/*--------------------------------------------------------------------------
            Look whether the phi range is closed (< 5 degrees )
-------------------------------------------------------------------------- */
   phiDiff = para.phiMax - para.phiMin ;
   if ( phiDiff > 2. * pi + 0.1 ) {
      printf ( " Wrong phi range %f7.2, %f7.2 ", 
                 para.phiMin*toDeg, para.phiMax*toDeg ) ;
      return 1 ;
   }
   if ( fabs(phiDiff-twoPi ) < pi / 36. ) para.phiClosed = 1 ;
   else 
      para.phiClosed = 0 ;
/*--------------------------------------------------------------------------
            Calculate volume dimensions
-------------------------------------------------------------------------- */
   para.phiSlice   = (para.phiMax - para.phiMin)/para.nPhi ;
   para.etaSlice   = (para.etaMax - para.etaMin)/para.nEta ;
/*--------------------------------------------------------------------------
            If needed calculate track area dimensions
-------------------------------------------------------------------------- */
   para.phiSliceTrack   = (para.phiMaxTrack - para.phiMinTrack)/para.nPhiTrack ;
   para.etaSliceTrack   = (para.etaMaxTrack - para.etaMinTrack)/para.nEtaTrack ;
//
//    Set vertex parameters
//
   if ( para.xVertex != 0 || para.yVertex != 0 ) { 
      para.rVertex   = (float)sqrt (para.xVertex*para.xVertex +
	                            para.yVertex*para.yVertex) ;
      para.phiVertex = (float)atan2(para.yVertex,para.xVertex);
   }
   else {
      para.rVertex   = 0.F ;
      para.phiVertex = 0.F ;
   }

   if ( para.dxVertex != 0 || para.dyVertex != 0 )
      para.xyWeightVertex = 1.F / ((float)sqrt(para.dxVertex*para.dxVertex+
	                                       para.dyVertex*para.dyVertex) ) ;
   else para.xyWeightVertex = 1.0F ;
//
//   Set # hits & tracks to zero
//
// nHits   = 0 ;
// nTracks = 0 ;
//
//    Set initialization flag to true
//
   para.init = 1 ;
   return 0 ;
}

//*********************************************************************
//	Set hit pointers
//*********************************************************************
int FtfFinder::setConformalCoordinates ( )
{
/*-------------------------------------------------------------------------
        Loop over hits 
-------------------------------------------------------------------------*/
   FtfHit* thisHit ;
   float x, y, r2, invR2 ;
   for ( int ihit = 0 ; ihit<nHits ; ihit++ )
   {
/*-------------------------------------------------------------------------
        Transform coordinates
-------------------------------------------------------------------------*/
     thisHit = &(hit[ihit]) ;

     x            = thisHit->x - para.xVertex ;
     y            = thisHit->y - para.yVertex ;
     r2           = x * x + y * y ;
     invR2        = 1.F / r2 ;

     thisHit->xp    =     x * invR2 ;
     thisHit->yp    =   - y * invR2 ;
     thisHit->wxy   =   r2 * r2 /  ( square(para.xyErrorScale)
	                            * ( square(thisHit->dx) + square(thisHit->dy) ) ) ;
   } 

   return 0 ;
} 
//********************************************************************
//	Set hit pointers
//********************************************************************
int FtfFinder::setPointers ( )
{
    int ihit, localRow ;
    register int volumeIndex;
    float r, r2, phi, eta ;
    FtfHit *thisHit ;
//
//   Set volumes to zero
//
   memset ( rowk,   0, para.nRowsPlusOne*sizeof(ROW) ) ;
   int n = para.nRowsPlusOne*para.nEtaPlusOne*para.nPhiPlusOne ;
   memset ( volume, 0, n*sizeof(VOLUME) ) ;
   if ( para.mergePrimaries ) 
      memset ( trackArea, 0, para.nPhiTrackPlusOne*para.nEtaTrackPlusOne*sizeof(AREA) ) ;  
/*-------------------------------------------------------------------------
        Loop over hits 
-------------------------------------------------------------------------*/

   for ( ihit = 0 ; ihit<nHits ; ihit++ )
   {
/*-------------------------------------------------------------------------
        Check whether row is to be considered
-------------------------------------------------------------------------*/
      thisHit = &(hit[ihit]) ;
      localRow = thisHit->row - para.rowInnerMost ;
      if ( fmod(localRow,para.modRow) != 0 ) continue ;

      if ( thisHit->row < para.rowInnerMost ) continue ;
      if ( thisHit->row > para.rowOuterMost ) continue ;
/*
 *->    Get "renormalized" index
 */
      localRow = localRow / para.modRow + 1 ;

/*-------------------------------------------------------------------------
        Transform coordinates
-------------------------------------------------------------------------*/
      r2            = thisHit->x * thisHit->x + thisHit->y * thisHit->y ;
      r             = (float)sqrt ( r2 ) ;
      phi           = (float)atan2(thisHit->y,thisHit->x) + para.phiShift ;
      if ( phi < 0 ) phi = phi + twoPi ;
      eta           = (float)seta(r,(thisHit->z-para.zVertex)) ;

      if ( para.szFitFlag ) {
        thisHit->s  = 0.F ;
        thisHit->wz = (float)(1./ square ( para.szErrorScale * thisHit->dz ));
      }

      thisHit->r   = r   ;
      thisHit->phi = phi ;
      thisHit->eta = eta ;
/*-------------------------------------------------------------------------
        Set pointers
-------------------------------------------------------------------------*/
      thisHit->nextVolumeHit  = 
      thisHit->nextRowHit     = 0 ;
/*-------------------------------------------------------------------------
        Get phi index for hit
-------------------------------------------------------------------------*/

      thisHit->phiIndex = (int)( (thisHit->phi-para.phiMin)/para.phiSlice + 1);
      if ( thisHit->phiIndex < 1 || thisHit->phiIndex > para.nPhi ) {
         if ( para.infoLevel > 2 ) {
	      printf ( " \n === > Hit %d has Phi = %f  ", thisHit->id, 
                                                          thisHit->phi ) ;
              printf ( " \n Phi index %d out of range  ", thisHit->phiIndex ) ;
         }
	 continue ;
      } 

/*-------------------------------------------------------------------------
        Get eta index for hit
-------------------------------------------------------------------------*/

    thisHit->etaIndex = (int)((thisHit->eta - para.etaMin)/para.etaSlice + 1);
    if ( thisHit->etaIndex < 1 || thisHit->etaIndex > para.nEta ) {
       if ( para.infoLevel > 2 ) {
          printf ( " \n === > Hit %d has Eta = %f  ", thisHit->id, 
                                                     thisHit->eta ) ;
          printf ( " \n Eta min/max %f %f ", para.etaMin, para.etaMax ) ;
          printf ( " \n Eta slice   %f    ", para.etaSlice ) ;
          printf ( " \n Eta index %d out of range ", thisHit->etaIndex ) ;	  
       }

       continue ;
    }
//
//    Reset track assigment
//
    thisHit->nextTrackHit  = 0 ;
    thisHit->track         = 0 ;
/* ------------------------------------------------------------------------- 
   Increase nr. of hits in small volume  WARNING! C-arrays go from 0
   Set Id of first hit in this vol. and link to next hit in previous
   hit in the same volume
-------------------------------------------------------------------------*/
      volumeIndex = localRow  * para.nPhiEtaPlusOne + 
                    thisHit->phiIndex * para.nEtaPlusOne + thisHit->etaIndex ;

      if (volume[volumeIndex].firstHit == 0 ) 
	      volume[volumeIndex].firstHit = thisHit ;
      else
         (volume[volumeIndex].lastHit)->nextVolumeHit = thisHit ;
      volume[volumeIndex].lastHit = thisHit ;

/*-------------------------------------------------------------------------
     Set row pointers
-------------------------------------------------------------------------*/
      if ( rowk[localRow].firstHit == NULL )
         rowk [localRow].firstHit = thisHit ;
      else
         (rowk[localRow].lastHit)->nextRowHit = thisHit ;
      rowk[localRow].lastHit = thisHit ;
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
float FtfFinder::time( void )

{
   clock_t now ;
   double  duration;

   now = clock();
   duration = (double)(now - last_time) / CLOCKS_PER_SEC;
   last_time = now ;

   return (float)duration ;
}
