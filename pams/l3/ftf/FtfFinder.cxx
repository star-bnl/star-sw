//:>------------------------------------------------------------------
//: FILE:       FtfFinder.cpp
//: HISTORY:
//:             28oct1996 version 1.00
//:             03jun1999 ppy para.fillTracks included. Merging only when tracks filled
//:             03jun1999 ppy add a function for real time, clock gives cpu time 
//:             06may1999 ppy getTracks returns 1 for error
//:             10aug1999 ppy nHitsForSegment set to at least 3 for
//:                           secondary search.
//:             23aug1999 ppy ClassImp added with ROOT flag
//:             21dec1999 ppy printf replaced by fprintf(stderr,...
//:             26jan2000 ppy malloc replaced with new, destructor function added
//:              1feb2000 ppy track id starting at 1
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: CLASS:       FtfFinder, steers track finding
//: DESCRIPTION: Functions associated with this class
//: AUTHOR:      ppy - Pablo Yepes, yepes@physics.rice.edu
//:>------------------------------------------------------------------
#include "FtfFinder.h"


#ifdef SL3ROOT
ClassImp(FtfFinder)
#endif


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
    nHitsOutOfRange = 0 ;
}
//*********************************************************************
//      Initializes the package
//*********************************************************************
FtfFinder::~FtfFinder ( ) 
{
//
    if ( mcTrack ) delete[] mcTrack ;
    if ( volume  ) delete[] volume  ;
    if ( rowk    ) delete[] rowk ;
    if ( trackArea ) delete[] trackArea ;
}
//*********************************************************************
//      Steers the tracking 
//*********************************************************************
double FtfFinder::process (  ) {  
//-----------------------------------------------------------------
//        Make sure there is something to work with
//------------------------------------------------------------------ 
    if ( nHits <= 0 ) {
        if ( para.infoLevel > 2 )         
           fprintf ( stderr, "fft: Hit structure is empty \n " ) ;
        return 1 ;
    }
//
    CpuTime ( );
    RealTime ( );
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
        if ( getTracks ( ) ) break ;
//
//      Look for secondaries    
//
   para.primaries = 0 ;
   for ( i = 0 ; i < para.nSecondaryPasses ; i++ )
        if ( getTracks ( ) ) break ;

//   if ( para.dEdx ) dEdx ( ) ;

   cpuTime  = CpuTime ( ) ;
   realTime = RealTime ( ) ;
#ifdef DEBUG
   if ( para.infoLevel > 0 )
      fprintf ( stderr, "FtfFinder::process: cpu %7.3f real %f7.2 \n", 
                cpuTime, realTime ) ;
#endif
   return cpuTime ;
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
int FtfFinder::getTracks ( ) {
//
//     Set conformal coordinates if we are working with primaries
//
   short nHitsSegment   = (short)para.nHitsForSegment;  
   if ( para.primaries ) {
      setConformalCoordinates ( ) ;
      para.minHitsForFit = 1 ;
      para.nHitsForSegment = max(2,nHitsSegment);
   }
   else {
      para.minHitsForFit = 2 ;
      para.nHitsForSegment = max(3,nHitsSegment);
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
            fprintf(stderr,"\n FtfFinder::getTracks: Max nr tracks reached !") ;
            nTracks = maxTracks  ;
            return 1 ;
         }
//
//     Initialize variables before going into track hit loop
//
         FtfTrack *thisTrack = &track[nTracks-1];
	 thisTrack->para     = &para ;
	 thisTrack->id       = nTracks ;
         thisTrack->firstHit = thisTrack->lastHit = thisTrack->refHit = firstHit ;
#ifdef TRDEBUG
         thisTrack->debugNew ( ) ;
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
                para.fillTracks &&
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
//
   para.nHitsForSegment = nHitsSegment ;  
//
   return 0 ;
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
   double phiDiff ;
//
//   Initialization flag in principle assume failure
//
   para.init = 0 ;
//----------------------------------------------------------------------------
//     Allocate volumes 
//---------------------------------------------------------------------------*/
   para.nRowsPlusOne = ( para.rowOuterMost - para.rowInnerMost ) / para.modRow + 2 ;
   if ( para.nRowsPlusOne < 1 ) {
       fprintf ( stderr, " =====> Error <===== \n" ) ;
       fprintf ( stderr, " Rows: Outer Most Inner Most %d % d \n", 
                          para.rowOuterMost,  para.rowInnerMost ) ;
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
// if (volume != NULL) free ( (void *) volume ) ; 
   if (volume != NULL) delete []volume; 
#ifdef TRDEBUG
   fprintf(stderr,"Allocating %d bytes of memory for volume\n",
               para.nRowsPlusOne*
               para.nPhiPlusOne*
               para.nEtaPlusOne*sizeof(VOLUME));
#endif
   int nVolumes = para.nRowsPlusOne*para.nPhiPlusOne *
                  para.nEtaPlusOne ;
   volume = new VOLUME[nVolumes];
   if(volume == NULL) {
     fprintf ( stderr, "Problem with memory allocation... exiting\n" ) ;
     return 1 ;
   }
// 
//      Allocate row memory
//
   if ( rowk != NULL ) delete[] rowk ;
#ifdef TRDEBUG
   fprintf( stderr, "Allocating %d bytes of memory for rowk\n",
                              para.nRowsPlusOne*sizeof(ROW));
#endif
   rowk = new ROW[para.nRowsPlusOne];
   if ( rowk == NULL) {
     fprintf ( stderr, "Problem with memory allocation... exiting\n" ) ;
     exit(0);
   }
//
//       Allocate track area memory
//
   if ( para.mergePrimaries ) {
      if (trackArea != NULL) delete []trackArea  ;
#ifdef TRDEBUG
         fprintf(stderr, "Allocating %d bytes of memory for track_area\n",
                       para.nPhiTrackPlusOne*
                       para.nEtaTrackPlusOne*sizeof(AREA));
#endif
         int nTrackVolumes = para.nPhiTrackPlusOne*
                             para.nEtaTrackPlusOne ;
         trackArea = new AREA[nTrackVolumes];
         if(trackArea == NULL) {
	    fprintf ( stderr, "Problem with memory allocation... exiting\n" ) ;
	    return 1 ;
	 }
	 else{
//
//   Check there is some memory allocated
//
         if ( trackArea == 0 ){
            fprintf ( stderr, "FtfFinder::reset: Merging option not available \n " ) ; 
            printf ( " when option was not used the first time         \n " ) ; 
            return 1 ;
         }
     }
   }
/*--------------------------------------------------------------------------
            Look whether the phi range is closed (< 5 degrees )
-------------------------------------------------------------------------- */
   phiDiff = para.phiMax - para.phiMin ;
   if ( phiDiff > 2. * pi + 0.1 ) {
      fprintf ( stderr, "FtfFinder::reset: Wrong phi range %f, %f ", 
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
      para.rVertex   = (double)sqrt (para.xVertex*para.xVertex +
	                            para.yVertex*para.yVertex) ;
      para.phiVertex = (double)atan2(para.yVertex,para.xVertex);
   }
   else {
      para.rVertex   = 0.F ;
      para.phiVertex = 0.F ;
   }

   if ( para.dxVertex != 0 || para.dyVertex != 0 )
      para.xyWeightVertex = 1.F / ((double)sqrt(para.dxVertex*para.dxVertex+
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
   double x, y, r2, invR2 ;
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
    double r, r2, phi, eta ;
    FtfHit *thisHit ;
//
    nHitsOutOfRange = 0 ;
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
      if ( fmod((double)localRow,(double)para.modRow) != 0 ) continue ;

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
      r             = (double)sqrt ( r2 ) ;
      phi           = (double)atan2(thisHit->y,thisHit->x) + para.phiShift ;
      if ( phi < 0 ) phi = phi + twoPi ;
      eta           = (double)seta(r,thisHit->z) ;

      if ( para.szFitFlag ) {
        thisHit->s  = 0.F ;
        thisHit->wz = (double)(1./ square ( para.szErrorScale * thisHit->dz ));
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

      thisHit->phiIndex = (int)( (thisHit->phi-para.phiMin)/para.phiSlice + 1.);
      if ( thisHit->phiIndex < 1 || thisHit->phiIndex > para.nPhi ) {
         if ( para.infoLevel > 2 ) {
	      fprintf ( stderr, " === > Hit %d has Phi = %f \n", (int)thisHit->id,    
                                                      thisHit->phi*toDeg ) ;
              fprintf ( stderr, " Phi index %d out of range \n", thisHit->phiIndex ) ;
         }
	 nHitsOutOfRange++ ;
	 continue ;
      } 

/*-------------------------------------------------------------------------
        Get eta index for hit
-------------------------------------------------------------------------*/

    thisHit->etaIndex = (int)((thisHit->eta - para.etaMin)/para.etaSlice + 1.);
    if ( thisHit->etaIndex < 1 || thisHit->etaIndex > para.nEta ) {
       if ( para.infoLevel > 2 ) {
          fprintf ( stderr, " \n === > Hit %d has Eta = %f  z %f ", (int)thisHit->id, 
                                                           thisHit->eta, thisHit->z ) ;
          fprintf ( stderr, " \n Eta min/max %f %f ", para.etaMin, para.etaMax ) ;
          fprintf ( stderr, " \n Eta slice   %f    ", para.etaSlice ) ;
          fprintf ( stderr, " \n Eta index %d out of range ", thisHit->etaIndex ) ;	  
       }
       nHitsOutOfRange++ ;
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
double FtfFinder::CpuTime( void )
{
   clock_t now ;
   double  duration;

   now = clock();
   duration = (double)(now - last_time) / CLOCKS_PER_SEC;
   last_time = now ;

   return (double)duration ;
}

//
#ifdef LINUX
static unsigned long lastRealTime ;
double FtfFinder::RealTime (void) {
  const long nClicks = 400000000 ;
  unsigned long eax, edx;
  asm volatile("rdtsc":"=a" (eax), "=d" (edx));
  double realTime = (double)(eax-lastRealTime)/ nClicks;
  lastRealTime = eax ;
  return realTime;
}
#else
double FtfFinder::RealTime (void) {
   return 1. ;
}
#endif
