/*:>-------------------------------------------------------------------
**: FILE:       ftfTpc.cc

**: HISTORY:
**:   
**:  
**:<------------------------------------------------------------------*/
#include "PAM.h"
#include "ftfTpc.h"
#include "tcl_tphit.h"
#include "tpt_track.h"
#include "FtfFinder.h"

FtfFinder      tracker ;

extern "C" void ftfSetParameters ( SL3TPCPARA_ST* para ) ;

typedef FtfHit* PHit ; // type is hit pointer 

extern "C" long type_of_call ftfTpc_(
  TABLE_HEAD_ST     *para_h,       SL3TPCPARA_ST    *para,         
  TABLE_HEAD_ST     *tphit_h,      TCL_TPHIT_ST     *tphit,        
  TABLE_HEAD_ST     *tptrack_h,    TPT_TRACK_ST     *tptrack,   
  TABLE_HEAD_ST     *monitor_h,    SL3MONITOR_ST    *monitor )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    ftfTpc_
**: DESCRIPTION: Prepares data and calls fast tracking   
**:
**: 
**: AUTHOR:     ppy - Pablo P. Yepes, yepes@physics.rice.edu  
**: ARGUMENTS:
**:       IN:
**:        tcl_tphit      - TPC Space Points
**:      OUT:
**:        tpt_track      - TPC Tracks
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/
  float toDeg = 57.29577951F ;
//
//    Set parameters
//
  tracker.para.setDefaults ( ) ;
  ftfSetParameters ( para ) ;
//
//    Reset tracker
//
  tracker.reset ( ) ;
//
//   Check there is something coming in
//
  if ( tphit_h->nok < 1 ) {
     printf ( " \n rft: tphit table is empty " ) ;
     return STAFCV_BAD ;
  }
//
//    Check min hits per track
//
  if ( tracker.para.minHitsPerTrack < 3 ) {
     printf ( " \n Minimum # hits per track %d ", tracker.para.minHitsPerTrack ) ;
     return STAFCV_BAD ;
  }
//
//     Allocate memory 
//  
  int nSectors       = 0 ;
  int maxTracks      = (int)tptrack_h->maxlen ;
  tracker.track      = new FtfTrack[maxTracks] ;
  tracker.maxTracks  = maxTracks ;
  PHit* pHit ;
  long* nSectorHits ;
  long* maxSectorHits ;
  short sector ;
//
//    If event is not to be split
//    deal with all hits at once
//
  if ( para->FirstSector > para->LastSector ) {
     pHit        = new PHit[1] ;
     nSectorHits = new long[1] ;
     maxSectorHits = new long[1] ;
     pHit[0] = new FtfHit[tphit_h->nok] ;

     for ( int i = 0 ; i < tphit_h->nok ; i++ ) {
        (pHit[0])[i].id  = i ;
        tphit[i].track   = 0 ;
        (pHit[0])[i].row = (short)fmod(tphit[i].row,100)   ;
        if ( tphit[i].row == 4600 ) (pHit[0])[i].row = 46 ;
        if ( tphit[i].row == 4600 ) tphit[i].row = 46 ;
//      if ( (pHit[0])[i].row > 40   ) ((pHit[0])[i].row)++ ;
        (pHit[0])[i].x   = tphit[i].x ;
        (pHit[0])[i].y   = tphit[i].y ;
        (pHit[0])[i].z   = tphit[i].z ;
        (pHit[0])[i].dx  = tphit[i].dx ;
        (pHit[0])[i].dy  = tphit[i].dy ;
        (pHit[0])[i].dz  = tphit[i].dz ;
     }
     nSectors       = 1 ;
     nSectorHits[0] = tphit_h->nok ;
     maxSectorHits[0] = tphit_h->nok ;
  }
  else {
//
//   Get # sectors and make sure it makes sense
//
     nSectors = para->LastSector - para->FirstSector + 1 ;
     if ( nSectors > 24 ) {
        printf ( " \n ftf: Too many sectors %d ", nSectors ) ;
        return STAFCV_BAD ;
     }
//  
//      Allocate sector hit pointers
//
     pHit          = new PHit[nSectors] ;
     nSectorHits   = new long[nSectors] ;
     maxSectorHits = new long[nSectors] ;
//
//     Set hit counters to zero
//
     for ( int is = 0 ; is < nSectors ; is++ ) maxSectorHits[is] = 0 ;
//
//     Count hits in each sector
//
     int i, j, sectorIndex ;
     for ( i = 0 ; i < tphit_h->nok ; i++ ) {
        tphit[i].track = 0 ;
        sector = (short)(tphit[i].row/100) ;
        if ( sector < 13 && tphit[i].z < 0 ) {
           if ( sector == 12 ) sector = 24 ;
           else sector = 24 - sector ;
        }
        if ( sector < para->FirstSector || sector > para->LastSector ) continue ;
        sectorIndex = sector - para->FirstSector ;
        maxSectorHits[sectorIndex]++ ;
     }   
//
//     Allocate memory for hits in sectors
//
     for ( i = 0 ; i < nSectors ; i++ ) {
        if ( maxSectorHits[i] > 0 ) pHit[i] = new FtfHit[maxSectorHits[i]] ;
        nSectorHits[i] = 0 ;
     }
//
//     Fill hit classes
//
     for ( i = 0 ; i < tphit_h->nok ; i++ ) {
//
//   Check sector
//
        sector = (short)(tphit[i].row/100) ;
        if ( sector < 13 && tphit[i].z < 0 ) {
           if ( sector == 12 ) sector = 24 ;
           else sector = 24 - sector ;
        }
        if ( sector < para->FirstSector || sector > para->LastSector ) continue ;
        sectorIndex = sector - para->FirstSector ;
//
//   Check # hits in sector
//
        if ( nSectorHits[sectorIndex] >= maxSectorHits[sectorIndex] ) {
           printf ( " \n ftf: Too many hits in sector %d ", sector ) ;
           continue ;
        }
//
        j = nSectorHits[sectorIndex] ;
        tphit[i].track             = 0 ;
        (pHit[sectorIndex])[j].id  = i ;
        (pHit[sectorIndex])[j].row = (short)fmod(tphit[i].row,100)   ;
        (pHit[sectorIndex])[j].x   = tphit[i].x ;
        (pHit[sectorIndex])[j].y   = tphit[i].y ;
        (pHit[sectorIndex])[j].z   = tphit[i].z ;
        (pHit[sectorIndex])[j].dx  = tphit[i].dx ;
        (pHit[sectorIndex])[j].dy  = tphit[i].dy ;
        (pHit[sectorIndex])[j].dz  = tphit[i].dz ;
        nSectorHits[sectorIndex]++ ;
     }   
  } // end if for sector by sector logic
//
//    Loop over sectors
//
  int monCounter    =  monitor_h->nok ;
  int nTracksLast = 0 ;

  for ( int sectorIndex = 0 ; sectorIndex < nSectors ; sectorIndex++ ) {
//
//   Check if any hits
//
     if ( nSectorHits[sectorIndex] < 1 ) continue ;
//
//   Set limits for phi and eta if analysis sector by sector
//
     if ( para->FirstSector <= para->LastSector ) {
        sector = para->FirstSector + sectorIndex -1 ;
        tracker.para.phiMin  = para->sectorPhiMin[sector]/toDeg ;
        tracker.para.phiMax  = para->sectorPhiMax[sector]/toDeg ;
        tracker.para.phiShift = para->sectorPhiShift[sector]/toDeg ;
        tracker.para.etaMin  = para->sectorEtaMin[sector] ;
        tracker.para.etaMax  = para->sectorEtaMax[sector] ;
     }
     else
        sector = -1 ;
//
//  Call tracker
//
     tracker.nHits  = nSectorHits[sectorIndex] ;
     tracker.hit    = pHit[sectorIndex] ;
     nTracksLast    = tracker.nTracks ;
     float sectorTime = tracker.process ( ) ;
//
//    Fill monitoring table
//
     if ( monCounter < monitor_h->maxlen ) {
        monitor[monCounter].sector  = sector + 1 ;
        monitor[monCounter].nPoints = nSectorHits[sectorIndex] ;
        monitor[monCounter].nTracks = tracker.nTracks - nTracksLast ;
        monitor[monCounter].sTime   = sectorTime ;
        monCounter++ ;
        monitor_h->nok = monCounter ;
     }
  }
//
//    Merge primary tracks
//
  if ( tracker.para.mergePrimaries ) tracker.mergePrimaryTracks ( ) ;
//
//    Transfer hit assignment
//
  for ( sectorIndex = 0 ; sectorIndex < nSectors ; sectorIndex++ ) {
//
//   Check if any hits
//
     if ( nSectorHits[sectorIndex] < 1 ) continue ;
     tracker.hit    = pHit[sectorIndex] ;
//
//    Move hit assignment info
//
     int ii ;
     for ( int iHit = 0 ; iHit < nSectorHits[sectorIndex] ; iHit++ ){
        ii = tracker.hit[iHit].id ;
        if ( tracker.hit[iHit].track != 0 ) 
           tphit[ii].track = 1000 * ( tracker.hit[iHit].track->id + 1 ) ;
        else 
           tphit[ii].track = 0 ;
     }
  }
//
//    Move info to tpt tracks
//
 int counter = 0 ;
 for ( int i = 0 ; i < tracker.nTracks ; i++ ) {
    if ( tracker.track[i].flag < 0 ) continue ;

    tptrack[counter].id       = i + 1  ; 
    tptrack[counter].flag     = 1                        ; 
    tptrack[counter].nrec     = 
    tptrack[counter].nfit     = tracker.track[i].nHits   ; 
    tptrack[counter].q        = tracker.track[i].q       ; 
    tptrack[counter].chisq[0] = tracker.track[i].chi2[0] ; 
    tptrack[counter].chisq[1] = tracker.track[i].chi2[1] ; 
    tptrack[counter].invp     = 1.F / tracker.track[i].pt ; 
    tptrack[counter].phi0     = tracker.track[i].phi0 * toDeg ; 
    tptrack[counter].psi      = tracker.track[i].psi  * toDeg ; 
    tptrack[counter].r0       = tracker.track[i].r0      ; 
    tptrack[counter].tanl     = tracker.track[i].tanl    ; 
    tptrack[counter].z0       = tracker.track[i].z0      ; 
    tptrack[counter].hitid    = tracker.track[i].lastHit->id    ; 
 // tptrack[counter].hitid    = 0    ; 
    tptrack[counter].dedx[0]  = tracker.track[i].dedx ;
    tptrack[counter].dedx[1]  = 0.F ;

    counter++;
 }
//
 tptrack_h->nok = counter ;
//
//    Destroy objects
//
  delete []tracker.track ;
  delete []nSectorHits ;
  for ( int sect = 0 ; sect < nSectors ; sect++ ) {
     if ( maxSectorHits[sect] > 0 ) delete [](pHit[sect]) ;
  }
  delete []pHit ;
//
//
   return STAFCV_OK;
}
//***************************************************************************
//     Set parameters
//**************************************************************************
   void ftfSetParameters ( SL3TPCPARA_ST* para ) { 
      float Todeg = 57.29577951F ;

      tracker.para.infoLevel       = para->infoLevel ;
      tracker.para.segmentRowSearchRange = (short)para->SMaxSearchPadrowsSegment;
      tracker.para.trackRowSearchRange   = (short)para->SMaxSearchPadrowsTrack;
      tracker.para.mergePrimaries  = para->MergePrimaries;
      tracker.para.minHitsPerTrack = (short)para->SMinimumHitsPerTrack;    
      tracker.para.nHitsForSegment = (short)para->SMinimumHitsPerSegment;
      tracker.para.nEta            = (short)para->EtaSlices ;
      tracker.para.nPhi            = (short)para->PhiSlices ;
      tracker.para.nEtaTrack       = (short)para->NumberOfTanLSlices;
      tracker.para.nPhiTrack       = (short)para->NumberOfPsiSlices ;
      tracker.para.phiMin          = para->Phimin / Todeg ;
      tracker.para.phiMax          = para->Phimax / Todeg ;
      tracker.para.etaMin          = para->Etamin ;
      tracker.para.etaMax          = para->Etamax ;
      tracker.para.phiMinTrack     = para->MinSlicePsi / Todeg ;
      tracker.para.phiMaxTrack     = para->MaxSlicePsi / Todeg ;
      tracker.para.etaMinTrack     = para->MinSliceTanL ;
      tracker.para.etaMaxTrack     = para->MaxSliceTanL ;
      tracker.para.rowInnerMost    = (short)para->InnerMostRow ;
      tracker.para.rowOuterMost    = (short)para->OuterMostRow ;
      tracker.para.rowStart        = (short)para->startRow     ;
      tracker.para.szFitFlag       = para->SFitSz ;
      tracker.para.bField          = para->BField ;
      tracker.para.hitChi2Cut      = para->SChi2Cut;
      tracker.para.goodHitChi2     = para->SGoodChi2;
      tracker.para.trackChi2Cut    = para->SChi2TrackCut;
      tracker.para.deta            = para->SDEtaLimit ;
      tracker.para.dphi            = para->SDPhiLimit ;
      tracker.para.detaMerge       = para->SDTanlMaxMerge;
      tracker.para.dphiMerge       = para->SDPsiMaxMerge;
      tracker.para.szErrorScale    = para->ErrorScaleSz;
      tracker.para.xyErrorScale    = para->ErrorScaleXy;

      tracker.para.goBackwards     = 1 ;

   }
