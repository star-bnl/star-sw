/*:>-------------------------------------------------------------------
**: FILE:       ftfTpc.cc

**: HISTORY:
**:             13aug1999 ppy FtFinder class creation moved to inside ftfTpc
**:             13aug1999 ppy track variable initialize to 0 for FtfHit strucs
**:                           This was crashing bfc in second event
**:                           cosmetics changes in some printf's
**:   
**:  
**:<------------------------------------------------------------------*/
#include "PAM.h"
#include "ftfTpc.h"
#include "tcl_tphit.h"
#include "tpt_track.h"
#include "FtfFinder.h"
#include "FtfPara.h"


extern "C" void ftfSetParameters ( FtfPara *ftfPara, SL3TPCPARA_ST* para ) ;

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
  FtfFinder      tracker ;
  float toDeg = 57.29577951F ;
//
//    Set parameters
//
  tracker.para.setDefaults ( ) ;
  FtfPara *ftfPara = &(tracker.para) ;
  ftfSetParameters ( ftfPara, para ) ;
//
//    Reset tracker
//
  tracker.reset ( ) ;
//
//   Check there is something coming in
//
  if ( tphit_h->nok < 1 ) {
     printf ( "ftfTpc: tphit table is empty \n" ) ;
     return STAFCV_BAD ;
  }
//
//    Check min hits per track
//
  if ( ftfPara->minHitsPerTrack < 3 ) {
     printf ( "ftfTpc: Minimum # hits per track %d\n", ftfPara->minHitsPerTrack ) ;
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
        (pHit[0])[i].row = (short)fmod((double)tphit[i].row,100)   ;
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
        printf ( "ftfTpc: Too many sectors %d \n", nSectors ) ;
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
           printf ( "ftfTpc: Too many hits in sector %d\n", sector ) ;
           continue ;
        }
//
        j = nSectorHits[sectorIndex] ;
        tphit[i].track             = 0 ;
        (pHit[sectorIndex])[j].id  = i ;
        (pHit[sectorIndex])[j].track  = 0 ;
        (pHit[sectorIndex])[j].row = (short)fmod((double)tphit[i].row,100)   ;
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
  tracker.nTracks   = 0 ;
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
        ftfPara->phiMin  = para->sectorPhiMin[sector]/toDeg ;
        ftfPara->phiMax  = para->sectorPhiMax[sector]/toDeg ;
        ftfPara->phiShift = para->sectorPhiShift[sector]/toDeg ;
        ftfPara->etaMin  = para->sectorEtaMin[sector] ;
        ftfPara->etaMax  = para->sectorEtaMax[sector] ;
     }
     else
        sector = -1 ;
//
//  Call tracker
//
     tracker.nHits  = nSectorHits[sectorIndex] ;
     tracker.hit    = pHit[sectorIndex] ;
     nTracksLast    = tracker.nTracks ;
     if ( para->infoLevel > 2 ) 
         printf ( "ftfTpc: start tracking sector %d maxHits %d\n", 
                             sectorIndex, tracker.nHits ) ;
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
  if ( ftfPara->mergePrimaries ) tracker.mergePrimaryTracks ( ) ;
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
    if ( tracker.track[i].lastHit != 0 ) 
       tptrack[counter].hitid    = tracker.track[i].lastHit->id    ; 
    else
       tptrack[counter].hitid    = 0    ; 
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
   void ftfSetParameters ( FtfPara* ftfPara, SL3TPCPARA_ST* para ) { 
      float Todeg = 57.29577951F ;

      ftfPara->infoLevel       = para->infoLevel ;
      ftfPara->segmentRowSearchRange = (short)para->SMaxSearchPadrowsSegment;
      ftfPara->trackRowSearchRange   = (short)para->SMaxSearchPadrowsTrack;
      ftfPara->mergePrimaries  = para->MergePrimaries;
      ftfPara->minHitsPerTrack = (short)para->SMinimumHitsPerTrack;    
      ftfPara->nHitsForSegment = (short)para->SMinimumHitsPerSegment;
      ftfPara->nEta            = (short)para->EtaSlices ;
      ftfPara->nPhi            = (short)para->PhiSlices ;
      ftfPara->nEtaTrack       = (short)para->NumberOfTanLSlices;
      ftfPara->nPhiTrack       = (short)para->NumberOfPsiSlices ;
      ftfPara->phiMin          = para->Phimin / Todeg ;
      ftfPara->phiMax          = para->Phimax / Todeg ;
      ftfPara->etaMin          = para->Etamin ;
      ftfPara->etaMax          = para->Etamax ;
      ftfPara->phiMinTrack     = para->MinSlicePsi / Todeg ;
      ftfPara->phiMaxTrack     = para->MaxSlicePsi / Todeg ;
      ftfPara->etaMinTrack     = para->MinSliceTanL ;
      ftfPara->etaMaxTrack     = para->MaxSliceTanL ;
      ftfPara->rowInnerMost    = (short)para->InnerMostRow ;
      ftfPara->rowOuterMost    = (short)para->OuterMostRow ;
      ftfPara->rowStart        = (short)para->startRow     ;
      ftfPara->szFitFlag       = para->SFitSz ;
      ftfPara->bField          = para->BField ;
      ftfPara->hitChi2Cut      = para->SChi2Cut;
      ftfPara->goodHitChi2     = para->SGoodChi2;
      ftfPara->trackChi2Cut    = para->SChi2TrackCut;
      ftfPara->deta            = para->SDEtaLimit ;
      ftfPara->dphi            = para->SDPhiLimit ;
      ftfPara->detaMerge       = para->SDTanlMaxMerge;
      ftfPara->dphiMerge       = para->SDPsiMaxMerge;
      ftfPara->szErrorScale    = para->ErrorScaleSz;
      ftfPara->xyErrorScale    = para->ErrorScaleXy;

      ftfPara->goBackwards     = 1 ;

   }
