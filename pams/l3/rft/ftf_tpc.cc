/*:>-------------------------------------------------------------------
**: FILE:       ftf_tpc.cc

**: HISTORY:
**:   
**:  
**:<------------------------------------------------------------------*/
#include "PAM.h"
#include "ftf_tpc.h"
#include "tcl_tphit.h"
#include "tpt_track.h"
#include "FTFinder.h"

FTFinder      tracker ;

extern "C" void ftfSetParameters ( L3T_TPC_PARA_ST* para ) ;

typedef FTF_Hit* PHit ; // type is hit pointer 

extern "C" long type_of_call ftf_tpc_(
  TABLE_HEAD_ST     *para_h,       L3T_TPC_PARA_ST    *para,         
  TABLE_HEAD_ST     *tphit_h,      TCL_TPHIT_ST       *tphit,        
  TABLE_HEAD_ST     *tptrack_h,    TPT_TRACK_ST       *tptrack,   
  TABLE_HEAD_ST     *monitor_h,    L3T_MONITOR_ST     *monitor )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    ftf_tpc_
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
  if ( tracker.para.mn_hit_trk < 3 ) {
     printf ( " \n Minimum # hits per track %d ", tracker.para.mn_hit_trk ) ;
     return STAFCV_BAD ;
  }
//
//     Allocate memory 
//  
  int nSectors       = 0 ;
  int max_tracks     = (int)tptrack_h->maxlen ;
  tracker.track      = new FTF_Track[max_tracks] ;
  tracker.max_tracks = max_tracks ;
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
     pHit[0] = new FTF_Hit[tphit_h->nok] ;

     for ( int i = 0 ; i < tphit_h->nok ; i++ ) {
        (pHit[0])[i].id  = i ;
        tphit[i].track   = 0 ;
        (pHit[0])[i].i_r = (short)fmod(tphit[i].row,100)   ;
        (pHit[0])[i].x   = tphit[i].x ;
        (pHit[0])[i].y   = tphit[i].y ;
        (pHit[0])[i].z   = tphit[i].z ;
        (pHit[0])[i].dx  = tphit[i].dx ;
        (pHit[0])[i].dy  = tphit[i].dy ;
        (pHit[0])[i].dz  = tphit[i].dz ;
     }
     nSectors       = 1 ;
     nSectorHits[0] = tphit_h->nok ;
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
        if ( maxSectorHits[i] > 0 ) pHit[i] = new FTF_Hit[maxSectorHits[i]] ;
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
        (pHit[sectorIndex])[j].i_r = (short)fmod(tphit[i].row,100)   ;
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
        tracker.para.phi_min  = para->sectorPhiMin[sector]/toDeg ;
        tracker.para.phi_max  = para->sectorPhiMax[sector]/toDeg ;
        tracker.para.phiShift = para->sectorPhiShift[sector]/toDeg ;
        tracker.para.eta_min  = para->sectorEtaMin[sector] ;
        tracker.para.eta_max  = para->sectorEtaMax[sector] ;
     }
     else
        sector = -1 ;
//
//  Call tracker
//
     tracker.n_hits = nSectorHits[sectorIndex] ;
     tracker.hit    = pHit[sectorIndex] ;
     nTracksLast    = tracker.n_tracks ;
     float sectorTime = tracker.FTF ( ) ;
//
//    Fill monitoring table
//
     if ( monCounter < monitor_h->maxlen ) {
        monitor[monCounter].sector  = sector + 1 ;
        monitor[monCounter].nPoints = nSectorHits[sectorIndex] ;
        monitor[monCounter].nTracks = tracker.n_tracks - nTracksLast ;
        monitor[monCounter].sTime   = sectorTime ;
        monCounter++ ;
        monitor_h->nok = monCounter ;
     }
  }
//
//    Merge primary tracks
//
  if ( tracker.para.merge_primaries ) tracker.mergePrimaryTracks ( ) ;
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
 for ( int i = 0 ; i < tracker.n_tracks ; i++ ) {
    if ( tracker.track[i].flag < 0 ) continue ;

    tptrack[counter].id       = i + 1  ; 
    tptrack[counter].flag     = 1                        ; 
    tptrack[counter].nrec     = 
    tptrack[counter].nfit     = tracker.track[i].n_hits  ; 
    tptrack[counter].q        = tracker.track[i].q       ; 
    tptrack[counter].chisq[0] = tracker.track[i].chi2[0] ; 
    tptrack[counter].chisq[1] = tracker.track[i].chi2[1] ; 
    tptrack[counter].invp     = 1.F / tracker.track[i].pt ; 
    tptrack[counter].phi0     = tracker.track[i].phi0 * toDeg ; 
    tptrack[counter].psi      = tracker.track[i].psi  * toDeg ; 
    tptrack[counter].r0       = tracker.track[i].r0      ; 
    tptrack[counter].tanl     = tracker.track[i].tanl    ; 
    tptrack[counter].z0       = tracker.track[i].z0      ; 
    tptrack[counter].hitid    = tracker.track[i].last_hit->id    ; 
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
   void ftfSetParameters ( L3T_TPC_PARA_ST* para ) { 
      float Todeg = 57.29577951F ;

      tracker.para.infoLevel       = para->infoLevel ;
      tracker.para.dr_segm         = (short)para->SMaxSearchPadrowsSegment;
      tracker.para.dr_track        = (short)para->SMaxSearchPadrowsTrack;
      tracker.para.merge_primaries = para->MergePrimaries;
      tracker.para.mn_hit_trk      = (short)para->SMinimumHitsPerTrack;    
      tracker.para.n_hit_segm      = (short)para->SMinimumHitsPerSegment;
      tracker.para.n_eta           = (short)para->EtaSlices ;
      tracker.para.n_phi           = (short)para->PhiSlices ;
      tracker.para.n_eta_track     = (short)para->NumberOfTanLSlices;
      tracker.para.n_phi_track     = (short)para->NumberOfPsiSlices ;
      tracker.para.phi_min         = para->Phimin / Todeg ;
      tracker.para.phi_max         = para->Phimax / Todeg ;
      tracker.para.eta_min         = para->Etamin ;
      tracker.para.eta_max         = para->Etamax ;
      tracker.para.phi_min_track   = para->MinSlicePsi / Todeg ;
      tracker.para.phi_max_track   = para->MaxSlicePsi / Todeg ;
      tracker.para.eta_min_track   = para->MinSliceTanL ;
      tracker.para.eta_max_track   = para->MaxSliceTanL ;
      tracker.para.row_end         = (short)para->InnerMostRow ;
      tracker.para.row_start       = (short)para->OuterMostRow ;
      tracker.para.sz_fit_flag     = para->SFitSz ;
      tracker.para.bfield          = para->BField ;
      tracker.para.chi2_hit_cut    = para->SChi2Cut;
      tracker.para.chi2_hit_good   = para->SGoodChi2;
      tracker.para.chi2_track_cut  = para->SChi2TrackCut;
      tracker.para.deta            = para->SDEtaLimit ;
      tracker.para.dphi            = para->SDPhiLimit ;
      tracker.para.deta_merge      = para->SDTanlMaxMerge;
      tracker.para.dphi_merge      = para->SDPsiMaxMerge;
      tracker.para.sz_error_scale  = para->ErrorScaleSz;
      tracker.para.xy_error_scale  = para->ErrorScaleXy;

      tracker.para.go_backwards    = 1 ;

   }
