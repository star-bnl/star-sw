/*:>-------------------------------------------------------------------
**: FILE:       sl3tTpc.cxx
**: HISTORY:    07/05/99 ppy version with input/output buffer implemented
**:             07/06/99 replace SL3TRACK and SL3HIT with SL3BUFFER
**:   
**:  
**:<------------------------------------------------------------------*/
#include "PAM.h"
#include "sl3Tpc.h"
#include "FtfSl3.h"


void sl3TpcSetParameters ( FtfSl3* tracker, SL3TPCPARA_ST* para ) ;

extern "C" long type_of_call sl3Tpc_(
  TABLE_HEAD_ST     *paraH,       SL3TPCPARA_ST  *para,
  TABLE_HEAD_ST     *hitH,        SL3BUFFER_ST   *hit,        
  TABLE_HEAD_ST     *l3TrackH,    SL3BUFFER_ST   *l3Track )   
{
/*:>--------------------------------------------------------------------
**: ROUTINE:     sl3Tpc
**: DESCRIPTION: Calls sl3 Tracking   
**:
**: 
**: AUTHOR:     ppy - Pablo P. Yepes, yepes@rice.edu  
**: ARGUMENTS:
**:       IN:
**:        sl3TpcPara   - Level 3 TPC tracking parameters
**:        hit          - L3 Hit Buffer
**:      OUT:
**:        l3Track      - L3 Track Buffer
**: RETURNS:    STAF Condition Value
**:>------------------------------------------------------------------*/
//
   FtfSl3 tracker ;
   tracker.setup();
   sl3TpcSetParameters ( &tracker, para ) ;
   int debugLevel = tracker.debugLevel = tracker.para.infoLevel ;
//
//   Decode hit buffer
//
   int nBytes = hitH->nok ;
   int nHits =  tracker.readSector ( (struct TPCSECLP *)hit ) ;
//
//   Track sector
//
   tracker.processSector ( ) ;
//
//   Fill Track buffer
//
   l3TrackH->nok = tracker.fillTracks ( l3TrackH->maxlen,  (int *)l3Track ) ; 
   if ( debugLevel > 0 ) {
      printf ( " sl3: tracks found %d \n ", tracker.nTracks ) ;
   }
//
   return STAFCV_OK ;
}
//**************************************************************************
//     Set parameters
//**************************************************************************
   void sl3TpcSetParameters ( FtfSl3* tracker, SL3TPCPARA_ST* para ) {

      tracker->para.infoLevel       = para->infoLevel ;
      tracker->para.segmentRowSearchRange = (short)para->SMaxSearchPadrowsSegment;
      tracker->para.trackRowSearchRange   = (short)para->SMaxSearchPadrowsTrack;
      tracker->para.mergePrimaries  = para->MergePrimaries;
      tracker->para.minHitsPerTrack = (short)para->SMinimumHitsPerTrack;
      tracker->para.nHitsForSegment = (short)para->SMinimumHitsPerSegment;
      tracker->para.nEta            = (short)para->EtaSlices ;
      tracker->para.nPhi            = (short)para->PhiSlices ;
      tracker->para.nEtaTrack       = (short)para->NumberOfTanLSlices;
      tracker->para.nPhiTrack       = (short)para->NumberOfPsiSlices ;
      tracker->para.phiMin          = para->Phimin / toDeg ;
      tracker->para.phiMax          = para->Phimax / toDeg ;
      tracker->para.etaMin          = para->Etamin ;
      tracker->para.etaMax          = para->Etamax ;
      tracker->para.phiMinTrack     = para->MinSlicePsi / toDeg ;
      tracker->para.phiMaxTrack     = para->MaxSlicePsi / toDeg ;
      tracker->para.etaMinTrack     = para->MinSliceTanL ;
      tracker->para.etaMaxTrack     = para->MaxSliceTanL ;
      tracker->para.rowInnerMost    = (short)para->InnerMostRow ;
      tracker->para.rowOuterMost    = (short)para->OuterMostRow ;
      tracker->para.rowStart        = (short)para->startRow     ;
      tracker->para.szFitFlag       = para->SFitSz ;
      tracker->para.bField          = para->BField ;
      tracker->para.hitChi2Cut      = para->SChi2Cut;
      tracker->para.goodHitChi2     = para->SGoodChi2;
      tracker->para.trackChi2Cut    = para->SChi2TrackCut;
      tracker->para.deta            = para->SDEtaLimit ;
      tracker->para.dphi            = para->SDPhiLimit ;
      tracker->para.detaMerge       = para->SDTanlMaxMerge;
      tracker->para.dphiMerge       = para->SDPsiMaxMerge;
      tracker->para.szErrorScale    = para->ErrorScaleSz;
      tracker->para.xyErrorScale    = para->ErrorScaleXy;

      tracker->para.goBackwards     = 1 ;
   }
