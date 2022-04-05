#include <iostream>
#include <math.h>
#include <vector>
#include <stdlib.h>
#include <bitset>

#include "TH1F.h"

#include "StMessMgr.h"

#include "StEventTypes.h"
#include "StThreeVectorF.hh"
#include "StEvent.h"
#include "StTrack.h"
#include "StTrackGeometry.h"
#include "StDcaGeometry.h"
#include "StTpcDedxPidAlgorithm.h"
#include "StDedxPidTraits.h"
#include "StTrackPidTraits.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuMtdHit.h"
#include "StMuDSTMaker/COMMON/StMuMtdPidTraits.h"


#include "StMtdEvtFilterMaker.h"
#include "tables/St_trgOfflineFilter_Table.h"
#include "tables/St_mtdEventFilterCuts_Table.h"
#include "tables/St_MtdTrackFilterTag_Table.h"

#include "tables/St_HighPtTag_Table.h"

ClassImp(StMtdEvtFilterMaker)

//_____________________________________________________________________________
StMtdEvtFilterMaker::StMtdEvtFilterMaker(const Char_t *name) : StMaker(name)
{
  // default constructor

  mStEvent               = NULL;
  mMuDst                 = NULL;
  mIsJpsiEvent           = kFALSE;
  mIsDiMuon              = kFALSE;
  mIsDiMuonOnly          = kFALSE;

     
  mMinTrkPtAll           = 1.0;
  mMinTrkPtLead          = 0;
  mMinNHitsFit           = 15;
  mMinNHitsDedx          = 10;
  mMinFitHitsFraction    = 0.52;
  mMaxDca                = 1e4;
  mMinNsigmaPi           = -1e4;
  mMaxNsigmaPi           = 1e4;
  mMaxDeltaZ             = 1e4;
  nMinMuonCandidates     = 2;

  mSaveHistos            = false;
  mhEventStat            = NULL;
  mhNMuonCandidates      = NULL;
}
 
//_____________________________________________________________________________
StMtdEvtFilterMaker::~StMtdEvtFilterMaker()
{
  // default destructor
}

//_____________________________________________________________________________
Int_t StMtdEvtFilterMaker::Init()
{
  SetFlavor("MtdEvtFilter","trgOfflineFilter");

  // book histograms
  bookHistos();

  return kStOK;
}

//_____________________________________________________________________________
Int_t StMtdEvtFilterMaker::InitRun(const Int_t runNumber)
{
  LOG_INFO << "Retrieving trigger ID from database ..." << endm;
  mTriggerIDs.clear();
  mOtherTrigIDs.clear();

  St_trgOfflineFilter* flaggedTrgs =
    static_cast<St_trgOfflineFilter *>(GetDataBase("Calibrations/trg/trgOfflineFilter"));
  if (!flaggedTrgs) 
    {
      LOG_ERROR << "Could not find Calibrations/trg/trgOfflineFilter in database" << endm;
      return kStErr;
    }

  trgOfflineFilter_st* flaggedTrg = flaggedTrgs->GetTable();
  for (long j = 0; j < flaggedTrgs->GetNRows(); j++, flaggedTrg++) 
    {
      int trigid = flaggedTrg->trigid;
      if(trigid==0) continue;
      else if (trigid<1e9) mTriggerIDs.push_back(trigid);
      else mOtherTrigIDs.push_back(trigid-1e9);
    }

  if(Debug())
    {
      for(unsigned int i=0; i<mTriggerIDs.size(); i++){
	LOG_INFO << "Di-muon trigger ID: " << mTriggerIDs[i] << endm; }

      for(unsigned int i=0; i<mOtherTrigIDs.size(); i++){
	LOG_INFO << "Other MTD trigger ID: " << mOtherTrigIDs[i] << endm; }
    }

  LOG_INFO << "Retrieving event filtering cuts from database ..." << endm;

  St_mtdEventFilterCuts *mtdEventFilterCuts = 
    static_cast<St_mtdEventFilterCuts*>(GetDataBase("Calibrations/mtd/mtdEventFilterCuts"));
  if(!mtdEventFilterCuts)
    {
      LOG_ERROR << "No mtdEventFilterCuts table found in database" << endm;
      return kStErr;
    }
  mtdEventFilterCuts_st *table = static_cast<mtdEventFilterCuts_st*>(mtdEventFilterCuts->GetTable());
    
  mMinTrkPtAll           = table->minTrkPtAll;
  mMinTrkPtLead          = table->minTrkPtLead;
  mMinNHitsFit           = (int)table->minNHitsFit;
  mMinNHitsDedx          = (int)table->minNHitsDedx;
  mMinFitHitsFraction    = table->minFitHitsFrac;
  mMaxDca                = table->maxDca;
  mMinNsigmaPi           = table->minNsigmaPi;
  mMaxNsigmaPi           = table->maxNsigmaPi;
  mMaxDeltaZ             = table->maxDeltaZ;
  nMinMuonCandidates     = (int)table->minNMuons;
  if(Debug())
    {
      LOG_INFO << "minTrkPtAll    = " << mMinTrkPtAll        << endm;
      LOG_INFO << "minTrkPtLead   = " << mMinTrkPtLead       << endm;
      LOG_INFO << "minNHitsFit    = " << mMinNHitsFit        << endm;
      LOG_INFO << "minNHitsDedx   = " << mMinNHitsDedx       << endm;
      LOG_INFO << "minFitHitsFrac = " << mMinFitHitsFraction << endm;
      LOG_INFO << "maxDca         = " << mMaxDca             << endm;
      LOG_INFO << "minNsigmaPi    = " << mMinNsigmaPi        << endm;
      LOG_INFO << "maxNsigmaPi    = " << mMaxNsigmaPi        << endm;
      LOG_INFO << "maxDeltaZ      = " << mMaxDeltaZ          << endm;
      LOG_INFO << "minNMuons      = " << nMinMuonCandidates  << endm;
    }

  
  return kStOK;
}

//_____________________________________________________________________________
Int_t StMtdEvtFilterMaker::Make()
{

  mIsJpsiEvent = kFALSE;
  mIsDiMuon = kFALSE;
  mIsDiMuonOnly = kFALSE;


  // Check the availability of input data
  Int_t iret = -1;

  mStEvent = (StEvent*) GetInputDS("StEvent");
  if(mStEvent)
    {
      LOG_DEBUG << "Running on StEvent ..." << endm;
      iret = processStEvent();
    }
  else 
    {
      StMuDstMaker *muDstMaker = (StMuDstMaker*) GetMaker("MuDst");
      if(muDstMaker) 
	{
	  mMuDst = muDstMaker->muDst();
	  iret = processMuDst();
	}
      else
	{
	  LOG_ERROR << "No muDST is available ... "<< endm;
	  iret = kStErr;
	}
    }
  
  MtdTrackFilterTag_st tagTable;
  StMaker* maskMk = GetMakerInheritsFrom("StMtdTrackingMaskMaker");
  tagTable.tpcSectors = (maskMk ? maskMk->UAttr("TpcSectorsByMtd") : ~0U);
  tagTable.isRejectEvent = (isRejectEvent() ? 1 : 0);
  tagTable.shouldHaveRejectEvent = shouldHaveRejectEvent();
  St_MtdTrackFilterTag* MtdTrackFilterTag = new St_MtdTrackFilterTag("MtdTrackFilterTag",1);
  MtdTrackFilterTag->AddAt(&tagTable,0);
  AddData(MtdTrackFilterTag);

  if(mStEvent)
    {
      LOG_INFO << "Is event rejected:        " << tagTable.isRejectEvent << endm;
      LOG_INFO << "Should event be rejected: " << tagTable.shouldHaveRejectEvent << endm;
    }

  return iret;
}

//_____________________________________________________________________________
bool StMtdEvtFilterMaker::isRejectEvent()
{
  ///
  /// check if this event should be discarded, namely it does not
  /// contain a j/psi candidate
  ///

  return (mIsDiMuonOnly && !mIsJpsiEvent);
}

//_____________________________________________________________________________
int StMtdEvtFilterMaker::shouldHaveRejectEvent()
{
  ///
  /// check whether this event should have been discarded, had it
  /// not contain MTD triggers other than the di-muon trigger
  ///

  if(mIsDiMuon)
    {
      if(!mIsDiMuonOnly && !mIsJpsiEvent) return 1;
      else return 2;
    }
  else return 0;
}

//_____________________________________________________________________________
void StMtdEvtFilterMaker::checkTriggerIDs(const vector<unsigned int> triggers)
{
  ///
  /// check if the event contains a di-muon trigger, and if
  /// it only contains a di-muon trigger mudulo the MB trigger
  ///

  for(unsigned int i=0; !mIsDiMuon && i<triggers.size(); i++)
    {
      for(unsigned int j=0; !mIsDiMuon && j<mTriggerIDs.size(); j++)
  	{
  	  mIsDiMuon = ((int)triggers[i]==mTriggerIDs[j]);
  	}
    }

  mIsDiMuonOnly = mIsDiMuon;
  for(unsigned int i=0; mIsDiMuonOnly && i<triggers.size(); i++)
    {
      for(unsigned int j=0; mIsDiMuonOnly && j<mOtherTrigIDs.size(); j++)
	{
	  mIsDiMuonOnly = !((int)triggers[i]==mOtherTrigIDs[j]);
	}
    }
}

//_____________________________________________________________________________
Int_t StMtdEvtFilterMaker::processMuDst()
{
  mhEventStat->Fill(0.5);

  // check trigger id
  vector<unsigned int> triggers = mMuDst->event()->triggerIdCollection().nominal().triggerIds();
  checkTriggerIDs(triggers);
  if(!mIsDiMuon) return kStOK;
  mhEventStat->Fill(1.5);

  // Global tracks
  int nNodes = mMuDst->numberOfGlobalTracks();
  LOG_DEBUG << "# of global tracks " << nNodes << endm;

  int nMuonCandidates = 0;
  double pt_max = 0;

  for(int i=0; i<nNodes; i++)
    {
      StMuTrack *gTrack = mMuDst->globalTracks(i);
      if(!isValidTrack(gTrack)) continue;
      if(!isMuonCandidate(gTrack)) continue;

      nMuonCandidates ++;
      double pt = gTrack->pt();
      if(pt_max<pt) pt_max = pt;
    }

  mhNMuonCandidates->Fill(nMuonCandidates);
  if(nMuonCandidates>=nMinMuonCandidates && pt_max>mMinTrkPtLead)
    {
      mIsJpsiEvent = kTRUE;
      mhEventStat->Fill(2.5);
    }

  return kStOK;
}

//_____________________________________________________________________________
Int_t StMtdEvtFilterMaker::processStEvent()
{
  mhEventStat->Fill(0.5);

  // check trigger id
  vector<unsigned int> triggers = mStEvent->triggerIdCollection()->nominal()->triggerIds();
  checkTriggerIDs(triggers);
  if(!mIsDiMuon) return kStOK;
  mhEventStat->Fill(1.5);

  // Global tracks
  StSPtrVecTrackNode& nodes = mStEvent->trackNodes();
  int nNodes = nodes.size();
  LOG_DEBUG << "# of global tracks " << nNodes << endm;

  int nMuonCandidates = 0;
  double pt_max = 0;
  for(int i=0; i<nNodes; i++)
    {
      StTrack *gTrack = nodes[i]->track(global);

      if(!isValidTrack(gTrack)) continue;
      if(!isMuonCandidate(gTrack)) continue;

      nMuonCandidates ++;
      double pt = gTrack->geometry()->momentum().perp();
      if(pt_max<pt) pt_max = pt;
    }

  mhNMuonCandidates->Fill(nMuonCandidates);
  if(nMuonCandidates>=nMinMuonCandidates && pt_max>mMinTrkPtLead)
    {
      mIsJpsiEvent = kTRUE;
      mhEventStat->Fill(2.5);
    }

  return kStOK;
}

//_____________________________________________________________________________
bool StMtdEvtFilterMaker::isValidTrack(StTrack *track) 
{
  ///
  /// check if the StTrack fulfill all the quality cuts
  ///
  if(!track) return kFALSE;
  StThreeVectorF mom = track->geometry()->momentum();
  Float_t pt      = mom.perp();
  int nHitsFit  = track->fitTraits().numberOfFitPoints(kTpcId);
  int nHitsPoss = track->numberOfPossiblePoints(kTpcId);
  int nHitsDedx = 0;
  StTpcDedxPidAlgorithm pidAlgorithm;
  const StParticleDefinition *pd = track->pidTraits(pidAlgorithm);
  if(pd && pidAlgorithm.traits())
    nHitsDedx = pidAlgorithm.traits()->numberOfPoints();

  if(pt < mMinTrkPtAll)                            return kFALSE;
  if(nHitsFit<mMinNHitsFit)                        return kFALSE;
  if(nHitsDedx<mMinNHitsDedx)                      return kFALSE;
  if(nHitsFit/(1.0*nHitsPoss)<mMinFitHitsFraction) return kFALSE;
  if(mMaxDca<1e4)
    {
      // Only check track DCA when the cut is reset
      // by the user
      
      StPrimaryVertex* pvtx = mStEvent->primaryVertex();
      if(!pvtx) return kFALSE;
      StThreeVectorD vtxPos = pvtx->position();
      StDcaGeometry* trDcaGeom = ((StGlobalTrack*) track)->dcaGeometry();
      if(!trDcaGeom) return kFALSE;
      StPhysicalHelixD dcahh = trDcaGeom->helix();
      double dca = dcahh.distance(vtxPos,kFALSE);
      if(dca>mMaxDca) return kFALSE;
    }

  return kTRUE;
}

//_____________________________________________________________________________
bool StMtdEvtFilterMaker::isValidTrack(StMuTrack *track) 
{
  ///
  /// check if the StMuTrack fulfill all the quality cuts
  ///

  if(!track) return kFALSE;
  StThreeVectorF mom = track->momentum();
  double pt = mom.perp();
  int nHitsFit  = track->nHitsFit(kTpcId);
  int nHitDedx  = track->nHitsDedx();
  int nHitPoss  = track->nHitsPoss(kTpcId);
  double dca    = track->dcaGlobal().mag();
  
  if(pt < mMinTrkPtAll)                           return kFALSE;
  if(nHitsFit<mMinNHitsFit)                       return kFALSE;
  if(nHitDedx<mMinNHitsDedx)                      return kFALSE;
  if(dca>mMaxDca)                                 return kFALSE;
  if(nHitsFit/(1.0*nHitPoss)<mMinFitHitsFraction) return kFALSE;
  return kTRUE;
}

//_____________________________________________________________________________
bool StMtdEvtFilterMaker::isMuonCandidate(StTrack *track) 
{
  ///
  /// check if the StTrack a muon candidate
  ///

  if(!track) return kFALSE;

  if(mMaxNsigmaPi<1e4)
    {
      // nSigmaPi cut
      double nSigmaPi = -999.;
      StTpcDedxPidAlgorithm pidAlgorithm;
      const StParticleDefinition *pd = track->pidTraits(pidAlgorithm);
      if(pd && pidAlgorithm.traits())
	{
	  static StPionPlus* Pion = StPionPlus::instance();
	  nSigmaPi = pidAlgorithm.numberOfSigma(Pion);
	}
      if(nSigmaPi<mMinNsigmaPi || nSigmaPi>mMaxNsigmaPi) return kFALSE;
    }

  // dz cut
  StMtdPidTraits* mtdpid = 0;
  StSPtrVecTrackPidTraits& traits = track->pidTraits();
  for(unsigned int it=0; it<traits.size(); it++)
    {
      if (traits[it]->detector() == kMtdId)
	{
	  mtdpid = dynamic_cast<StMtdPidTraits*>(traits[it]);
	  break;
	}
    }
  if(!mtdpid) return kFALSE;
  if(mMaxDeltaZ<1e4)
    {
      double dz = mtdpid->deltaZ();
      if(dz > mMaxDeltaZ) return kFALSE;
    }

  return kTRUE;
 }

//_____________________________________________________________________________
bool StMtdEvtFilterMaker::isMuonCandidate(StMuTrack *track) 
{
  ///
  /// check if the StMuTrack a muon candidate
  ///

  if(!track) return kFALSE;

  if(mMaxNsigmaPi<1e4)
    {
      // nSigmaPi cut
      double nSigmaPi = track->nSigmaPion();
      if(nSigmaPi<mMinNsigmaPi || nSigmaPi>mMaxNsigmaPi)  return kFALSE;
    }

  // dz cut
  const StMuMtdHit *hit = track->mtdHit(); 
  if(!hit) return kFALSE;
  if(mMaxDeltaZ<1e4)
    {
      const StMuMtdPidTraits mtdPid = track->mtdPidTraits();
      double dz = mtdPid.deltaZ();
      if(dz > mMaxDeltaZ) return kFALSE;
    }

  return kTRUE;
 }

//_____________________________________________________________________________
void StMtdEvtFilterMaker::bookHistos()
{
  mhEventStat = new TH1F("hEventStat","Event statistics",5,0,5);
  mhEventStat->GetXaxis()->SetBinLabel(1,"All events");
  mhEventStat->GetXaxis()->SetBinLabel(2,"Good trigger");
  mhEventStat->GetXaxis()->SetBinLabel(3,"Accepted");

  mhNMuonCandidates = new TH1F("hNMuonCandidates","Number of muon candidates per event;N",10,0,10);

  if(mSaveHistos)
    {
      AddHist(mhEventStat);
      AddHist(mhNMuonCandidates);
    }

}

// $Id: StMtdEvtFilterMaker.cxx,v 1.3 2016/07/27 15:24:30 marr Exp $
// $Log: StMtdEvtFilterMaker.cxx,v $
// Revision 1.3  2016/07/27 15:24:30  marr
// Fix coverity check: initialization of data members
//
// Revision 1.2  2015/04/23 21:10:19  marr
// 1. remove dz and pTlead cuts in the filtering by default
// 2. change the number scheme for shouldHaveRejectEvent()
//
// Revision 1.1  2015/04/07 14:10:37  jeromel
// First version of StMtdEvtFilterMaker - R.Ma - review closed 2015/04/06
//
//
