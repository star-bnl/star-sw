#include <iostream>
#include <bitset>
#include <math.h>
#include <vector>
#include <stdlib.h>
#include <iterator>

#include "TTree.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TProfile.h"
#include "TLorentzVector.h"

#include "StEventTypes.h"
#include "StThreeVectorF.hh"
#include "PhysicalConstants.h"
#include "StMemoryInfo.hh"
#include "StMessMgr.h"
#include "StTimer.hh"
#include "StEnumerations.h"

#include "StEvent.h"
#include "StVertex.h"
#include "StTriggerData.h"
#include "StTrack.h"
#include "StDcaGeometry.h"
#include "StDedxPidTraits.h"
#include "StTrackPidTraits.h"
#include "StBTofPidTraits.h"
#include "StBTofCollection.h"
#include "StBTofHit.h"
#include "StBTofRawHit.h"
#include "StBTofHeader.h"
#include "StMtdCollection.h"
#include "StMtdHeader.h"
#include "StMtdRawHit.h"
#include "StMtdHit.h"
#include "StMtdPidTraits.h"
#include "StTpcDedxPidAlgorithm.h"
#include "StMuDSTMaker/COMMON/StMuBTofPidTraits.h"
#include "StMuDSTMaker/COMMON/StMuBTofHit.h"
#include "StarClassLibrary/StParticleDefinition.hh"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuMtdCollection.h"
#include "StMuDSTMaker/COMMON/StMuMtdHeader.h"
#include "StMuDSTMaker/COMMON/StMuMtdRawHit.h"
#include "StMuDSTMaker/COMMON/StMuMtdHit.h"
#include "StMuDSTMaker/COMMON/StMuMtdPidTraits.h"

#include "StPicoDstMaker/StPicoDstMaker.h"
#include "StPicoEvent/StPicoDst.h"
#include "StPicoEvent/StPicoEvent.h"
#include "StPicoEvent/StPicoTrack.h"
#include "StPicoEvent/StPicoMtdHit.h"
#include "StPicoEvent/StPicoMtdTrigger.h"
#include "StPicoEvent/StPicoMtdPidTraits.h"
#include "StPicoEvent/StPicoBTofPidTraits.h"

#include "StMtdQAMaker/StMtdTrigUtil.h"
#include "StMtdQAMaker.h"
#include "tables/St_mtdModuleToQTmap_Table.h"
#include "tables/St_mtdQTSlewingCorr_Table.h"
#include "tables/St_mtdQTSlewingCorrPart2_Table.h"

ClassImp(StMtdQAMaker)

//_____________________________________________________________________________
StMtdQAMaker::StMtdQAMaker(const Char_t *name) : 
  StMaker(name),
  mStEvent(NULL), mMuDst(NULL), mPicoDst(NULL), mTrigUtil(NULL),
  mRunId(-1), mRunYear(-1), mCollisionSystem("pp"), mVertexMode(1),
  mPrintMemory(kFALSE), mPrintCpu(kFALSE), mPrintConfig(kFALSE), mTriggerIDs(0),
  mMaxVtxZ(100.), mMaxVtxR(2.), mApplyVtxDzCut(kTRUE), mMaxVtxDz(3.),
  mMinTrkPt(1.0), mMaxTrkPt(100), mMinTrkPhi(0.), mMaxTrkPhi(2*pi), mMinTrkEta(-1), mMaxTrkEta(1),
  mMinNHitsFit(15), mMinNHitsDedx(10), mMinFitHitsFraction(0.52), mMaxDca(3.), 
  mMinMuonPt(1.3), mMinNsigmaPi(-2), mMaxNsigmaPi(3), mMinMuonDeltaZ(-20.), mMaxMuonDeltaZ(20.),
  mMinMuonDeltaY(-20), mMaxMuonDeltaY(20), mMinMuonDeltaTof(-2), mMaxMuonDeltaTof(1), mMtdHitTrigger(kTRUE)
{
  // default constructor
  mhEventStat              = NULL;
  mhEventCuts              = NULL;
  mhRunId                  = NULL;
  mhZdcRate                = NULL;
  mhBbcRate                = NULL;

  mhVtxZvsVpdVzDefault     = NULL;
  mhVtxZDiffDefault        = NULL;
  mhVtxZvsVpdVzClosest     = NULL;
  mhVtxZDiffClosest        = NULL;
  mhVtxClosestIndex        = NULL;
  mhVertexXY               = NULL;
  mhVertexXZ               = NULL;
  mhVertexYZ               = NULL;
  mhVertexZ                = NULL;
  mhVtxZDiffVsTpcVz        = NULL;
  mhVpdVz                  = NULL;

  mhRefMult                = NULL;
  mhgRefMult               = NULL;
  mhgRefMultVsRefMult      = NULL;
  mhTpcVzVsRefMult         = NULL;
  mhDiffVzVsRefMult        = NULL;
  mhZdcRateVsRefMult       = NULL;
  mhBbcRateVsRefMult       = NULL;
  mhTofMultVsRefMult       = NULL;

  mhNTrk                   = NULL;
  mhTrkPt                  = NULL;
  mhTrkDcaVsPt             = NULL;
  mhTrkPhiVsPt             = NULL;
  mhTrkEtaVsPt             = NULL;
  mhTrkPhiEta              = NULL;
  mhTrkNHitsFitVsPt        = NULL;
  mhTrkNHitsPossVsPt       = NULL;
  mhTrkNHitsDedxVsPt       = NULL;
  mhTrkDedxVsMom           = NULL;
  mhTrkDedxVsPhi           = NULL;
  mhTrkNsigmaPiVsMom       = NULL;

  mhTrkBetaVsMom           = NULL;
  mhTrkM2VsMom             = NULL;
  mhTofMthTrkLocaly        = NULL;
  mhTofMthTrkLocalz        = NULL;

  mhMtdQTAdcAll            = NULL;
  mhMtdQTTacAll            = NULL;
  mhMtdQTAdcVsTacAll       = NULL;
  mhMtdQTJ2J3Diff          = NULL;
  mhMtdVpdTacDiffMT001     = NULL;
  mhMtdVpdTacDiffMT001Mth  = NULL;
  mhMtdVpdTacDiffMT001Muon = NULL;
  mhMtdVpdTacDiffMT101     = NULL;
  mhMtdVpdTacDiffMT101Mth  = NULL;
  mhMtdVpdTacDiffMT101Muon = NULL;
  mhNQtSignal              = NULL;
  mhNMT101Signal           = NULL;
  mhNTF201Signal           = NULL;
  for(int i=0; i<kNQTboard; i++)
    {
      for(int j=0; j<2; j++)
	mhMixMtdTacSumvsMxqMtdTacSum[i][j] = NULL;
    }
  for(int j=0; j<2; j++)
    mhMtdTriggerTime[j]    = NULL;

  mhMtdNRawHits            = NULL;
  mhMtdRawHitMap           = NULL;
  mhMtdRawHitLeTime        = NULL;
  mhMtdRawHitTrTime        = NULL;
  mhMtdRawHitLeNEast       = NULL;
  mhMtdRawHitLeNWest       = NULL;
  mhMtdRawHitTrNEast       = NULL;
  mhMtdRawHitTrNWest       = NULL;
  mhMtdRawHitLeNDiff       = NULL;
  mhMtdRawHitTrNDiff       = NULL;

  mhMtdNHits               = NULL;
  mhMtdHitMap              = NULL;
  mhMtdHitLeTimeDiff       = NULL;
  mhMtdHitTotWest          = NULL;
  mhMtdHitTotEast          = NULL;
  mhMtdHitTrigTime         = NULL;
  mhMtdHitTrigTimeTrkMth   = NULL;
  mhMtdTrigNHits           = NULL;
  mhMtdTrigHitMap          = NULL;
  mhMtdTrigMthNHits        = NULL;
  mhMtdTrigMthHitMap       = NULL;

  mhMtdNMatchHits          = NULL;
  mhMtdMatchHitMap         = NULL;
  mhMtdMatchTrkPt          = NULL;
  mhMtdMatchTrkPhiEta      = NULL;
  mhMtdMatchTrkPhiPt       = NULL;
  mhMtdMatchDzVsChan       = NULL;
  mhMtdMatchDzVsPtPos      = NULL;
  mhMtdMatchDzVsPtNeg      = NULL;
  mhMtdMatchDyVsChan       = NULL;
  mhMtdMatchDyVsPtPos      = NULL;
  mhMtdMatchDyVsPtNeg      = NULL;
  mhMtdMatchDtofVsPt       = NULL;
  mhMtdMatchMtdTofVsChan   = NULL;
  mhMtdMatchExpTofVsChan   = NULL;
  mhMtdMatchDtofVsChan     = NULL;
  mhMtdMatchLocalyVsChan   = NULL;
  mhMtdMatchLocalzVsChan   = NULL;

  mhNMuonPos               = NULL;
  mhNMuonNeg               = NULL;
  mhMuonPt                 = NULL;
  mhMuonPhiVsEta           = NULL;
  mhMuonMap                = NULL;
  mhNULpair                = NULL;
  mhNLSpairPos             = NULL;
  mhNLSpairNeg             = NULL;
  mhInvMvsPtUL             = NULL;
  mhInvMvsPtLSpos          = NULL;
  mhInvMvsPtLSneg          = NULL;

  mhBBCrateVsRun           = NULL;
  mhZDCrateVsRun           = NULL;
  mhRefMultVsRun           = NULL;
  mhgRefMultVsRun          = NULL;
  mhTpcVxVsRun             = NULL;
  mhTpcVyVsRun             = NULL;
  mhTpcVzVsRun             = NULL;
  mhVpdVzVsRun             = NULL;
  mhDiffVzVsRun            = NULL;
  mhpTrkPtVsRun            = NULL;
  mhpTrkEtaVsRun           = NULL;
  mhpTrkPhiVsRun           = NULL;
  mhpTrkDcaVsRun           = NULL;
  mhNHitsFitVsRun          = NULL;
  mhNHitsPossVsRun         = NULL;
  mhNHitsDedxVsRun         = NULL;
  mhDedxVsRun              = NULL;
  mhNsigmaPiVsRun          = NULL;
  mhNsigmaEVsRun           = NULL;
  mhNsigmaKVsRun           = NULL;
  mhNsigmaPVsRun           = NULL;
  mhBetaVsRun              = NULL;
  mhNMtdHitsVsRun          = NULL;
  mhNMtdTrigHitsVsRun      = NULL;
  mhNMtdMthHitsVsRun       = NULL;
  mhNMuonPosVsRun          = NULL;
  mhNMuonNegVsRun          = NULL;
  mhNMuonPairULVsRun       = NULL;
  mhNMuonPairLSPosVsRun    = NULL;
  mhNMuonPairLSNegVsRun    = NULL;
}
 
//_____________________________________________________________________________
StMtdQAMaker::~StMtdQAMaker()
{
  // default destructor
}

//_____________________________________________________________________________
Int_t StMtdQAMaker::Init()
{
  if(mPrintConfig) printConfig();
  bookHistos();
  return kStOK;
}

//_____________________________________________________________________________
Int_t StMtdQAMaker::Make()
{
  StTimer timer;
  if (mPrintMemory) StMemoryInfo::instance()->snapshot();
  if (mPrintCpu)    timer.start();

  // Check the availability of input data
  Int_t iret = -1;
  if(GetInputDS("StEvent"))
    {
      mStEvent = (StEvent*) GetInputDS("StEvent");
      if(mStEvent)
	{
	  iret = processStEvent();
	}
      else
	{
	  LOG_ERROR << "No StEvent is available ..." << endm;
	  return kStErr;
	}
    }
  else if(GetMaker("MuDst")) 
    {
      StMuDstMaker *muDstMaker = (StMuDstMaker*) GetMaker("MuDst");
      mMuDst = muDstMaker->muDst();
      if(mMuDst)
	{
	  iret = processMuDst();
	}
      else
	{
	  LOG_ERROR << "No muDST is available ... "<< endm;
	  iret = kStErr;
	}
    }
  else if(GetMaker("picoDst"))
    {
      StPicoDstMaker *picoDstMaker = (StPicoDstMaker*) GetMaker("picoDst");
      mPicoDst = picoDstMaker->picoDst();
      if(mPicoDst)
	{
	  iret = processPicoDst();
	}
      else
	{
	  LOG_ERROR << "No picoDst is available ..." << endm;
	  iret = kStErr;
	}
    }
  else
    {
      LOG_WARN << "No input data available ..." << endm;
      iret = kStWarn;
    }

  if (mPrintMemory) 
    {
      StMemoryInfo::instance()->snapshot();
      StMemoryInfo::instance()->print();
    }
  if (mPrintCpu)    
    {
      timer.stop();
      LOG_INFO << "CPU time for StMtdQAMaker::Make(): " 
	       << timer.elapsedTime() << "sec " << endm;
    }

  return iret;
}

//_____________________________________________________________________________
Int_t StMtdQAMaker::processStEvent()
{
  // Event statistics
  mhEventStat->Fill(0.5);
  return kStOK;
}


//_____________________________________________________________________________
Int_t StMtdQAMaker::processMuDst()
{
  mRunId   = mMuDst->event()->runId();
  mRunYear = mRunId/1000000 + 1999;
  int runIdFill = mRunId%1000000;
  
  int mass_east = mMuDst->event()->runInfo().beamMassNumber(east);
  int mass_west = mMuDst->event()->runInfo().beamMassNumber(west);
  if(mass_east == mass_west)
    {
      if(mass_east == 1) mCollisionSystem = "pp";
      else mCollisionSystem = "AA";
    }
  else
    {
      mCollisionSystem = "pA";
    }

  // Event statistics
  mhEventStat->Fill(0.5);
  mhRunId->Fill(runIdFill);

  //========== select valid triggers ==========
  Bool_t isGoodTrigger = kFALSE; 
  Int_t nTrig = mTriggerIDs.size();
  if(nTrig==0) 
    {
      isGoodTrigger = kTRUE;
    }
  else
    {
      for(Int_t i=0; i<nTrig; i++)
	{
	  if(mMuDst->event()->triggerIdCollection().nominal().isTrigger(mTriggerIDs[i]))
	    {
	      isGoodTrigger = kTRUE;
	      break;
	    }
	}
    }
  if(!isGoodTrigger) return kStOK;
  mhEventStat->Fill(1.5);

  //========== Select vertex ==========
  int vtxIndex = -1;
  Int_t nPrim = mMuDst->numberOfPrimaryVertices();
  if(nPrim == 0) 
    {
      LOG_WARN << "No reconstructed vertex in MuDst... " << endm;
      return kStWarn;
    }

  // start time & VPD vz
  StBTofHeader *tofHeader = mMuDst->btofHeader();
  Double_t vpdvz   = -999;
  if(tofHeader)
    {
      vpdvz  = tofHeader->vpdVz();
      mhEventStat->Fill(2.5);
    }

  int index = -1;
  if(tofHeader)
    {
      // constrain vertex with VPD
      Double_t min_dz = 999;
      for(Int_t i=0; i<nPrim; i++)
	{
	  StMuPrimaryVertex *vertex = mMuDst->primaryVertex(i);
	  double ranking = vertex->ranking();
	  if(mCollisionSystem=="pp" && ranking<0) continue;
	  Double_t dz = fabs(vertex->position().z()-vpdvz);
	  if(dz<min_dz)
	    {
	      min_dz = dz;
	      index = i;
	    }
	}
      mhVtxClosestIndex->Fill(index);
	  
      double default_z = mMuDst->primaryVertex(0)->position().z();
      mhVtxZvsVpdVzDefault->Fill(default_z, vpdvz);
      mhVtxZDiffDefault->Fill(default_z - vpdvz);
      if(index>-1)
	{
	  double cloest_z = mMuDst->primaryVertex(index)->position().z();
	  mhVtxZvsVpdVzClosest->Fill(cloest_z, vpdvz);
	  mhVtxZDiffClosest->Fill(cloest_z - vpdvz);
	}
    }
      
  if(mVertexMode==0) vtxIndex = 0;
  else if(mVertexMode==1) vtxIndex = index;
  else
    {
      LOG_WARN << "No vertex mode is set. Use default vertex!" << endm;
      vtxIndex = 0;
    }

  if(vtxIndex<0) return kStWarn;
  mMuDst->setVertexIndex(vtxIndex);
  StMuPrimaryVertex* priVertex = mMuDst->primaryVertex();
  if(!priVertex) return kStWarn;
  StThreeVectorF verPos = priVertex->position();
  double tpcvz = verPos.z();
  mhVertexXY->Fill(verPos.y(), verPos.x());
  mhVertexXZ->Fill(verPos.z(), verPos.x());
  mhVertexYZ->Fill(verPos.z(), verPos.y());
  mhVertexZ->Fill(tpcvz);
  mhVpdVz->Fill(vpdvz);
  mhVtxZDiff->Fill(tpcvz-vpdvz);
  mhVtxZDiffVsTpcVz->Fill(tpcvz, tpcvz-vpdvz);
  if(fabs(tpcvz)>mMaxVtxZ)        return kStOK;
  if(sqrt(verPos.x()*verPos.x()+verPos.y()*verPos.y())>mMaxVtxR) return kStOK;
  if(mApplyVtxDzCut && fabs(tpcvz-vpdvz)>mMaxVtxDz) return kStOK;
  mhEventStat->Fill(3.5);
  for(UInt_t i=0; i<mTriggerIDs.size(); i++)
    {
      if(mMuDst->event()->triggerIdCollection().nominal().isTrigger(mTriggerIDs[i]))
	{
	  mhEventStat->Fill(4.5+i);
	}
    }
  mhTpcVxVsRun->Fill(runIdFill, verPos.x());
  mhTpcVyVsRun->Fill(runIdFill, verPos.y());
  mhTpcVzVsRun->Fill(runIdFill, tpcvz);
  mhVpdVzVsRun->Fill(runIdFill, vpdvz);
  mhDiffVzVsRun->Fill(runIdFill, tpcvz-vpdvz);

  //================= reference multiplicity ===================
  StMuEvent *muEvent = mMuDst->event();
  StRunInfo runInfo = muEvent->runInfo();
  Int_t refMult  = muEvent->refMult();
  Int_t gRefMult = muEvent->grefmult();
  Double_t bbcRate = runInfo.bbcCoincidenceRate() * 1e-3; // kHz
  Double_t zdcRate = runInfo.zdcCoincidenceRate() * 1e-3; // kHz
  Int_t tofMult = mMuDst->numberOfBTofHit();
  mhZdcRate->Fill(zdcRate);
  mhBbcRate->Fill(bbcRate);
  mhRefMult->Fill(refMult);
  mhgRefMult->Fill(gRefMult);
  mhgRefMultVsRefMult->Fill(refMult, gRefMult);
  mhTpcVzVsRefMult   ->Fill(refMult, verPos.z());
  mhDiffVzVsRefMult  ->Fill(refMult, verPos.z()-vpdvz);
  mhZdcRateVsRefMult ->Fill(refMult, zdcRate);
  mhBbcRateVsRefMult ->Fill(refMult, bbcRate);
  mhTofMultVsRefMult ->Fill(refMult, tofMult);
  mhBBCrateVsRun->Fill(runIdFill, bbcRate);
  mhZDCrateVsRun->Fill(runIdFill, zdcRate);
  mhRefMultVsRun->Fill(runIdFill, refMult);
  mhgRefMultVsRun->Fill(runIdFill, gRefMult);

  //================= primary tracks ===================
  Int_t nGoodTrack = 0;
  vector<int> muonId;
  muonId.clear();
  Int_t nPosMuon = 0, nNegMuon = 0;
  Int_t nTrks = mMuDst->numberOfPrimaryTracks();
  map<Short_t, UShort_t> trackIndex;
  for(Int_t i=0; i<nTrks; i++)
    {
      StMuTrack* pTrack = mMuDst->primaryTracks(i);
      if(!pTrack) continue;
      if(!isValidTrack(pTrack)) continue;
      trackIndex[pTrack->id()] = i;
      nGoodTrack++;
      Int_t charge       = pTrack->charge();
      Double_t p         = pTrack->p().mag();
      Double_t pt        = pTrack->pt();
      Double_t eta       = pTrack->eta();
      Double_t phi       = rotatePhi(pTrack->phi());
      Double_t dca       = pTrack->dcaGlobal().mag();
      Int_t nHitsFit     = pTrack->nHitsFit(kTpcId);
      Int_t nHitsPoss    = pTrack->nHitsPoss(kTpcId);
      Int_t nHitsDedx    = pTrack->nHitsDedx();
      Double_t dedx      = pTrack->dEdx() * 1e6;
      Double_t nSigmaE   = pTrack->nSigmaElectron();
      Double_t nSigmaPi  = pTrack->nSigmaPion();
      Double_t nSigmaK   = pTrack->nSigmaKaon();
      Double_t nSigmaP   = pTrack->nSigmaProton();
      mhTrkPt            ->Fill(pt);
      mhTrkDcaVsPt       ->Fill(pt, dca);
      mhTrkPhiVsPt       ->Fill(pt, phi);
      mhTrkEtaVsPt       ->Fill(pt, eta);
      mhTrkPhiEta        ->Fill(eta, phi);
      mhTrkNHitsFitVsPt  ->Fill(pt, nHitsFit);
      mhTrkNHitsPossVsPt ->Fill(pt, nHitsPoss);
      mhTrkNHitsDedxVsPt ->Fill(pt, nHitsDedx);
      mhTrkDedxVsMom     ->Fill(p, dedx);
      mhTrkDedxVsPhi     ->Fill(phi, dedx);
      mhTrkNsigmaPiVsMom ->Fill(p, nSigmaPi);

      mhpTrkPtVsRun      ->Fill(runIdFill, pt);
      mhpTrkEtaVsRun     ->Fill(runIdFill, eta);
      mhpTrkPhiVsRun     ->Fill(runIdFill, phi);
      mhpTrkDcaVsRun     ->Fill(runIdFill, dca);
      mhNHitsFitVsRun    ->Fill(runIdFill, nHitsFit);
      mhNHitsPossVsRun   ->Fill(runIdFill, nHitsPoss);
      mhNHitsDedxVsRun   ->Fill(runIdFill, nHitsDedx);
      mhDedxVsRun        ->Fill(runIdFill, dedx);
      mhNsigmaPiVsRun    ->Fill(runIdFill, nSigmaPi);
      mhNsigmaEVsRun     ->Fill(runIdFill, nSigmaE);
      mhNsigmaKVsRun     ->Fill(runIdFill, nSigmaK);
      mhNsigmaPVsRun     ->Fill(runIdFill, nSigmaP);

      // TOF matching
      const StMuBTofHit *tofHit = pTrack->tofHit();
      if(tofHit) 
	{
	  const StMuBTofPidTraits &tofPid = pTrack->btofPidTraits();
	  double beta = tofPid.beta();
	  if(beta!=0)
	    {
	      Double_t m2 = pow(p,2) * (1/pow(beta,2)-1);
	      mhTrkBetaVsMom->Fill(p, 1./beta);
	      mhTrkM2VsMom->Fill(p, m2);
	      mhBetaVsRun->Fill(runIdFill, 1./beta);
	    }
	  Int_t tofTray = tofHit->tray();
	  Int_t tofModule = tofHit->module();
	  if(tofTray>60 && tofTray<=120) tofModule += 32;
	  mhTofMthTrkLocaly->Fill(tofTray,tofPid.yLocal());
	  mhTofMthTrkLocalz->Fill(tofModule,tofPid.zLocal());
	}

      // MTD matching
      Int_t iMtd = pTrack->index2MtdHit();
      if(iMtd>-1)
	{
	  StMuMtdHit *hit = mMuDst->mtdHit(iMtd);
	  Int_t backleg = hit->backleg();
	  Int_t module  = hit->module();
	  Int_t cell    = hit->cell();
	  Double_t gChan   = (backleg-1)*60 + (module-1)*12 + cell;
	  const StMuMtdPidTraits mtdPid = pTrack->mtdPidTraits();
	  Double_t dy     = mtdPid.deltaY();
	  Double_t dz     = mtdPid.deltaZ();
	  Double_t localy = mtdPid.yLocal();
	  Double_t localz = mtdPid.zLocal();
	  Double_t mtdtof = mtdPid.timeOfFlight();
	  Double_t exptof = mtdPid.expTimeOfFlight();
	  Double_t dtof   = mtdtof - exptof;

	  mhMtdMatchTrkPt                 ->Fill(pt);
	  mhMtdMatchTrkPhiEta             ->Fill(eta, phi);
	  mhMtdMatchTrkPhiPt              ->Fill(pt, phi);
	  mhMtdMatchDzVsChan              ->Fill(gChan, dz);
	  if(charge>0) mhMtdMatchDzVsPtPos->Fill(pt, dz);
	  if(charge<0) mhMtdMatchDzVsPtNeg->Fill(pt, dz);
	  mhMtdMatchDyVsChan              ->Fill(gChan, dy);
	  if(charge>0) mhMtdMatchDyVsPtPos->Fill(pt, dy);
	  if(charge<0) mhMtdMatchDyVsPtNeg->Fill(pt, dy);
	  mhMtdMatchDtofVsPt              ->Fill(pt, dtof);
	  mhMtdMatchMtdTofVsChan          ->Fill(gChan, mtdtof);
	  mhMtdMatchExpTofVsChan          ->Fill(gChan, exptof);
	  mhMtdMatchDtofVsChan            ->Fill(gChan, dtof);
	  mhMtdMatchLocalyVsChan          ->Fill(gChan, localy);
	  mhMtdMatchLocalzVsChan          ->Fill(gChan, localz);

	  Int_t tacDiffQT = 0, tacDiffMT101 = 0;
	  Int_t qt = 0, pos = 0, bin = 0;
	  if(mTrigUtil)
	    {
	      tacDiffQT = mTrigUtil->getHitTimeDiffToVPDInQT(backleg, module);
	      tacDiffMT101 = mTrigUtil->getHitTimeDiffToVPDInMT101(backleg, module);
	      qt = mTrigUtil->getQt(backleg, module);
	      pos = mTrigUtil->getQtPos(backleg, module);
	      if(mRunYear!=2016) bin = (qt-1)*8+pos;
	      else               bin = (qt-1)*4+(pos-1)/2+1;
	      mhMtdVpdTacDiffMT001Mth->Fill(bin, tacDiffQT);
	      mhMtdVpdTacDiffMT101Mth->Fill(bin, tacDiffMT101);
	      if(isMuonCandidate(pt, nSigmaPi, dz, dy, dtof, kTRUE))
		{
		  mhMtdVpdTacDiffMT001Muon->Fill(bin, tacDiffQT);
		  mhMtdVpdTacDiffMT101Muon->Fill(bin, tacDiffMT101);
		}
	    }
	  
	  if(isMuonCandidate(pTrack))
	    {
	      if(charge>0) nPosMuon++;
	      else         nNegMuon++;
	      mhMuonPt->Fill(pt);
	      mhMuonPhiVsEta->Fill(eta, phi);
	      mhMuonMap->Fill(backleg, (module-1)*12+cell+1);
	      muonId.push_back(i);
	    }
	}
    }
  mhNTrk->Fill(nGoodTrack);
  mhNMuonPos->Fill(nPosMuon);
  mhNMuonNeg->Fill(nNegMuon);
  mhNMuonPosVsRun->Fill(runIdFill, nPosMuon);
  mhNMuonNegVsRun->Fill(runIdFill, nNegMuon);

  //================= MTD trigger time ===================
  StMuMtdHeader *muMtdHeader = mMuDst->mtdHeader();
  int mtdTrigTime[2] = {0, 0};
  if(muMtdHeader)
    {
      for(Int_t i=0; i<2; i++)
	{
	  mtdTrigTime[i] = 25.*(muMtdHeader->triggerTime(i+1)&0xfff);
	  mhMtdTriggerTime[i]->Fill(mtdTrigTime[i]);
	}
    }

  //================= Trigger performance ===========
  StTriggerData* trigData = const_cast<StTriggerData*>(muEvent->triggerData());
  Int_t mtdQTtac[kNQTboard][16];
  Int_t mtdQTadc[kNQTboard][16];
  for(Int_t i=0; i<32; i++)
    {
      Int_t type = (i/4)%2;
      if(mRunYear<=2015)
	{
	  if(type==1)
	    {
	      mtdQTtac[0][i-i/4*2-2] = trigData->mtdAtAddress(i,0);
	      mtdQTtac[1][i-i/4*2-2] = trigData->mtdgemAtAddress(i,0);
	      mtdQTtac[2][i-i/4*2-2] = trigData->mtd3AtAddress(i,0);
	      mtdQTtac[3][i-i/4*2-2] = trigData->mtd4AtAddress(i,0);
	    }
	  else
	    {
	      mtdQTadc[0][i-i/4*2] = trigData->mtdAtAddress(i,0);
	      mtdQTadc[1][i-i/4*2] = trigData->mtdgemAtAddress(i,0);
	      mtdQTadc[2][i-i/4*2] = trigData->mtd3AtAddress(i,0);
	      mtdQTadc[3][i-i/4*2] = trigData->mtd4AtAddress(i,0);
	    }
	}
      else
	{
	  for(int im=0; im<kNQTboard; im++)
	    {
	      if(type==0) mtdQTadc[im][i-i/4*2] = trigData->mtdQtAtCh(im+1,i,0);
	      else        mtdQTtac[im][i-i/4*2-2] = trigData->mtdQtAtCh(im+1,i,0);
	    }
	}
    }

  if(mTrigUtil)
    {
      Int_t nQTsignal = 0;
      Int_t vpdTacSum = mTrigUtil->getVpdTacSum();
      for(Int_t im=0; im<kNQTboard; im++)
	{
	  for(Int_t i=0; i<8; i++)
	    {
	      Int_t mtdTacSum = mTrigUtil->getQtTacSum(im+1, i+1);
	      if(mtdTacSum<=10) continue;
	      nQTsignal++;

	      for(Int_t k=0; k<2; k++)
		{	      
		  int index = 0;
		  if(mRunYear!=2016) index = im*16 + i*2 + k + 1;
		  else               index = im*8 + (i/2)*2 + k + 1;
		  mhMtdQTAdcAll->Fill(index, mtdQTadc[im][i*2+k]);
		  mhMtdQTTacAll->Fill(index, mtdQTtac[im][i*2+k]);
		  mhMtdQTAdcVsTacAll->Fill(mtdQTtac[im][i*2+k], mtdQTadc[im][i*2+k]);
		}

	      int bin = 0;
	      if(mRunYear!=2016) bin = im*8+i+1;
	      else               bin = im*4+i/2+1;
	      mhMtdQTJ2J3Diff->Fill(bin,mtdQTtac[im][i*2]-mtdQTtac[im][i*2+1]);
	      mhMtdVpdTacDiffMT001->Fill(bin, mtdTacSum/8 - vpdTacSum/8 + 1024);
	    }
	}
      mhNQtSignal->Fill(nQTsignal);

      Int_t nMT101signal = 0;
      for(Int_t im=0; im<kNQTboard; im++)
	{
	  for(Int_t j=0; j<2; j++)
	    {
	      Int_t mix_tacSum = mTrigUtil->getMT101Tac(im+1, j);
	      if(mix_tacSum>0) 
		{
		  nMT101signal++;
		  Int_t mxq_tacSum = mTrigUtil->getQtTacSumHighestTwo(im+1, j);
		  Int_t mxq_tacPos = mTrigUtil->getQtPosHighestTwo(im+1, j);
		  mhMixMtdTacSumvsMxqMtdTacSum[im][j]->Fill(mxq_tacSum/8,mix_tacSum);
		  int bin = 0;
		  if(mRunYear!=2016) bin = im*8+mxq_tacPos;
		  else               bin = im*4+mxq_tacPos/2;
		  mhMtdVpdTacDiffMT101->Fill(bin,mix_tacSum-vpdTacSum/8+1024);
		}
	    }
	}
      mhNMT101Signal->Fill(nMT101signal);

      Int_t nTF201signal = 0;
      Int_t decision = mTrigUtil->getTF201TriggerBit();
      for(Int_t i=0; i<4; i++)
	{
	  for(Int_t j=0; j<2; j++)
	    {
	      if((decision>>(i*2+j))&0x1)
		nTF201signal++;
	    }
	}
      mhNTF201Signal->Fill(nTF201signal);
    }
  

  //================= MTD raw hits ===================
  Int_t nMtdRawHits = mMuDst->numberOfBMTDRawHit();
  mhMtdNRawHits->Fill(nMtdRawHits);
  Int_t nDiffLe[gMtdNBacklegs * gMtdNModules * gMtdNCells] = {0};
  Int_t nDiffTr[gMtdNBacklegs * gMtdNModules * gMtdNCells] = {0};
  for(Int_t i=0; i<nMtdRawHits; i++)
    {
      StMuMtdRawHit *rawHit = (StMuMtdRawHit*)mMuDst->mtdRawHit(i);
      if(!rawHit) continue;
      Int_t backleg = rawHit->backleg();
      Int_t channel = rawHit->channel();
      Int_t module  = (channel-1)/gMtdNChannels+1;
      if(backleg<1 || backleg>30) continue;
      Int_t gChan = (backleg-1)*120 + channel;
      Int_t localChan = channel - (module-1) * 24;
      Int_t gCell = (backleg-1)*60 + (module-1)*12 + localChan;
      Int_t flag = rawHit->flag();
      Double_t tdc = rawHit->tdc()*gMtdConvertTdcToNs;

      mhMtdRawHitMap->Fill(backleg, channel);
      if(flag>0) mhMtdRawHitLeTime->Fill(gChan, tdc);
      else       mhMtdRawHitTrTime->Fill(gChan, tdc);
     if(localChan <= 12)
	{
	  if(flag>0)  { mhMtdRawHitLeNWest->Fill(gCell); nDiffLe[gCell-1]++; }
	  else        { mhMtdRawHitTrNWest->Fill(gCell); nDiffTr[gCell-1]++; }
	} 
      else if (localChan > 12 && localChan <= 24)
	{								
	  gCell -= 12;
	  if(flag>0)  { mhMtdRawHitLeNEast->Fill(gCell); nDiffLe[gCell-1]--; }
	  else        { mhMtdRawHitTrNEast->Fill(gCell); nDiffTr[gCell-1]--; }
	}
      else
	{
	  LOG_WARN << "Weird local channel number: " << localChan << " from global channel " << channel << " and module " << module << endm;
	}
    }
  for(Int_t i=0; i<gMtdNBacklegs * gMtdNModules * gMtdNCells; i++)
    {
      mhMtdRawHitLeNDiff->Fill(i+1,nDiffLe[i]);
      mhMtdRawHitTrNDiff->Fill(i+1,nDiffTr[i]);
    }

  //================= MTD hits ===================
  int nMtdHits = mMuDst->numberOfMTDHit();
  int mMthMtdHit = 0, nTrigMtdHit = 0, nTrigMtdHitMth = 0;
  mhMtdNHits->Fill(nMtdHits);
  for(Int_t i=0; i<nMtdHits; i++)
    {
      StMuMtdHit *hit = mMuDst->mtdHit(i);
      if(!hit) continue;
      Int_t backleg   = hit->backleg();
      Int_t module    = hit->module();
      Int_t cell      = hit->cell();
      Int_t lChan     = (module-1)*12+cell;
      Int_t gChan     = (backleg-1)*60 + lChan;
      Int_t tHub      = getMtdHitTHUB(backleg);
      Double_t tDiff = (hit->leadingEdgeTime().first+hit->leadingEdgeTime().second)/2 - mtdTrigTime[tHub-1];
      while(tDiff<0) tDiff += 51200;
      Bool_t isMth    = kFALSE;
      Short_t trackId = hit->associatedTrackKey();
      if(trackId>0)
	{
	  Int_t index = (trackIndex.find(trackId)!=trackIndex.end()) ? trackIndex.find(trackId)->second : -1;
	  if(index>-1)
	    {
	      StMuTrack *pTrack = mMuDst->primaryTracks(index);
	      if(pTrack && isValidTrack(pTrack)) isMth = kTRUE;
	    }
	}
      Bool_t isTrig =  mTrigUtil ? mTrigUtil->isHitFireTrigger(hit->backleg(), hit->module()) : kFALSE;

      mhMtdHitMap       ->Fill(backleg,lChan);
      mhMtdHitLeTimeDiff->Fill(gChan, hit->leadingEdgeTime().first-hit->leadingEdgeTime().second);
      mhMtdHitTotWest   ->Fill(gChan, hit->tot().first);
      mhMtdHitTotEast   ->Fill(gChan, hit->tot().second);
      mhMtdHitTrigTime  ->Fill(gChan, tDiff);

      if(isMth)
	{
	  mMthMtdHit++;
	  mhMtdHitTrigTimeTrkMth->Fill(gChan,tDiff);
	  mhMtdMatchHitMap->Fill(backleg,lChan);
	}
      if(isTrig)
	{
	  nTrigMtdHit ++;
	  mhMtdTrigHitMap->Fill(backleg,lChan);
	  if(isMth)
	    {
	      nTrigMtdHitMth++;
	      mhMtdTrigMthHitMap->Fill(backleg,lChan);
	    }
	}
    }
  mhMtdNMatchHits     ->Fill(mMthMtdHit);
  mhMtdTrigNHits      ->Fill(nTrigMtdHit);
  mhMtdTrigMthNHits   ->Fill(nTrigMtdHitMth);
  mhNMtdHitsVsRun     ->Fill(runIdFill, nMtdHits);
  mhNMtdTrigHitsVsRun ->Fill(runIdFill, nTrigMtdHit);
  mhNMtdMthHitsVsRun  ->Fill(runIdFill, mMthMtdHit);

  //================= muon analysis ===================
  int nULpair = 0, nLSpairPos = 0, nLSpairNeg = 0;
  UInt_t nMuon = muonId.size();
  for(UInt_t i=0; i<nMuon; i++)
    {
      StMuTrack *ipTrack = mMuDst->primaryTracks(muonId[i]); 
      int iq = ipTrack->charge();
      const StThreeVectorF imom = ipTrack->momentum();
      TLorentzVector imuon;
      imuon.SetXYZM(imom.x(),imom.y(),imom.z(),0.10566);
      for(UInt_t j=i+1; j<nMuon; j++)
	{
	  StMuTrack *jpTrack = mMuDst->primaryTracks(muonId[j]);
	  int jq = jpTrack->charge();
	  const StThreeVectorF jmom = jpTrack->momentum();
	  TLorentzVector jmuon;
	  jmuon.SetXYZM(jmom.x(),jmom.y(),jmom.z(),0.10566);

	  float pt1 = imom.perp(), pt2 = jmom.perp();
	  if(pt1<pt2) 
	    {
	      pt1 = jmom.perp();
	      pt2 = imom.perp();
	    }

	  if(pt1<1.5) continue;
	  TLorentzVector jpsi = imuon + jmuon;
	  Double_t invmass = jpsi.M();
	  if(iq*jq<0)
	    {
	      nULpair++;
	      mhInvMvsPtUL->Fill(invmass, jpsi.Pt());
	      mhInvMUL->Fill(invmass);
	    }
	  else 
	    {
	      if(iq>0)
		{
		  nLSpairPos++;
		  mhInvMvsPtLSpos->Fill(invmass, jpsi.Pt());
		  mhInvMLSpos->Fill(invmass);
		}
	      else
		{
		  nLSpairNeg++;
		  mhInvMvsPtLSneg->Fill(invmass, jpsi.Pt());
		  mhInvMLSneg->Fill(invmass);
		}
	    }
	}
    }
  mhNULpair            ->Fill(nULpair); 
  mhNLSpairPos         ->Fill(nLSpairPos); 
  mhNLSpairNeg         ->Fill(nLSpairNeg);
  mhNMuonPairULVsRun   ->Fill(runIdFill, nULpair);
  mhNMuonPairLSPosVsRun->Fill(runIdFill, nLSpairPos);
  mhNMuonPairLSNegVsRun->Fill(runIdFill, nLSpairNeg);

  return kStOK;
}


//_____________________________________________________________________________
Int_t StMtdQAMaker::processPicoDst()
{
  StPicoEvent *picoEvent = mPicoDst->event();
  mRunId   = picoEvent->runId();
  mRunYear = mRunId/1000000 + 1999;
  int runIdFill = mRunId%1000000;

  // Event statistics
  mhEventStat->Fill(0.5);
  mhRunId->Fill(runIdFill);

  //========== select valid triggers ==========
  Bool_t isGoodTrigger = kFALSE; 
  Int_t nTrig = mTriggerIDs.size();
  if(nTrig==0) 
    {
      isGoodTrigger = kTRUE;
    }
  else
    {
      for(Int_t i=0; i<nTrig; i++)
	{
	  if(picoEvent->isTrigger(mTriggerIDs[i]))
	    {
	      isGoodTrigger = kTRUE;
	      break;
	    }
	}
    }
  if(!isGoodTrigger) return kStOK;
  mhEventStat->Fill(1.5);

  //========== Select vertex ==========
  Double_t vpdvz = picoEvent->vzVpd();
  if(fabs(vpdvz)>300)
    {
      return kStOK;
    }
  mhEventStat->Fill(2.5);
  TVector3 verPos = picoEvent->primaryVertex();
  double tpcvz = verPos.z();
  mhVertexXY->Fill(verPos.y(), verPos.x());
  mhVertexXZ->Fill(verPos.z(), verPos.x());
  mhVertexYZ->Fill(verPos.z(), verPos.y());
  mhVertexZ->Fill(tpcvz);
  mhVpdVz->Fill(vpdvz);
  mhVtxZDiff->Fill(tpcvz-vpdvz);
  mhVtxZDiffVsTpcVz->Fill(tpcvz, tpcvz-vpdvz);
  if(fabs(tpcvz)>mMaxVtxZ)        return kStOK;
  if(sqrt(verPos.x()*verPos.x()+verPos.y()*verPos.y())>mMaxVtxR) return kStOK; 
  if(mApplyVtxDzCut && fabs(tpcvz-vpdvz)>mMaxVtxDz) return kStOK;
  mhEventStat->Fill(3.5);
  for(UInt_t i=0; i<mTriggerIDs.size(); i++)
    {
      if(picoEvent->isTrigger(mTriggerIDs[i]))
	{
	  mhEventStat->Fill(4.5+i);
	}
    }
  mhTpcVxVsRun->Fill(runIdFill, verPos.x());
  mhTpcVyVsRun->Fill(runIdFill, verPos.y());
  mhTpcVzVsRun->Fill(runIdFill, tpcvz);
  mhVpdVzVsRun->Fill(runIdFill, vpdvz);
  mhDiffVzVsRun->Fill(runIdFill, tpcvz-vpdvz);

  //================= reference multiplicity ===================
  Int_t refMult  = picoEvent->refMult();
  Int_t gRefMult = picoEvent->grefMult();
  Double_t bbcRate = picoEvent->BBCx() * 1e-3; // kHz
  Double_t zdcRate = picoEvent->ZDCx() * 1e-3; // kHz
  Int_t tofMult = mPicoDst->numberOfBTofHits();
  mhZdcRate->Fill(zdcRate);
  mhBbcRate->Fill(bbcRate);
  mhRefMult->Fill(refMult);
  mhgRefMult->Fill(gRefMult);
  mhgRefMultVsRefMult->Fill(refMult, gRefMult);
  mhTpcVzVsRefMult   ->Fill(refMult, verPos.z());
  mhDiffVzVsRefMult  ->Fill(refMult, verPos.z()-vpdvz);
  mhZdcRateVsRefMult ->Fill(refMult, zdcRate);
  mhBbcRateVsRefMult ->Fill(refMult, bbcRate);
  mhTofMultVsRefMult ->Fill(refMult, tofMult);
  mhBBCrateVsRun->Fill(runIdFill, bbcRate);
  mhZDCrateVsRun->Fill(runIdFill, zdcRate);
  mhRefMultVsRun->Fill(runIdFill, refMult);
  mhgRefMultVsRun->Fill(runIdFill, gRefMult);

  //================= primary tracks ===================
  Int_t nGoodTrack = 0;
  vector<int> muonId;
  muonId.clear();
  Int_t nPosMuon = 0, nNegMuon = 0;
  Int_t nTrks = mPicoDst->numberOfTracks();
  for(Int_t i=0; i<nTrks; i++)
    {
      StPicoTrack* pTrack = mPicoDst->track(i);
      if(!pTrack) continue;
      TVector3 mom = pTrack->pMom();
      if(mom.Mag()<=0) continue;
      if(!isValidTrack(pTrack)) continue;
      nGoodTrack++;
      Int_t charge       = pTrack->charge();
      Double_t p         = mom.Mag();
      Double_t pt        = mom.Perp();
      Double_t eta       = mom.Eta();
      Double_t phi       = rotatePhi(mom.Phi());
      Double_t dca       = pTrack->gDCA(picoEvent->primaryVertex()).Mag();
      Int_t nHitsFit     = pTrack->nHitsFit();
      Int_t nHitsPoss    = pTrack->nHitsMax();
      Int_t nHitsDedx    = pTrack->nHitsDedx();
      Double_t dedx      = pTrack->dEdx();
      Double_t nSigmaE   = pTrack->nSigmaElectron();
      Double_t nSigmaPi  = pTrack->nSigmaPion();
      Double_t nSigmaK   = pTrack->nSigmaKaon();
      Double_t nSigmaP   = pTrack->nSigmaProton();
      mhTrkPt            ->Fill(pt);
      mhTrkDcaVsPt       ->Fill(pt, dca);
      mhTrkPhiVsPt       ->Fill(pt, phi);
      mhTrkEtaVsPt       ->Fill(pt, eta);
      mhTrkPhiEta        ->Fill(eta, phi);
      mhTrkNHitsFitVsPt  ->Fill(pt, nHitsFit);
      mhTrkNHitsPossVsPt ->Fill(pt, nHitsPoss);
      mhTrkNHitsDedxVsPt ->Fill(pt, nHitsDedx);
      mhTrkDedxVsMom     ->Fill(p, dedx);
      mhTrkDedxVsPhi     ->Fill(phi, dedx);
      mhTrkNsigmaPiVsMom ->Fill(p, nSigmaPi);

      mhpTrkPtVsRun      ->Fill(runIdFill, pt);
      mhpTrkEtaVsRun     ->Fill(runIdFill, eta);
      mhpTrkPhiVsRun     ->Fill(runIdFill, phi);
      mhpTrkDcaVsRun     ->Fill(runIdFill, dca);
      mhNHitsFitVsRun    ->Fill(runIdFill, nHitsFit);
      mhNHitsPossVsRun   ->Fill(runIdFill, nHitsPoss);
      mhNHitsDedxVsRun   ->Fill(runIdFill, nHitsDedx);
      mhDedxVsRun        ->Fill(runIdFill, dedx);
      mhNsigmaPiVsRun    ->Fill(runIdFill, nSigmaPi);
      mhNsigmaEVsRun     ->Fill(runIdFill, nSigmaE);
      mhNsigmaKVsRun     ->Fill(runIdFill, nSigmaK);
      mhNsigmaPVsRun     ->Fill(runIdFill, nSigmaP);

      // TOF matching
      if(pTrack->bTofPidTraitsIndex()>-1)
	{
	  StPicoBTofPidTraits *tofPid = mPicoDst->btofPidTraits(pTrack->bTofPidTraitsIndex());
	  double beta = tofPid->btofBeta();
	  if(beta!=0)
	    {
	      Double_t m2 = pow(p,2) * (1/pow(beta,2)-1);
	      mhTrkBetaVsMom->Fill(p, 1./beta);
	      mhTrkM2VsMom->Fill(p, m2);
	      mhBetaVsRun->Fill(runIdFill, 1./beta);
	    }
	  Int_t tofTray = tofPid->btofCellId()/192+1;
	  Int_t tofModule = tofPid->btofCellId()%192/6+1;
	  if(tofTray>60 && tofTray<=120) tofModule += 32;
	  mhTofMthTrkLocaly->Fill(tofTray,tofPid->btofYLocal());
	  mhTofMthTrkLocalz->Fill(tofModule,tofPid->btofZLocal());
	}

      // MTD matching
      int iMtd = pTrack->mtdPidTraitsIndex();
      if(iMtd>-1)
	{
	  StPicoMtdPidTraits *mtdPid = mPicoDst->mtdPidTraits(iMtd);
	  Int_t backleg = mtdPid->backleg();
	  Int_t module  = mtdPid->module();
	  Int_t cell    = mtdPid->cell();
	  Double_t gChan   = (backleg-1)*60 + (module-1)*12 + cell;
	  Double_t dy     = mtdPid->deltaY();
	  Double_t dz     = mtdPid->deltaZ();
	  Double_t dtof   = mtdPid->deltaTimeOfFlight();

	  mhMtdMatchTrkPt                 ->Fill(pt);
	  mhMtdMatchTrkPhiEta             ->Fill(eta, phi);
	  mhMtdMatchTrkPhiPt              ->Fill(pt, phi);
	  mhMtdMatchDzVsChan              ->Fill(gChan, dz);
	  if(charge>0) mhMtdMatchDzVsPtPos->Fill(pt, dz);
	  if(charge<0) mhMtdMatchDzVsPtNeg->Fill(pt, dz);
	  mhMtdMatchDyVsChan              ->Fill(gChan, dy);
	  if(charge>0) mhMtdMatchDyVsPtPos->Fill(pt, dy);
	  if(charge<0) mhMtdMatchDyVsPtNeg->Fill(pt, dy);
	  mhMtdMatchDtofVsPt              ->Fill(pt, dtof);
	  mhMtdMatchDtofVsChan            ->Fill(gChan, dtof);

	  Int_t tacDiffQT = 0, tacDiffMT101 = 0;
	  Int_t qt = 0, pos = 0, bin = 0;
	  if(mTrigUtil)
	    {
	      tacDiffQT = mTrigUtil->getHitTimeDiffToVPDInQT(backleg, module);
	      tacDiffMT101 = mTrigUtil->getHitTimeDiffToVPDInMT101(backleg, module);
	      qt = mTrigUtil->getQt(backleg, module);
	      pos = mTrigUtil->getQtPos(backleg, module);
	      if(mRunYear!=2016) bin = (qt-1)*8+pos;
	      else               bin = (qt-1)*4+(pos-1)/2+1;
	      mhMtdVpdTacDiffMT001Mth->Fill(bin, tacDiffQT);
	      mhMtdVpdTacDiffMT101Mth->Fill(bin, tacDiffMT101);
	      if(isMuonCandidate(pt, nSigmaPi, dz, dy, dtof, kTRUE))
		{
		  mhMtdVpdTacDiffMT001Muon->Fill(bin, tacDiffQT);
		  mhMtdVpdTacDiffMT101Muon->Fill(bin, tacDiffMT101);
		}
	    }
	  
	  if(isMuonCandidate(pTrack))
	    {
	      if(charge>0) nPosMuon++;
	      else         nNegMuon++;
	      mhMuonPt->Fill(pt);
	      mhMuonPhiVsEta->Fill(eta, phi);
	      mhMuonMap->Fill(backleg, (module-1)*12+cell+1);
	      muonId.push_back(i);
	    }
	}
    }
  mhNTrk->Fill(nGoodTrack);
  mhNMuonPos->Fill(nPosMuon);
  mhNMuonNeg->Fill(nNegMuon);
  mhNMuonPosVsRun->Fill(runIdFill, nPosMuon);
  mhNMuonNegVsRun->Fill(runIdFill, nNegMuon);

  //================= MTD trigger time ===================
  StPicoMtdTrigger *mtdTrig = mPicoDst->mtdTrigger(0);
  int mtdTrigTime[2] = {0, 0};
  for(Int_t i=0; i<2; i++)
    {
      mtdTrigTime[i] = mtdTrig ? mtdTrig->getTHUBtime(i+1) : 0;
      mhMtdTriggerTime[i]->Fill(mtdTrigTime[i]);
    }

  //================= Trigger performance ===========
  if(mTrigUtil)
    {
      Int_t nQTsignal = 0;
      Int_t vpdTacSum = mTrigUtil->getVpdTacSum();
      for(Int_t im=0; im<kNQTboard; im++)
	{
	  for(Int_t i=0; i<8; i++)
	    {
	      Int_t mtdTacSum = mTrigUtil->getQtTacSum(im+1, i+1);
	      if(mtdTacSum<=10) continue;
	      nQTsignal++;
	      int bin = 0;
	      if(mRunYear!=2016) bin = im*8+i+1;
	      else               bin = im*4+i/2+1;
	      mhMtdVpdTacDiffMT001->Fill(bin, mtdTacSum/8 - vpdTacSum/8 + 1024);
	    }
	}
      mhNQtSignal->Fill(nQTsignal);

      Int_t nMT101signal = 0;
      for(Int_t im=0; im<kNQTboard; im++)
	{
	  if(mRunYear!=2016 && im>3) continue;
	  for(Int_t j=0; j<2; j++)
	    {
	      Int_t mix_tacSum = mTrigUtil->getMT101Tac(im+1, j);
	      Int_t mxq_tacSum = mTrigUtil->getQtTacSumHighestTwo(im+1, j);
	      Int_t mxq_tacPos = mTrigUtil->getQtPosHighestTwo(im+1, j);
	      if(mix_tacSum>0) 
		{
		  nMT101signal++;
		  mhMixMtdTacSumvsMxqMtdTacSum[im][j]->Fill(mxq_tacSum/8,mix_tacSum);
		  int bin = 0;
		  if(mRunYear!=2016) bin = im*8+mxq_tacPos;
		  else               bin = im*4+mxq_tacPos/2;
		  mhMtdVpdTacDiffMT101->Fill(bin,mix_tacSum-vpdTacSum/8+1024);
		}
	    }
	}
      mhNMT101Signal->Fill(nMT101signal);

      Int_t nTF201signal = 0;
      Int_t decision = mTrigUtil->getTF201TriggerBit();
      for(Int_t i=0; i<4; i++)
	{
	  for(Int_t j=0; j<2; j++)
	    {
	      if((decision>>(i*2+j))&0x1)
		nTF201signal++;
	    }
	}
      mhNTF201Signal->Fill(nTF201signal);
    }

  //================= MTD hits ===================
  int nMtdHits = mPicoDst->numberOfMtdHits();
  int mMthMtdHit = 0, nTrigMtdHit = 0, nTrigMtdHitMth = 0;
  mhMtdNHits->Fill(nMtdHits);
  for(Int_t i=0; i<nMtdHits; i++)
    {
      StPicoMtdHit *hit = mPicoDst->mtdHit(i);
      if(!hit) continue;
      Int_t backleg   = hit->backleg();
      Int_t module    = hit->module();
      Int_t cell      = hit->cell();
      Int_t lChan     = (module-1)*12+cell;
      Int_t gChan     = (backleg-1)*60 + lChan;
      Int_t tHub      = getMtdHitTHUB(backleg);
      Double_t tDiff = (hit->leadingEdgeTime().first+hit->leadingEdgeTime().second)/2 - mtdTrigTime[tHub-1];
      while(tDiff<0) tDiff += 51200;
      Bool_t isMth    = kFALSE;
      Int_t pidIndex  = getMtdPidTraitsIndex(hit);
      if(pidIndex>-1)
	{
	  StPicoMtdPidTraits *mtdPid = mPicoDst->mtdPidTraits(pidIndex);
	  StPicoTrack* pTrack = mPicoDst->track(mtdPid->trackIndex());
	  if(pTrack && pTrack->pMom().Mag()>0 && isValidTrack(pTrack)) isMth = kTRUE;
	}
      Bool_t isTrig =  mTrigUtil ? mTrigUtil->isHitFireTrigger(hit->backleg(), hit->module()) : kFALSE;
      mhMtdHitMap       ->Fill(backleg,lChan);
      mhMtdHitLeTimeDiff->Fill(gChan, hit->leadingEdgeTime().first-hit->leadingEdgeTime().second);
      mhMtdHitTotWest   ->Fill(gChan, hit->tot().first);
      mhMtdHitTotEast   ->Fill(gChan, hit->tot().second);
      mhMtdHitTrigTime  ->Fill(gChan, tDiff);

      if(isMth)
	{
	  mMthMtdHit++;
	  mhMtdHitTrigTimeTrkMth->Fill(gChan,tDiff);
	  mhMtdMatchHitMap->Fill(backleg,lChan);
	}
      if(isTrig)
	{
	  nTrigMtdHit ++;
	  mhMtdTrigHitMap->Fill(backleg,lChan);
	  if(isMth)
	    {
	      nTrigMtdHitMth++;
	      mhMtdTrigMthHitMap->Fill(backleg,lChan);
	    }
	}
    }
  mhMtdNMatchHits     ->Fill(mMthMtdHit);
  mhMtdTrigNHits      ->Fill(nTrigMtdHit);
  mhMtdTrigMthNHits   ->Fill(nTrigMtdHitMth);
  mhNMtdHitsVsRun     ->Fill(runIdFill, nMtdHits);
  mhNMtdTrigHitsVsRun ->Fill(runIdFill, nTrigMtdHit);
  mhNMtdMthHitsVsRun  ->Fill(runIdFill, mMthMtdHit);

  //================= muon analysis ===================
  int nULpair = 0, nLSpairPos = 0, nLSpairNeg = 0;
  UInt_t nMuon = muonId.size();
  for(UInt_t i=0; i<nMuon; i++)
    {
      StPicoTrack* ipTrack = mPicoDst->track(muonId[i]); 
      int iq = ipTrack->charge();
      TVector3 imom = ipTrack->pMom();
      TLorentzVector imuon;
      imuon.SetXYZM(imom.x(),imom.y(),imom.z(),0.10566);
      for(UInt_t j=i+1; j<nMuon; j++)
	{
	  StPicoTrack* jpTrack = mPicoDst->track(muonId[j]);
	  int jq = jpTrack->charge();
	  TVector3 jmom = jpTrack->pMom();
	  TLorentzVector jmuon;
	  jmuon.SetXYZM(jmom.x(),jmom.y(),jmom.z(),0.10566);

	  float pt1 = imom.Perp(), pt2 = jmom.Perp();
	  if(pt1<pt2) 
	    {
	      pt1 = jmom.Perp();
	      pt2 = imom.Perp();
	    }

	  if(pt1<1.5) continue;
	  TLorentzVector jpsi = imuon + jmuon;
	  Double_t invmass = jpsi.M();
	  if(iq*jq<0)
	    {
	      nULpair++;
	      mhInvMvsPtUL->Fill(invmass, jpsi.Pt());
	      mhInvMUL->Fill(invmass);
	    }
	  else 
	    {
	      if(iq>0)
		{
		  nLSpairPos++;
		  mhInvMvsPtLSpos->Fill(invmass, jpsi.Pt());
		  mhInvMLSpos->Fill(invmass);
		}
	      else
		{
		  nLSpairNeg++;
		  mhInvMvsPtLSneg->Fill(invmass, jpsi.Pt());
		  mhInvMLSneg->Fill(invmass);
		}
	    }
	}
    }
  mhNULpair            ->Fill(nULpair); 
  mhNLSpairPos         ->Fill(nLSpairPos); 
  mhNLSpairNeg         ->Fill(nLSpairNeg);
  mhNMuonPairULVsRun   ->Fill(runIdFill, nULpair);
  mhNMuonPairLSPosVsRun->Fill(runIdFill, nLSpairPos);
  mhNMuonPairLSNegVsRun->Fill(runIdFill, nLSpairNeg);

  return kStOK;
}


//_____________________________________________________________________________
void StMtdQAMaker::addCutToHisto(TH1 *h, const Int_t bin, const char *label, const Float_t value)
{
  if(!h) return;
  h->GetXaxis()->SetBinLabel(bin,label);
  if(value!=-9999999)
    h->SetBinContent(bin,value);
}

//_____________________________________________________________________________
void StMtdQAMaker::bookHistos()
{
  // event histograms


//this array describe the Channel input from which backleg & position & direction 

  const  char qtlabel[64][100] = {"QT1-1  25-1-J2","QT1-1  25-1-J3","QT1-2  25-5-J2","QT1-2  25-5-J3","QT1-3  25-2-J2","QT1-3  25-2-J3","QT1-4  25-4-J2","QT1-4  25-4-J3","QT1-5  25-3-J2","QT1-5  25-3-J3","QT1-6  30-3-J2","QT1-6  30-3-J3","QT1-7  30-1-J2","QT1-7  30-1-J3","QT1-8  30-5-J2","QT1-8  30-5-J3","QT2-1  05-1-J2","QT2-1  05-1-J3","QT2-2  05-5-J2","QT2-2  05-5-J3","QT2-3  05-2-J2","QT2-3  05-2-J3","QT2-4  05-4-J2","QT2-4  05-4-J3","QT2-5  05-3-J2","QT2-5  05-3-J3","QT2-6          ","QT2-6          ","QT2-7  30-2-J2","QT2-7  30-2-J3","QT2-8  30-4-J2","QT2-8  30-4-J3","QT3-1  10-1-J2","QT3-1  10-1-J3","QT3-2  10-5-J2","QT3-2  10-5-J3","QT3-3  10-2-J2","QT3-3  10-2-J3","QT3-4  10-4-J2","QT3-4  10-4-J3","QT3-5  10-3-J2","QT3-5  10-3-J3","QT3-6  15-3-J2","QT3-6  15-3-J3","QT3-7          ","QT3-7          ","QT3-8          ","QT3-8          ","QT4-1  21-1-J2","QT4-1  21-1-J3","QT4-2  21-5-J2","QT4-2  21-5-J3","QT4-3  20-2-J2","QT4-3  20-2-J3","QT4-4  20-4-J2","QT4-4  20-4-J3","QT4-5  20-3-J2","QT4-5  20-3-J3","QT4-6          ","QT4-6          ","QT4-7  15-2-J2","QT4-7  15-2-J3","QT4-8  15-4-J2","QT4-8  15-4-J3"};

  const char qtlabel2[32][100] = {"QT1-1  25-1","QT1-2  25-5","QT1-3  25-2","QT1-4  25-4","QT1-5  25-3","QT1-6  30-3","QT1-7  30-1","QT1-8  30-5","QT2-1  05-1","QT2-2  05-5","QT2-3  05-2","QT2-4  05-4","QT2-5  05-3","QT2-6      ","QT2-7  30-2","QT2-8  30-4","QT3-1  10-1","QT3-2  10-5","QT3-3  10-2","QT3-4  10-4","QT3-5  10-3","QT3-6  15-3","QT3-7      ","QT3-8      ","QT4-1  21-1","QT4-2  21-5","QT4-3  20-2","QT4-4  20-4","QT4-5  20-3","QT4-6      ","QT4-7  15-2","QT4-8  15-4"};

  const Int_t nSpecMBins = 56;//PRL mass bin
  Double_t specM[nSpecMBins+1] = {0, 0.01, 0.02, 0.03, 0.04, 0.05, 
				  0.06, 0.07, 0.08, 0.09, 0.1, 0.11, 0.125, 0.175, 0.2, 0.31, 0.4, 0.51, 
				  0.63, 0.75, 0.78, 0.785, 0.79, 0.8, 0.89, 0.965, 1, 1.01, 1.015, 1.02, 
				  1.035, 1.13, 1.25, 1.45, 1.65, 1.875, 2.075, 2.25, 2.475, 2.665, 2.85, 
				  2.99, 3.02, 3.035, 3.055, 3.07, 3.075, 3.09, 3.095, 3.1, 3.115, 3.13, 
				  3.225, 3.4, 3.85, 4.4, 5.5};

  const int startRun = 0;
  const int endRun = 200000;

  const int nPtBins = 40;
  const double lowPtBin = 0, hiPtBin = 20;

  mhEventCuts = new TH1F("hEventCuts","Cuts used for analysis",20,0,20);
  addCutToHisto(mhEventCuts, 1,  "mVertexMode",        mVertexMode);
  addCutToHisto(mhEventCuts, 2,  "|vtx_z|",            mMaxVtxZ);
  addCutToHisto(mhEventCuts, 3,  "dz",                 mMaxVtxDz);
  addCutToHisto(mhEventCuts, 4,  "trk_pt_min",         mMinTrkPt);
  addCutToHisto(mhEventCuts, 5,  "trk_pt_max",         mMaxTrkPt);
  addCutToHisto(mhEventCuts, 6,  "trk_eta",            mMaxTrkEta);
  addCutToHisto(mhEventCuts, 7,  "MinNHitsFit",        mMinNHitsFit);
  addCutToHisto(mhEventCuts, 8,  "MinNHitsDedx",       mMinNHitsDedx);
  addCutToHisto(mhEventCuts, 9,  "MinNHitsFrac",       mMinFitHitsFraction);
  addCutToHisto(mhEventCuts, 10, "mMaxDca",            mMaxDca);
  addCutToHisto(mhEventCuts, 11, "mMinNsigmaPi",       mMinNsigmaPi);
  addCutToHisto(mhEventCuts, 12, "mMaxNsigmaPi",       mMaxNsigmaPi);
  addCutToHisto(mhEventCuts, 13, "mMinMuonDeltaZ",     mMinMuonDeltaZ);
  addCutToHisto(mhEventCuts, 14, "mMaxMuonDeltaZ",     mMaxMuonDeltaZ);
  addCutToHisto(mhEventCuts, 15, "mMinMuonDeltaY",     mMinMuonDeltaY);
  addCutToHisto(mhEventCuts, 16, "mMaxMuonDeltaY",     mMaxMuonDeltaY);
  addCutToHisto(mhEventCuts, 17, "mMinMuonDeltaTof",   mMinMuonDeltaTof);
  addCutToHisto(mhEventCuts, 18, "mMaxMuonDeltaTof",   mMaxMuonDeltaTof);
  addCutToHisto(mhEventCuts, 19, "mMinMuonPt",         mMinMuonPt);
  addCutToHisto(mhEventCuts, 20, "mMtdHitTrigger",     mMtdHitTrigger);
  AddHist(mhEventCuts);

  const Int_t nbins = 4 + mTriggerIDs.size();
  mhEventStat = new TH1F("hEventStat","Event statistics",nbins,0.,(Float_t)nbins);
  mhEventStat->GetXaxis()->SetBinLabel(1,"All events");
  mhEventStat->GetXaxis()->SetBinLabel(2,"Good trigger");
  mhEventStat->GetXaxis()->SetBinLabel(3,"VPD");
  mhEventStat->GetXaxis()->SetBinLabel(4,"Vtx Cuts");
  for(UInt_t i=0; i<mTriggerIDs.size(); i++)
    {
      mhEventStat->GetXaxis()->SetBinLabel(i+5,Form("%d",mTriggerIDs[i]));
    }
  AddHist(mhEventStat);
  
  mhRunId = new TH1F("hRunId","Number of events per run",endRun-startRun+1,startRun-0.5,endRun+0.5);
  AddHist(mhRunId);

  mhZdcRate = new TH1F("hZdcRate","ZDC coincidence distribution;ZDC (kHz)",500,0,500);
  AddHist(mhZdcRate);

  mhBbcRate = new TH1F("hBbcRate","BBC coincidence distribution;BBC (kHz)",500,0,5000);
  AddHist(mhBbcRate);

  // vertex
  mhVtxZvsVpdVzDefault = new TH2F("hVtxZvsVpdVzDefault","Primary vertex z: VPD vs TPC (default);vz_{TPC} (cm);vz_{VPD} (cm)",201,-201,201,201,-201,201);
  AddHist(mhVtxZvsVpdVzDefault);

  mhVtxZDiffDefault = new TH1F("hVtxZDiffDefault","TPC vz - VPD vz (default); #Deltavz (cm)",200,-20,20);
  AddHist(mhVtxZDiffDefault);

  mhVtxClosestIndex = new TH1F("hVtxClosestIndex","TPC vertex index closest to VPD (ranking>0)",50,0,50);
  AddHist(mhVtxClosestIndex);

  mhVtxZvsVpdVzClosest = new TH2F("hVtxZvsVpdVzClosest","Primary vertex z: VPD vs TPC (closest);vz_{TPC} (cm);vz_{VPD} (cm)",201,-201,201,201,-201,201);
  AddHist(mhVtxZvsVpdVzClosest);

  mhVtxZDiffClosest = new TH1F("hVtxZDiffClosest","TPC vz - VPD vz (closest); #Deltavz (cm)",200,-20,20);
  AddHist(mhVtxZDiffClosest);

  mhVertexXY = new TH2F("hVertexXY","Primary vertex y vs x (TPC);vx_{TPC} (cm);vy_{TPC} (cm)",100,-5,5,100,-5,5);
  AddHist(mhVertexXY);

  mhVertexXZ = new TH2F("hVertexXZ","Primary vertex x vs z (TPC);vz_{TPC} (cm);vx_{TPC} (cm)",201,-201,201,100,-5,5);
  AddHist(mhVertexXZ);

  mhVertexYZ = new TH2F("hVertexYZ","Primary vertex y vs z (TPC);vz_{TPC} (cm);vy_{TPC} (cm)",201,-201,201,100,-5,5);
  AddHist(mhVertexYZ);

  mhVertexZ = new TH1F("hVertexZ","Primary vertex z (TPC); vz_{TPC} (cm)",201,-201,201);
  AddHist(mhVertexZ);

  mhVtxZDiffVsTpcVz = new TH2F("hVtxZDiffVsTpcVz","Vertex z difference vs vz_{TPC};vz_{TPC} (cm);#Deltavz (cm)",201,-201,201,200,-20,20);
  AddHist(mhVtxZDiffVsTpcVz);

  mhVpdVz = new TH1F("hVpdVz","VPD z distribution; vz_{VPD} (cm)",201,-201,201);
  AddHist(mhVpdVz);

  mhVtxZDiff = new TH1F("hVtxZDiff","TPC vz - VPD vz distribution; #Deltavz (cm)",200,-20,20);
  AddHist(mhVtxZDiff);

  /// reference multiplicity
  mhRefMult = new TH1F("hRefMult","RefMult distribution;RefMult",500,0,1000);
  AddHist(mhRefMult);

  mhgRefMult = new TH1F("hgRefMult","gRefMult distribution;gRefMult",500,0,1000);
  AddHist(mhgRefMult);

  mhgRefMultVsRefMult = new TH2F("hgRefMultVsRefMult","gRefMult vs. RefMult;RefMult;gRefMult",500,0,1000,500,0,1000);
  AddHist(mhgRefMultVsRefMult);

  mhTpcVzVsRefMult = new TH2F("hTpcVzVsRefMult","TPC v_{z} vs. RefMult;RefMult;vz_{TPC} (cm)",500,0,1000,201,-201,201);
  AddHist(mhTpcVzVsRefMult);

  mhDiffVzVsRefMult = new TH2F("hDiffVzVsRefMult","TPC-VPD v_{z} vs. RefMult;RefMult;#Deltavz (cm)",500,0,1000,200,-20,20);
  AddHist(mhDiffVzVsRefMult);

  mhZdcRateVsRefMult = new TH2F("hZdcRateVsRefMult","ZDC rate vs. RefMult;RefMult;ZDC (kHz)",500,0,1000,500,0,5000);
  AddHist(mhZdcRateVsRefMult);

  mhBbcRateVsRefMult = new TH2F("hBbcRateVsRefMult","BBC rate vs. RefMult;RefMult;BBC (kHz)",500,0,1000,500,0,5000);
  AddHist(mhBbcRateVsRefMult);

  mhTofMultVsRefMult = new TH2F("hTofMultVsRefMult","TofMult vs. RefMult;RefMult;TofMult",500,0,1000,500,0,5000);
  AddHist(mhTofMultVsRefMult);

  // Primary tracks
  mhNTrk = new TH1F("hNTrk","Number of good primary tracks per event;N",1000,0,1000);
  AddHist(mhNTrk);

  mhTrkPt = new TH1F("hTrkPt","p_{T} of primary tracks;p_{T} (GeV/c)",nPtBins,lowPtBin,hiPtBin);
  AddHist(mhTrkPt);

  mhTrkDcaVsPt = new TH2F("hTrkDcaVsPt","Primary tracks: DCA vs p_{T};p_{T} (GeV/c);dca (cm)",nPtBins,lowPtBin,hiPtBin,35,0,3.5);
  AddHist(mhTrkDcaVsPt);

  mhTrkPhiVsPt = new TH2F("hTrkPhiVsPt","Primary tracks: #varphi vs p_{T};p_{T} (GeV/c);#varphi",nPtBins,lowPtBin,hiPtBin,120,0,2*pi);
  AddHist(mhTrkPhiVsPt);

  mhTrkEtaVsPt = new TH2F("hTrkEtaVsPt","Primary tracks: #eta vs p_{T};p_{T} (GeV/c);#eta",nPtBins,lowPtBin,hiPtBin,60,-1.2,1.2);
  AddHist(mhTrkEtaVsPt);

  mhTrkPhiEta = new TH2F("hTrkPhiEta","Primary tracks: #varphi vs #eta (p_{T} > 1 GeV/c);#eta;#varphi",60,-1.2,1.2,120,0,2*pi);
  AddHist(mhTrkPhiEta);

  mhTrkNHitsFitVsPt = new TH2F("hTrkNHitsFitVsPt","Primary tracks: NHitsFit vs p_{T};p_{T} (GeV/c);NHitsFit",nPtBins,lowPtBin,hiPtBin,45,0,45);
  AddHist(mhTrkNHitsFitVsPt);

  mhTrkNHitsPossVsPt = new TH2F("hTrkNHitsPossVsPt","Primary tracks: NHitsPoss vs p_{T};p_{T} (GeV/c);NHitsPoss",nPtBins,lowPtBin,hiPtBin,45,0,45);
  AddHist(mhTrkNHitsPossVsPt);

  mhTrkNHitsDedxVsPt = new TH2F("hTrkNHitsDedxVsPt","Primary tracks: NHitsDedx vs p_{T};p_{T} (GeV/c);NHitsDedx",nPtBins,lowPtBin,hiPtBin,45,0,45);
  AddHist(mhTrkNHitsDedxVsPt);

  mhTrkDedxVsMom = new TH2F("hTrkDedxVsMom","Primary tracks: dE/dx vs momentum;p (GeV/c);dE/dx (keV/cm)",nPtBins,lowPtBin,hiPtBin,100,0,10);
  AddHist(mhTrkDedxVsMom);

  mhTrkDedxVsPhi = new TH2F("hTrkDedxVsPhi","Primary tracks: dE/dx vs #varphi (p > 1 GeV/c);#varphi;dE/dx (keV/cm)",120,0,2*pi,100,0,10);
  AddHist(mhTrkDedxVsPhi);

  mhTrkNsigmaPiVsMom = new TH2F("hTrkNsigmaPiVsMom","Primary tracks: n#sigma_{#pi} vs momentum;p (GeV/c);n#sigma_{#pi}",nPtBins,lowPtBin,hiPtBin,100,-5,5);
  AddHist(mhTrkNsigmaPiVsMom);

  mhTrkM2VsMom = new TH2F("hTrkM2VsMom","Primary tracks: m^{2} vs momentum;p (GeV/c);m^{2} (GeV/c^{2})^{2}",nPtBins,lowPtBin,hiPtBin,100,0,2);
  AddHist(mhTrkM2VsMom);

  mhTrkBetaVsMom = new TH2F("hTrkBetaVsMom","Primary tracks: 1/#beta vs momentum;p (GeV/c);1/#beta",nPtBins,lowPtBin,hiPtBin,100,0,5);
  AddHist(mhTrkBetaVsMom);

  mhTofMthTrkLocaly = new TH2F("hTofMthTrkLocaly","TOF match: local y vs tray;tray;local y (cm)",120,0.5,120.5,100,-5,5);
  AddHist(mhTofMthTrkLocaly);

  mhTofMthTrkLocalz = new TH2F("hTofMthTrkLocalz","TOF match: local z vs module;module;local z (cm)",64,0.5,64.5,100,-5,5);
  AddHist(mhTofMthTrkLocalz);

  // MTD trigger electronics
  mhMtdQTAdcAll = new TH2F("hMtdQTAdcAll","MTD QT: ADC vs channel (All);;ADC",64,0.5,64.5,350,0,3500);
  AddHist(mhMtdQTAdcAll);

  mhMtdQTTacAll = new TH2F("hMtdQTTacAll","MTD QT: TAC vs channel (All);;TAC",64,0.5,64.5,300,0,3000);
  AddHist(mhMtdQTTacAll);

  mhMtdQTAdcVsTacAll = new TH2F("hMtdQTAdcVsTacAll","MTD QT: ADC vs. TAC (All);TAC;ADC",350,0,3500,350,0,3500);
  AddHist(mhMtdQTAdcVsTacAll);

  mhMtdQTJ2J3Diff = new TH2F("hMtdQTJ2J3Diff","MTD QT: J3-J2 TAC vs channel;;TAC (J3-J2)",32,0.5,32.5,160,-800,800);
  AddHist(mhMtdQTJ2J3Diff);

  mhMtdVpdTacDiffMT001 = new TH2F("hMtdVpdTacDiffMT001","QT: MTD-VPD tac difference (All);;tac_{MTD}-tac_{VPD}",32,0.5,32.5,1000,500,1500);
  AddHist(mhMtdVpdTacDiffMT001);

  mhMtdVpdTacDiffMT001Mth = new TH2F("hMtdVpdTacDiffMT001Mth","QT: MTD-VPD tac difference (Track matched);;tac_{MTD}-tac_{VPD}",32,0.5,32.5,1000,500,1500);
  AddHist(mhMtdVpdTacDiffMT001Mth);

  mhMtdVpdTacDiffMT001Muon = new TH2F("hMtdVpdTacDiffMT001Muon","QT: MTD-VPD tac difference (Muon PID);;tac_{MTD}-tac_{VPD}",32,0.5,32.5,1000,500,1500);
  AddHist(mhMtdVpdTacDiffMT001Muon);

  mhMtdVpdTacDiffMT101 = new TH2F("hMtdVpdTacDiffMT101","MT101: MTD-VPD tac difference;;tac_{MTD}-tac_{VPD}+1024",32,0.5,32.5,1000,500,1500);
  AddHist(mhMtdVpdTacDiffMT101);

  mhMtdVpdTacDiffMT101Mth = new TH2F("hMtdVpdTacDiffMT101Mth","MT101: MTD-VPD tac difference (Track matched);;tac_{MTD}-tac_{VPD}+1024",32,0.5,32.5,1000,500,1500);
  AddHist(mhMtdVpdTacDiffMT101Mth);

  mhMtdVpdTacDiffMT101Muon = new TH2F("hMtdVpdTacDiffMT101Muon","MT101: MTD-VPD tac difference (Muon PID);;tac_{MTD}-tac_{VPD}+1024",32,0.5,32.5,1000,500,1500);
  AddHist(mhMtdVpdTacDiffMT101Muon);

  for(Int_t i=0; i<64; i++)
    {
      mhMtdQTAdcAll->GetXaxis()->SetBinLabel(i+1,qtlabel[i]);
      mhMtdQTTacAll->GetXaxis()->SetBinLabel(i+1,qtlabel[i]);
    }
  for(Int_t i=0; i<32; i++)
    {
      mhMtdVpdTacDiffMT001->GetXaxis()->SetBinLabel(i+1,qtlabel2[i]);
      mhMtdVpdTacDiffMT001Mth->GetXaxis()->SetBinLabel(i+1,qtlabel2[i]);
      mhMtdVpdTacDiffMT001Muon->GetXaxis()->SetBinLabel(i+1,qtlabel2[i]);
      mhMtdVpdTacDiffMT101->GetXaxis()->SetBinLabel(i+1,qtlabel2[i]);
      mhMtdVpdTacDiffMT101Mth->GetXaxis()->SetBinLabel(i+1,qtlabel2[i]);
      mhMtdVpdTacDiffMT101Muon->GetXaxis()->SetBinLabel(i+1,qtlabel2[i]);
      mhMtdQTJ2J3Diff->GetXaxis()->SetBinLabel(i+1,qtlabel2[i]);
    }

  for(Int_t i=0; i<kNQTboard; i++)
    {
      for(Int_t j=0; j<2; j++)
	{
	  mhMixMtdTacSumvsMxqMtdTacSum[i][j] = new TH2F(Form("hMixMtdTacSumvsMxqMtdTacSum_QT%d_%d",i+1,j),Form("MTD QT%d: MIX vs MXQ at %d;mxq_mtdtacsum;mix_mtdtacsum",i+1,j),1024,0,1024,1024,0,1024);
	  AddHist(mhMixMtdTacSumvsMxqMtdTacSum[i][j]);
	}
    }

  mhNQtSignal = new TH1F("hNQtSignal","Number of good QT signals;N",10,0,10);
  AddHist(mhNQtSignal);

  mhNMT101Signal = new TH1F("hNMT101Signal","Number of good MT101 signals;N",10,0,10);
  AddHist(mhNMT101Signal);

  mhNTF201Signal = new TH1F("hNTF201Signal","Number of good TF201 signals;N",10,0,10);
  AddHist(mhNTF201Signal);


  // MTD hits
  mhMtdTriggerTime[0] = new TH1F("hMtdTriggerTime0","MTD: trigger time for backleg 16-30;t",120,0,1.2e5);
  AddHist(mhMtdTriggerTime[0]);

  mhMtdTriggerTime[1] = new TH1F("hMtdTriggerTime1","MTD: trigger time for backleg 1-15;t",120,0,1.2e5);
  AddHist(mhMtdTriggerTime[1]);

  // ===== raw hits
  mhMtdNRawHits = new TH1F("hMtdNRawHits","Number of raw MTD hits per event;N",100,0,100);
  AddHist(mhMtdNRawHits);

  mhMtdRawHitMap = new TH2F("hMtdRawHitMap","MTD: channel vs backleg of raw hits;backleg;channel",30,0.5,30.5,120,0.5,120.5);
  AddHist(mhMtdRawHitMap);

  mhMtdRawHitLeTime = new TH2F("hMtdRawHitLeTime","MTD: leading time of raw hit;channel;t_{leading} (ns)",3601,-0.5,3600.5,128,0,51200);
  AddHist(mhMtdRawHitLeTime);

  mhMtdRawHitTrTime = new TH2F("hMtdRawHitTrTime","MTD: trailing time of raw hit;channel;t_{trailing} (ns)",3601,-0.5,3600.5,128,0,51200);
  AddHist(mhMtdRawHitTrTime);

  mhMtdRawHitLeNDiff = new TH2F("hMtdRawHitLeNDiff","MTD: difference in leading raw hit rates (west-east);channel;nLeRawHit: West - East",1801,-0.5,1800.5,11,-5.5,5.5);
  AddHist(mhMtdRawHitLeNDiff);

  mhMtdRawHitTrNDiff = new TH2F("hMtdRawHitTrNDiff","MTD: difference in trailing raw hit rates (west-east);channel;nTrRawHit: West - East",1801,-0.5,1800.5,11,-5.5,5.5);
  AddHist(mhMtdRawHitTrNDiff);

  mhMtdRawHitLeNEast = new TH1F("hMtdRawHitLeNEast","MTD: number of leading raw hit (east);channel;N_{leading,east}",1801,-0.5,1800.5);
  AddHist(mhMtdRawHitLeNEast);

  mhMtdRawHitLeNWest = new TH1F("hMtdRawHitLeNWest","MTD: number of leading raw hit (west);channel;N_{leading,west}",1801,-0.5,1800.5);
  AddHist(mhMtdRawHitLeNWest);

  mhMtdRawHitTrNEast = new TH1F("hMtdRawHitTrNEast","MTD: number of trailing raw hit (east);channel;N_{trailing,east}",1801,-0.5,1800.5);
  AddHist(mhMtdRawHitTrNEast);

  mhMtdRawHitTrNWest = new TH1F("hMtdRawHitTrNWest","MTD: number of trailing raw hit (west);channel;N_{trailing,west}",1801,-0.5,1800.5);
  AddHist(mhMtdRawHitTrNWest);

  // ===== hits
  mhMtdNHits = new TH1F("hMtdNHits","Number of MTD hits per event;N",50,0,50);
  AddHist(mhMtdNHits);

  mhMtdHitMap = new TH2F("hMtdHitMap","MTD: channel vs backleg of hits;backleg;channel",30,0.5,30.5,60,-0.5,59.5);
  AddHist(mhMtdHitMap);

  mhMtdHitLeTimeDiff = new TH2F("hMtdHitLeTimeDiff","MTD: (east-west) leading time of hits;channel;#Deltat_{leading} (ns)",1801,-0.5,1800.5,41,-20.5,20.5);
  AddHist(mhMtdHitLeTimeDiff);

  mhMtdHitTotWest = new TH2F("hMtdHitTotWest","MTD: west TOT of hits;channel;tot (ns)",1801,-0.5,1800.5,50,0,50);
  AddHist(mhMtdHitTotWest);

  mhMtdHitTotEast = new TH2F("hMtdHitTotEast","MTD: east TOT of hits;channel;tot (ns)",1801,-0.5,1800.5,50,0,50);
  AddHist(mhMtdHitTotEast);

  const int nBinsTrigTime = 750;
  const double minTrigTime = 2000, maxTrigTime = 3500;
  
  mhMtdHitTrigTime = new TH2F("hMtdHitTrigTime","MTD: trigger time of hit (west+east)/2;channel;tdc-t_{trigger} (ns)",1801,-0.5,1800.5,nBinsTrigTime,minTrigTime,maxTrigTime);
  AddHist(mhMtdHitTrigTime);

  mhMtdHitTrigTimeTrkMth = new TH2F("hMtdHitTrigTimeTrkMth","MTD: trigger time of hits (Track matched);channel;tdc-t_{trigger} (ns)",1801,-0.5,1800.5,nBinsTrigTime,minTrigTime,maxTrigTime);
  AddHist(mhMtdHitTrigTimeTrkMth);

  mhMtdTrigNHits = new TH1F("hMtdTrigNHits","Number of triggering MTD hits per event;N",50,0,50);
  AddHist(mhMtdTrigNHits);

  mhMtdTrigHitMap = new TH2F("hMtdTrigHitMap","MTD: channel vs backleg of triggering hits;backleg;channel",30,0.5,30.5,60,-0.5,59.5);
  AddHist(mhMtdTrigHitMap);

  mhMtdTrigMthNHits = new TH1F("hMtdTrigMthNHits","Number of triggering MTD hits matched to tracks;N",50,0,50);
  AddHist(mhMtdTrigMthNHits);

  mhMtdTrigMthHitMap = new TH2F("hMtdTrigMthHitMap","MTD: channel vs backleg of triggering hits matched to tracks;backleg;channel",30,0.5,30.5,60,-0.5,59.5);
  AddHist(mhMtdTrigMthHitMap);

  // ===== matched hits
  mhMtdNMatchHits = new TH1F("mhMtdNMatchHits","Number of matched MTD hits per event;N",50,0,50);
  AddHist(mhMtdNMatchHits);

  mhMtdMatchHitMap = new TH2F("hMtdMatchHitMap","MTD: channel vs backleg of matched hits;backleg;channel",30,0.5,30.5,60,-0.5,59.5);
  AddHist(mhMtdMatchHitMap);

  mhMtdMatchTrkPt = new TH1F("hMtdMatchTrkPt","MTD: p_{T} of matched primary tracks;p_{T} (GeV/c)",nPtBins,lowPtBin,hiPtBin);
  AddHist(mhMtdMatchTrkPt);

  mhMtdMatchTrkPhiEta = new TH2F("hMtdMatchTrkPhiEta","MTD: #varphi vs #eta of matched primary tracks;#eta;#varphi",60,-1.2,1.2,120,0,2*pi);
  AddHist(mhMtdMatchTrkPhiEta);

  mhMtdMatchTrkPhiPt = new TH2F("hMtdMatchTrkPhiPt","MTD: #varphi vs p_{T} of matched primary tracks;p_{T} (GeV/c);#varphi",nPtBins,lowPtBin,hiPtBin,120,0,2*pi);
  AddHist(mhMtdMatchTrkPhiPt);

  mhMtdMatchDzVsChan = new TH2F("hMtdMatchDzVsChan","MTD: #Deltaz distribution;channel;#Deltaz = z_{proj}-z_{hit} (cm)",1801,-0.5,1800.5,200,-100,100);
  AddHist(mhMtdMatchDzVsChan);

  mhMtdMatchDzVsPtPos = new TH2F("hMtdMatchDzVsPtPos","MTD: #Deltaz vs p_{T} for positive tracks;p_{T} (GeV/c);#Deltaz (cm)",nPtBins,lowPtBin,hiPtBin,200,-100,100);
  AddHist(mhMtdMatchDzVsPtPos);

  mhMtdMatchDzVsPtNeg = new TH2F("hMtdMatchDzVsPtNeg","MTD: #Deltaz vs p_{T} for negative tracks;p_{T} (GeV/c);#Deltaz (cm)",nPtBins,lowPtBin,hiPtBin,200,-100,100);
  AddHist(mhMtdMatchDzVsPtNeg);

  mhMtdMatchDyVsChan = new TH2F("hMtdMatchDyVsChan","MTD: #Deltay distribution;channel;#Deltay = y_{proj}-y_{hit} (cm)",1801,-0.5,1800.5,200,-100,100);
  AddHist(mhMtdMatchDyVsChan);

  mhMtdMatchDyVsPtPos = new TH2F("hMtdMatchDyVsPtPos","MTD: #Deltay vs p_{T} for positive tracks;p_{T} (GeV/c);#Deltay (cm)",nPtBins,lowPtBin,hiPtBin,200,-100,100);
  AddHist(mhMtdMatchDyVsPtPos);

  mhMtdMatchDyVsPtNeg = new TH2F("hMtdMatchDyVsPtNeg","MTD: #Deltay vs p_{T} for negative tracks;p_{T} (GeV/c);#Deltay (cm)",nPtBins,lowPtBin,hiPtBin,200,-100,100);
  AddHist(mhMtdMatchDyVsPtNeg);

  mhMtdMatchDtofVsPt = new TH2F("hMtdMatchDtofVsPt","MTD: #Deltatof vs p_{T} distribution;p_{T} (GeV/c);#Deltatof (ns)",nPtBins,lowPtBin,hiPtBin,100,-5,5);
  AddHist(mhMtdMatchDtofVsPt);

  mhMtdMatchMtdTofVsChan = new TH2F("hMtdMatchMtdTofVsChan","MTD: MTD time vs channel of primary tracks;channel;tof_{MTD} (ns)",1800,-0.5,1799.5,300,0,30);
  AddHist(mhMtdMatchMtdTofVsChan);

  mhMtdMatchExpTofVsChan = new TH2F("hMtdMatchExpTofVsChan","MTD: TPC time vs channel of primary tracks;channel;tof_{expected} (ns)",1800,-0.5,1799.5,300,0,30);
  AddHist(mhMtdMatchExpTofVsChan);

  mhMtdMatchDtofVsChan = new TH2F("hMtdMatchDtofVsChan","MTD: #Deltatof distribution;channel;#Deltatof (ns)",1801,-0.5,1800.5,100,-5,5);
  AddHist(mhMtdMatchDtofVsChan);

  mhMtdMatchLocalyVsChan = new TH2F("hMtdMatchLocalyVsChan","MTD: local y of matched tracks;channel;y (cm)",1801,-0.5,1800.5,100,-50.5,49.5);
  AddHist(mhMtdMatchLocalyVsChan);

  mhMtdMatchLocalzVsChan = new TH2F("hMtdMatchLocalzVsChan","MTD: local z of matched tracks;channel;z (cm)",1801,-0.5,1800.5,100,-50.5,49.5);
  AddHist(mhMtdMatchLocalzVsChan);

  /// Muon analysis
  mhNMuonPos = new TH1F("hNMuonPos","Number of positive muon candidates per event;N",5,-0.5,4.5);
  AddHist(mhNMuonPos);

  mhNMuonNeg = new TH1F("hNMuonNeg","Number of negative muon candidates per event;N",5,-0.5,4.5);
  AddHist(mhNMuonNeg);

  mhMuonPt = new TH1F("hMuonPt","p_{T} distribution of muon candidates;p_{T} (GeV/c)",nPtBins,lowPtBin,hiPtBin);
  AddHist(mhMuonPt);

  mhMuonPhiVsEta = new TH2F("hMuonPhiVsEta","#varphi vs #eta of muon candidates;#eta;#varphi",60,-1.2,1.2,120,0,2*pi);
  AddHist(mhMuonPhiVsEta);

  mhMuonMap = new TH2F("hMuonMap","Channel vs backleg of muon candidates;backleg;channel",30,0.5,30.5,60,0.5,60.5);
  AddHist(mhMuonMap);

  mhNULpair = new TH1F("hNULpair","Number of unlike-sign pairs per event;N",5,-0.5,4.5);
  AddHist(mhNULpair);

  mhNLSpairPos = new TH1F("hNLSpairPos","Number of positive like-sign pairs per event;N",5,-0.5,4.5);
  AddHist(mhNLSpairPos);
	  
  mhNLSpairNeg = new TH1F("hNLSpairNeg","Number of negative like-sign pairs per event;N",5,-0.5,4.5);
  AddHist(mhNLSpairNeg);
	  
  mhInvMvsPtUL = new TH2F("hInvMvsPtUL"," p_{T} vs M_{#mu#mu} for unlike-sign pairs;M_{#mu#mu} (GeV/c^2);p_{T} (GeV/c)",300,0,15,10,0,10);
  AddHist(mhInvMvsPtUL);
	  
  mhInvMvsPtLSpos = new TH2F("hInvMvsPtLSpos"," p_{T} vs M_{#mu#mu} for positive like-sign pairs;M_{#mu#mu} (GeV/c^2);p_{T} (GeV/c)",300,0,15,10,0,10);
  AddHist(mhInvMvsPtLSpos);
	  
  mhInvMvsPtLSneg = new TH2F("hInvMvsPtLSneg"," p_{T} vs M_{#mu#mu} for negative like-sign pairs;M_{#mu#mu} (GeV/c^2);p_{T} (GeV/c)",300,0,15,10,0,10);
  AddHist(mhInvMvsPtLSneg);

  mhInvMUL = new TH1F("hInvMUL","M_{#mu#mu} for unlike-sign pairs;M_{#mu#mu} (GeV/c^2)",nSpecMBins,specM);
  AddHist(mhInvMUL);
	  
  mhInvMLSpos = new TH1F("hInvMLSpos","M_{#mu#mu} for positive like-sign pairs;M_{#mu#mu} (GeV/c^2)",nSpecMBins,specM);
  AddHist(mhInvMLSpos);
	  
  mhInvMLSneg = new TH1F("hInvMLSneg","M_{#mu#mu} for negative like-sign pairs;M_{#mu#mu} (GeV/c^2)",nSpecMBins,specM);
  AddHist(mhInvMLSneg);

  int nRun = endRun-startRun+1;
  double run_lowBin = startRun - 0.5;
  double run_highBin = endRun + 0.5;

  mhBBCrateVsRun = new TProfile("mhBBCrateVsRun","BBC rate vs run; run index; BBC rate (kHz)",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhBBCrateVsRun);

  mhZDCrateVsRun = new TProfile("mhZDCrateVsRun","ZDC rate vs run; run index; ZDC rate (kHz)",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhZDCrateVsRun);

  mhRefMultVsRun = new TProfile("mhRefMultVsRun","Reference multiplicity vs run; run index; RefMult",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhRefMultVsRun);

  mhgRefMultVsRun = new TProfile("mhgRefMultVsRun","Global reference multiplicity vs run; run index; gRefMult",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhgRefMultVsRun);

  mhTpcVxVsRun = new TProfile("mhTpcVxVsRun","TPC v_{x} vs run; run index; TPC v_{x} (cm)",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhTpcVxVsRun);
      
  mhTpcVyVsRun = new TProfile("mhTpcVyVsRun","TPC v_{y} vs run; run index; TPC v_{y} (cm)",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhTpcVyVsRun);
      
  mhTpcVzVsRun = new TProfile("mhTpcVzVsRun","TPC v_{z} vs run; run index; TPC v_{z} (cm)",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhTpcVzVsRun);

  mhVpdVzVsRun = new TProfile("mhVpdVzVsRun","VPD v_{z} vs run; run index; VPD v_{z} (cm)",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhVpdVzVsRun);

  mhDiffVzVsRun = new TProfile("mhDiffVzVsRun","TPC-VPD v_{z} vs run; run index;#Deltav_{z} (cm)",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhDiffVzVsRun);

  mhpTrkPtVsRun = new TProfile("mhpTrkPtVsRun","Primary track p_{T} vs run; run index; p_{T,trk} (GeV/c)",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhpTrkPtVsRun);

  mhpTrkEtaVsRun = new TProfile("mhpTrkEtaVsRun","Primary track #eta vs run; run index; #eta_{trk}",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhpTrkEtaVsRun);

  mhpTrkPhiVsRun = new TProfile("mhpTrkPhiVsRun","Primary track #varphi vs run; run index; #varphi_{trk}",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhpTrkPhiVsRun);

  mhpTrkDcaVsRun = new TProfile("mhpTrkDcaVsRun","Primary track DCA vs run; run index; DCA (cm)",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhpTrkDcaVsRun);

  mhNHitsFitVsRun = new TProfile("mhNHitsFitVsRun","Primary track NHitsFit vs run; run index; NhitsFit",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhNHitsFitVsRun);

  mhNHitsPossVsRun = new TProfile("mhNHitsPossVsRun","Primary track NHitsPoss vs run; run index; NHitsPoss",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhNHitsPossVsRun);

  mhNHitsDedxVsRun = new TProfile("mhNHitsDedxVsRun","Primary track NHitsDedx vs run; run index; NhitsDedx",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhNHitsDedxVsRun);

  mhDedxVsRun = new TProfile("mhDedxVsRun","Primary track dE/dx vs run; run index; dE/dx (keV/cm)",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhDedxVsRun);

  mhNsigmaPiVsRun = new TProfile("mhNsigmaPiVsRun","Primary track n#sigma_{#pi} vs run; run index; n#sigma_{#pi}",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhNsigmaPiVsRun);

  mhNsigmaEVsRun = new TProfile("mhNsigmaEVsRun","Primary track n#sigma_{e} vs run; run index; n#sigma_{e}",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhNsigmaEVsRun);

  mhNsigmaKVsRun = new TProfile("mhNsigmaKVsRun","Primary track n#sigma_{K} vs run; run index; n#sigma_{K}",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhNsigmaKVsRun);

  mhNsigmaPVsRun = new TProfile("mhNsigmaPVsRun","Primary track n#sigma_{P} vs run; run index; n#sigma_{P}",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhNsigmaPVsRun);

  mhBetaVsRun = new TProfile("mhBetaVsRun","Primary track 1/#beta vs run; run index; 1/#beta",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhBetaVsRun);

  mhNMtdHitsVsRun = new TProfile("mhNMtdHitsVsRun","Number of MTD hits per event vs run; run index; N",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhNMtdHitsVsRun);

  mhNMtdTrigHitsVsRun = new TProfile("mhNMtdTrigHitsVsRun","Number of triggering MTD hits per event vs run; run index; N",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhNMtdTrigHitsVsRun);

  mhNMtdMthHitsVsRun = new TProfile("mhNMtdMthHitsVsRun","Number of matched MTD hits per event vs run; run index; N",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhNMtdMthHitsVsRun);

  mhNMuonPosVsRun = new TProfile("mhNMuonPosVsRun","Number of positive muon candidates per event vs run; run index; N",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhNMuonPosVsRun);

  mhNMuonNegVsRun = new TProfile("mhNMuonNegVsRun","Number of negative muon candidates per event vs run; run index; N",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhNMuonNegVsRun);

  mhNMuonPairULVsRun = new TProfile("mhNMuonPairULVsRun","Number of unlike-sign muon pairs per event vs run; run index; N",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhNMuonPairULVsRun);

  mhNMuonPairLSPosVsRun = new TProfile("mhNMuonPairLSPosVsRun","Number of positve like-sign muon pairs per event vs run; run index; N",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhNMuonPairLSPosVsRun);

  mhNMuonPairLSNegVsRun = new TProfile("mhNMuonPairLSNegVsRun","Number of negative like-sign muon pairs per event vs run; run index; N",nRun, run_lowBin, run_highBin);
  AddHist((TH1*)mhNMuonPairLSNegVsRun);
}


//_____________________________________________________________________________
void StMtdQAMaker::printConfig()
{
  const char *decision[2] = {"no","yes"};
  const char *vtxmode[2] = {"default","closest to VPD"};
  printf("=== Configuration for StMtdQAMaker ===\n");
  printf("Use vertex that is %s\n",vtxmode[mVertexMode]);
  printf("Maximum vertex z: %1.0f\n",mMaxVtxZ);
  printf("Maximum vertex r: %1.0f\n",mMaxVtxR);
  printf("Maximum vz diff: %1.0f\n",mMaxVtxDz);
  printf("Track pt  range: [%1.2f, %1.2f]\n",mMinTrkPt,mMaxTrkPt);
  printf("Track phi range: [%1.2f, %1.2f]\n",mMinTrkPhi,mMaxTrkPhi);
  printf("Track eta range: [%1.2f, %1.2f]\n",mMinTrkEta,mMaxTrkEta);
  printf("Minimum number of fit hits: %d\n",mMinNHitsFit);
  printf("Minimum number of dedx hits: %d\n",mMinNHitsDedx);
  printf("Minimum fraction of fit hits: %4.2f\n",mMinFitHitsFraction);
  printf("Maximum dca: %1.2f\n",mMaxDca);
  printf("Muon PID cuts:\n");
  printf("    pt > %1.1f GeV/c \n", mMinMuonPt);
  printf("    %1.1f < NsigmaPi < %1.1f\n",mMinNsigmaPi,mMaxNsigmaPi);
  printf("    MTD hit trigger: %s\n",decision[mMtdHitTrigger]);
  printf("    %1.0f < dz < %1.0f cm\n",mMinMuonDeltaZ,mMaxMuonDeltaZ);
  printf("    %1.0f < dy < %1.0f cm\n",mMinMuonDeltaY,mMaxMuonDeltaY);
  printf("    %1.1f < dtof < %1.1f ns\n",mMinMuonDeltaTof,mMaxMuonDeltaTof);
  printf("=======================================\n");
}

//_____________________________________________________________________________
Int_t StMtdQAMaker::getMtdHitTHUB(const Int_t backleg) const
{
  if(backleg>=1 && backleg<=15)        return 2;
  else if (backleg>=16 && backleg<=30) return 1;
  else return -1;
}

//_____________________________________________________________________________
Bool_t StMtdQAMaker::isValidTrack(StTrack *track, StVertex *vtx) const 
{
  StThreeVectorF mom = track->geometry()->momentum();
  Float_t pt = mom.perp();
  Float_t eta = mom.pseudoRapidity();
  Float_t phi = rotatePhi(mom.phi());

  if(pt < mMinTrkPt   || pt > mMaxTrkPt)  return kFALSE;
  if(eta < mMinTrkEta || eta > mMaxTrkEta) return kFALSE;
  if(phi < mMinTrkPhi || phi > mMaxTrkPhi) return kFALSE;

  Int_t nHitsFit = track->fitTraits().numberOfFitPoints(kTpcId);
  if(nHitsFit<mMinNHitsFit) return kFALSE;

  Int_t nHitsPoss = track->numberOfPossiblePoints(kTpcId);
  if(nHitsFit/(1.0*nHitsPoss)<mMinFitHitsFraction) return kFALSE;

  StTpcDedxPidAlgorithm pidAlgorithm;
  const StParticleDefinition *pd = track->pidTraits(pidAlgorithm);
  if(!pd || !pidAlgorithm.traits()) return kFALSE;
  if(pidAlgorithm.traits()->numberOfPoints()<mMinNHitsDedx) return kFALSE;

  StGlobalTrack *globalTrack = dynamic_cast<StGlobalTrack*>(track);
  if(!globalTrack) return kFALSE;
  THelixTrack    thelix      = globalTrack->dcaGeometry()->thelix();
  const Double_t *pos        = thelix.Pos();
  StThreeVectorF dcaGlobal   = StThreeVectorF(pos[0],pos[1],pos[2]) - vtx->position();
  if(dcaGlobal.mag()>mMaxDca)  return kFALSE;

  return kTRUE;
}


//_____________________________________________________________________________
Bool_t StMtdQAMaker::isValidTrack(const StMuTrack *track) const 
{
  if(!track) return kFALSE;
  const StThreeVectorF mom = track->momentum();
  Double_t pt = mom.perp();
  Double_t eta = mom.pseudoRapidity();
  Double_t phi = rotatePhi(mom.phi());

  if(pt < mMinTrkPt   || pt > mMaxTrkPt)             return kFALSE;
  if(eta < mMinTrkEta || eta > mMaxTrkEta)           return kFALSE;
  if(phi < mMinTrkPhi || phi > mMaxTrkPhi)           return kFALSE;
  if(track->nHitsFit(kTpcId)<mMinNHitsFit)           return kFALSE;
  if(track->nHitsFit(kTpcId)/(1.0*track->nHitsPoss(kTpcId))<mMinFitHitsFraction) return kFALSE;
  if(track->nHitsDedx()<mMinNHitsDedx)               return kFALSE;
  if(track->dcaGlobal().mag()>mMaxDca)               return kFALSE;
  return kTRUE;
}

//_____________________________________________________________________________
Bool_t StMtdQAMaker::isValidTrack(const StPicoTrack *track) const
{
  if(!track) return kFALSE;
  TVector3 mom = track->pMom();
  Float_t pt = mom.Perp();
  Float_t eta = mom.Eta();
  Float_t phi = rotatePhi(mom.Phi());

  if(pt < mMinTrkPt   || pt > mMaxTrkPt)       return kFALSE;
  if(eta < mMinTrkEta || eta > mMaxTrkEta)     return kFALSE;
  if(phi < mMinTrkPhi || phi > mMaxTrkPhi)     return kFALSE;
  if(track->nHitsFit()<mMinNHitsFit)           return kFALSE;
  if(track->nHitsFit()/(1.0*track->nHitsMax())<mMinFitHitsFraction) return kFALSE;
  if(track->nHitsDedx()<mMinNHitsDedx)     return kFALSE;
  double dca = track->gDCA(mPicoDst->event()->primaryVertex()).Mag();
  if(dca>mMaxDca) return kFALSE;
  return kTRUE;
}

//_____________________________________________________________________________
Bool_t StMtdQAMaker::isMuonCandidate(const StMuTrack *track)
{
  if(!track) return kFALSE;
  Double_t pt = track->momentum().perp();
  Double_t nSigmaPi = track->nSigmaPion();

  Double_t dz = -999., dy = -999, dtof = -999.;
  Bool_t isMtdTrig = kFALSE;
  Int_t iMtd = track->index2MtdHit();
  if(iMtd>=0)
    {
      const StMuMtdPidTraits mtdPid = track->mtdPidTraits();
      dy     = mtdPid.deltaY();
      dz     = mtdPid.deltaZ();
      dtof   = mtdPid.timeOfFlight() - mtdPid.expTimeOfFlight();

      StMuMtdHit *hit = mMuDst->mtdHit(iMtd);
      isMtdTrig = mTrigUtil ? mTrigUtil->isHitFireTrigger(hit->backleg(), hit->module()) : kFALSE;
    }

  return isMuonCandidate(pt, nSigmaPi, dz, dy, dtof, isMtdTrig);
}

//_____________________________________________________________________________
Bool_t StMtdQAMaker::isMuonCandidate(const StPicoTrack *track)
{
  if(!track) return kFALSE;
  Double_t pt = track->pMom().Perp();
  Double_t nSigmaPi = track->nSigmaPion();

  double dz = -999., dy = -999, dtof = -999.;
  bool isMtdTrig = kFALSE;
  int iMtd = track->mtdPidTraitsIndex();
  if(iMtd>=0)
    {
      StPicoMtdPidTraits *mtdPid = mPicoDst->mtdPidTraits(iMtd);
      dy = mtdPid->deltaY();
      dz = mtdPid->deltaZ();
      dtof = mtdPid->deltaTimeOfFlight();

      int hitIndex = getMtdHitIndex(track);
      StPicoMtdHit *hit = mPicoDst->mtdHit(hitIndex);
      isMtdTrig = hit->triggerFlag();
    }
	  
  return isMuonCandidate(pt, nSigmaPi, dz, dy, dtof, isMtdTrig);
}


//_____________________________________________________________________________
Bool_t StMtdQAMaker::isMuonCandidate(const Double_t pt, const Double_t nSigmaPi, const Double_t dz, const Double_t dy, const Double_t dtof, const Bool_t isTrig)
{
  if(pt<mMinMuonPt)                                   return kFALSE;
  if(nSigmaPi<mMinNsigmaPi || nSigmaPi>mMaxNsigmaPi)  return kFALSE;
  if(dz<mMinMuonDeltaZ || dz>mMaxMuonDeltaZ)          return kFALSE;
  if(dy<mMinMuonDeltaY || dy>mMaxMuonDeltaY)          return kFALSE;
  if(dtof<mMinMuonDeltaTof || dtof>mMaxMuonDeltaTof)  return kFALSE;
  if(mMtdHitTrigger && !isTrig)                       return kFALSE;

  return kTRUE;
}


//_____________________________________________________________________________
Int_t StMtdQAMaker::getMtdHitIndex(const StPicoTrack *track)
{
  Int_t index = -1;
  if(track->mtdPidTraitsIndex()>=0)
    {
      StPicoMtdPidTraits *mtdPid = mPicoDst->mtdPidTraits(track->mtdPidTraitsIndex());
      Int_t nMtdHits = mPicoDst->numberOfMtdHits();
      for(Int_t i=0; i<nMtdHits; i++)
	{
	  StPicoMtdHit *hit = mPicoDst->mtdHit(i);
	  if(!hit) continue;
	  if(mtdPid->backleg()==hit->backleg() &&
	     mtdPid->module()==hit->module() &&
	     mtdPid->cell()==hit->cell())
	    {
	      index = i;
	      break;
	    }
	}
    }
  return index;
}

//_____________________________________________________________________________
Int_t StMtdQAMaker::getMtdPidTraitsIndex(const StPicoMtdHit *hit)
{
  Int_t index = -1;
  Int_t nPidTraits = mPicoDst->numberOfMtdPidTraits();
  for(Int_t i=0; i<nPidTraits; i++)
    {
      StPicoMtdPidTraits *mtdPid = mPicoDst->mtdPidTraits(i);
      if(!mtdPid) continue;
      if(mtdPid->backleg()==hit->backleg() &&
	 mtdPid->module()==hit->module() &&
	 mtdPid->cell()==hit->cell())
	{
	  index = i;
	  break;
	}
    }
  return index;
}

//_____________________________________________________________________________
Double_t StMtdQAMaker::rotatePhi(Double_t phi) const
{
  Double_t outPhi = phi;
  while(outPhi<0) outPhi += 2*pi;
  while(outPhi>2*pi) outPhi -= 2*pi;
  return outPhi;
}

//
//// $Id: StMtdQAMaker.cxx,v 1.22 2019/03/14 13:11:56 marr Exp $
//// $Log: StMtdQAMaker.cxx,v $
//// Revision 1.22  2019/03/14 13:11:56  marr
//// Add option to apply vertex vr cut
////
//// Revision 1.21  2019/03/13 20:49:18  marr
//// Add option to stwich on/off VtxDz cut
////
//// Revision 1.20  2018/12/04 13:34:29  marr
//// i) Fill MtdVpdTacSum histograms for muon candidates without requiring firing the trigger ii) Fix a bug related a changed in MtdTrigUtil
////
//// Revision 1.19  2018/11/29 20:29:10  marr
//// Major update to QAMaker: i) add picoDst code; ii) remove cosmic type; iii) Add run-dependent QA plots; iv) modifications to regular histograms
////
//// Revision 1.18  2018/08/08 19:29:22  marr
//// MOdify to accomondate the cosmic ray data
////
//// Revision 1.17  2018/02/20 19:46:48  marr
//// Major update with more histograms
////
//// Revision 1.16  2017/03/10 20:16:42  marr
//// 1) Remove the function to apply trigger time window cuts since they are applied
//// already during reconstruction.
//// 2) Accommodate 8-QT system used in 2016
//// 3) Remove the implementation for StEvent QA since it is not maintained
//// 4) Vertex selection for cosmic ray: default one if available
////
//// Revision 1.15  2017/03/01 20:23:59  marr
//// 1) Add option to select different vertex
//// 2) More QA plots for PID variables
////
//// Revision 1.14  2016/08/04 21:26:36  marr
//// Add histograms for vertex QA, and dTof calibration
////
//// Revision 1.13  2016/07/28 14:33:23  marr
//// Fix coverity check: initialization of data member
////
//// Revision 1.12  2016/07/27 16:03:51  marr
//// Fix coverity check: initialization of data member
////
//// Revision 1.11  2015/10/28 19:51:10  marr
//// Remove printout
////
//// Revision 1.10  2015/10/28 19:50:23  marr
//// Add a new data member: mMaxVtxDz
////
//// Revision 1.9  2015/10/23 02:18:51  marr
//// 1) Add histogram for global T0 alignment calibration using primary tracks
//// 2) Add mMaxVtxDz to cut on vz difference between TPC and VPD
////
//// Revision 1.8  2015/04/08 14:03:17  marr
//// change to use gMtdCellDriftV from StMtdConstants.h
////
//// Revision 1.7  2015/02/01 16:26:31  marr
//// 1) Add a new histogram to store the run indices
//// 2) Change the titles of some histograms for better readability
////
//// Revision 1.6  2014/12/11 21:14:13  marr
//// Use (leadTimeW+leadTimeE)/2 instead of leadTimeW for MTD hit time
////
//// Revision 1.5  2014/11/12 18:11:01  marr
//// Minor bug fix
////
//// Revision 1.4  2014/11/12 17:50:11  marr
//// Check the validity of the matched MTD hit
////
//// Revision 1.3  2014/09/19 18:34:55  marr
//// Add histograms for LocalY, LocalZ, DeltaY, DeltaZ
////
//// Revision 1.2  2014/09/16 23:48:58  marr
//// Minor fix such that it compiles under SL5.3, gcc 4.3.2 (rplay17)
////
//// Revision 1.1  2014/09/12 17:13:13  marr
//// Add StMtdQAMaker class for MTD QA analysis
////
