#include <iostream>
#include <math.h>
#include <vector>
#include <stdlib.h>
#include <iterator>

#include "TTree.h"
#include "TH1F.h"
#include "TH2F.h"

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

#include "StMtdUtil/StMtdGeometry.h"
#include "StMtdQAMaker.h"
#include "tables/St_mtdModuleToQTmap_Table.h"
#include "tables/St_mtdQTSlewingCorr_Table.h"
#include "tables/St_mtdQTSlewingCorrPart2_Table.h"

ClassImp(StMtdQAMaker)

//_____________________________________________________________________________
StMtdQAMaker::StMtdQAMaker(const Char_t *name) : 
  StMaker(name),
  mIsCosmic(kFALSE), mStEvent(0), mMuDst(0), mVertexMode(0), mVertexIndex(-1), mRunId(-1), mRunYear(-1),
  mRunCount(0), mTriggerData(0),
  mMuDstIn(kFALSE), mPrintMemory(kFALSE), mPrintCpu(kFALSE), mPrintConfig(kFALSE),
  mTriggerIDs(0),
  mMaxVtxZ(100.), mMaxVtxDz(5.),
  mMinTrkPt(1.), mMaxTrkPt(1e4), mMinTrkPhi(0.), mMaxTrkPhi(2*pi), mMinTrkEta(-0.8), mMaxTrkEta(0.8),
  mMinNHitsFit(15), mMinNHitsDedx(10), mMinFitHitsFraction(0.52), mMaxDca(3.), mMinNsigmaPi(-1.), mMaxNsigmaPi(3.),
  mFillTree(kFALSE), fOutTreeFile(0), mOutTreeFileName(""), mQATree(NULL)
{
  // default constructor
  mTrigTime[0] = -1;
  mTrigTime[1] = -1;

  memset(&mMtdData, 0, sizeof(mMtdData));

  mhEventTrig              = NULL;
  mhEventCuts              = NULL;
  mhRunId                  = NULL;
  mhVertexXY               = NULL;
  mhVertexXZ               = NULL;
  mhVertexYZ               = NULL;
  mhVertexZ                = NULL;
  mhVtxZvsVpdVzDefault     = NULL;
  mhVtxZDiffDefault        = NULL;
  mhVtxZvsVpdVzClosest     = NULL;
  mhVtxZDiffClosest        = NULL;
  mhVtxIndClosestVsRank    = NULL;
  mhTofStartTime           = NULL;
  mhVpdQTadc               = NULL;
  mhVpdQTtac               = NULL;
  mhMtdQTadc               = NULL;
  mhMtdQTAllTac            = NULL;
  mhMtdQTBestTac           = NULL;
  mhMtdMthQTTac            = NULL;
  mhMtdQTvsHit             = NULL;
  mhMtdVpdTacDiffMT001     = NULL;
  mhMtdVpdMthTacDiffMT001  = NULL;
  mhMtdVpdTacDiffMT101     = NULL;
  mhMtdVpdMthTacDiffMT101  = NULL;
  mhMtdQTJ2J3Diff          = NULL;
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
  mhMtdRawHitTrigTime      = NULL;
  mhMtdRawHitLeNEast       = NULL;
  mhMtdRawHitLeNWest       = NULL;
  mhMtdRawHitTrNEast       = NULL;
  mhMtdRawHitTrNWest       = NULL;
  mhMtdRawHitLeNDiff       = NULL;
  mhMtdRawHitTrNDiff       = NULL;

  mhMtdNHits               = NULL;
  mhMtdHitMap              = NULL;
  mhMtdHitLeTimeWest       = NULL;
  mhMtdHitLeTimeEast       = NULL;
  mhMtdHitLeTimeDiff       = NULL;
  mhMtdHitTotWest          = NULL;
  mhMtdHitTotEast          = NULL;
  mhMtdHitTrigTime         = NULL;

  mhMtdNMatchHits          = NULL;
  mhMtdMatchHitMap         = NULL;
  mhMtdMatchPhi            = NULL;
  mhMtdMatchDzVsChan       = NULL;
  mhMtdMatchDyVsChan       = NULL;
  mhMtdMatchDtofVsChan     = NULL;
  mhMtdMatchLocalyVsChan   = NULL;
  mhMtdMatchLocalzVsChan   = NULL;
  mhMtdMatchDzVsPt         = NULL;
  mhMtdMatchDyVsPt         = NULL;
  mhMtdMatchDtofVsPt       = NULL;
  mhMtdMatchTrkPt          = NULL;
  mhMtdMatchTrkPhiEta      = NULL;
  mhMtdMatchTrkDedx        = NULL;

  mhTrkPt                  = NULL;
  mhTrkDca                 = NULL;
  mhTrkPhiEta              = NULL;
  mhMtdTrackProjMap        = NULL;
  mhTrkProjPhiZAtMtd       = NULL;
  mhTrkPhiVsMtdPhi         = NULL;
  mhTofMthTrkLocaly        = NULL;
  mhTofMthTrkLocalz        = NULL;

  mhPrimDzVsChan           = NULL;
  mhPrimDyVsChan           = NULL;
  mhPrimDtofVsChan         = NULL;
  mhPrimMtdTofVsChan       = NULL;
  mhPrimExpTofVsChan       = NULL;
}
 
//_____________________________________________________________________________
StMtdQAMaker::~StMtdQAMaker()
{
  // default destructor
  if(mQATree)      delete mQATree;
  if(fOutTreeFile) delete fOutTreeFile;
}


//_____________________________________________________________________________
Int_t StMtdQAMaker::InitRun(const Int_t runNumber)
{
  // run Id
  mRunId = runNumber;
  mRunCount++;
  mhRunId->GetXaxis()->SetBinLabel(mRunCount,Form("%d",mRunId));

  // initialize maps
  memset(mModuleToQT,-1,sizeof(mModuleToQT));
  memset(mModuleToQTPos,-1,sizeof(mModuleToQTPos));
  memset(mQTtoModule,-1,sizeof(mQTtoModule));

  // obtain maps from DB
  LOG_INFO << "Retrieving mtdModuleToQTmap table from database ..." << endm;
  TDataSet *dataset = GetDataBase("Geometry/mtd/mtdModuleToQTmap");
  St_mtdModuleToQTmap *mtdModuleToQTmap = static_cast<St_mtdModuleToQTmap*>(dataset->Find("mtdModuleToQTmap"));
  if(!mtdModuleToQTmap)
    {
      LOG_ERROR << "No mtdModuleToQTmap table found in database" << endm;
      return kStErr;
    }
  mtdModuleToQTmap_st *mtdModuleToQTtable = static_cast<mtdModuleToQTmap_st*>(mtdModuleToQTmap->GetTable());

  for(Int_t i=0; i<gMtdNBacklegs; i++)
    {
      for(Int_t j=0; j<gMtdNModules; j++)
	{
	  Int_t index = i*5 + j;
	  Int_t qt = mtdModuleToQTtable->qtBoardId[index];
	  Int_t channel = mtdModuleToQTtable->qtChannelId[index];
	  mModuleToQT[i][j]    = qt;
	  if(channel<0)
	    {
	      mModuleToQTPos[i][j] = channel;
	    }
	  else
	    {
	      if(channel%8==1) mModuleToQTPos[i][j] = 1 + channel/8 * 2;
	      else             mModuleToQTPos[i][j] = 2 + channel/8 * 2;
	    }
	  if(mModuleToQT[i][j]>0 && mModuleToQTPos[i][j]>0)
	    mQTtoModule[mModuleToQT[i][j]-1][mModuleToQTPos[i][j]-1] = j + 1;
	}
    }

  // online slewing correction for QT board
  memset(mQTSlewBinEdge,-1,sizeof(mQTSlewBinEdge));
  memset(mQTSlewCorr,-1,sizeof(mQTSlewCorr));
  LOG_INFO << "Retrieving mtdQTSlewingCorr table from database ..." << endm;
  dataset = GetDataBase("Calibrations/mtd/mtdQTSlewingCorr");
  St_mtdQTSlewingCorr *mtdQTSlewingCorr = static_cast<St_mtdQTSlewingCorr*>(dataset->Find("mtdQTSlewingCorr"));
  if(!mtdQTSlewingCorr)
    {
      LOG_ERROR << "No mtdQTSlewingCorr table found in database" << endm;
      return kStErr;
    }
  mtdQTSlewingCorr_st *mtdQTSlewingCorrtable = static_cast<mtdQTSlewingCorr_st*>(mtdQTSlewingCorr->GetTable());
  for(int j=0; j<4; j++)
    {
      for(int i=0; i<16; i++)
        {
          for(Int_t k=0; k<8; k++)
            {
              Int_t index = j*16*8 + i*8 + k;
              mQTSlewBinEdge[j][i][k] = (int) mtdQTSlewingCorrtable->slewingBinEdge[index];
              mQTSlewCorr[j][i][k] = (int) mtdQTSlewingCorrtable->slewingCorr[index];
            }
        }
    }

  dataset = GetDataBase("Calibrations/mtd/mtdQTSlewingCorrPart2");
  if(dataset)
    {
      St_mtdQTSlewingCorrPart2 *mtdQTSlewingCorr2 = static_cast<St_mtdQTSlewingCorrPart2*>(dataset->Find("mtdQTSlewingCorrPart2"));
      mtdQTSlewingCorrPart2_st *mtdQTSlewingCorrtable2 = static_cast<mtdQTSlewingCorrPart2_st*>(mtdQTSlewingCorr2->GetTable());
      for(int j=0; j<4; j++)
        {
          for(int i=0; i<16; i++)
            {
              for(Int_t k=0; k<8; k++)
                {
                  Int_t index = j*16*8 + i*8 + k;
                  mQTSlewBinEdge[j+4][i][k] = (int) mtdQTSlewingCorrtable2->slewingBinEdge[index];
                  mQTSlewCorr[j+4][i][k] = (int) mtdQTSlewingCorrtable2->slewingCorr[index];
                }
            }
        }
    }
  LOG_INFO << "===== End retrieving mtdQTSlewingCorr =====" << endm;

  return kStOK;
}


//_____________________________________________________________________________
Int_t StMtdQAMaker::Init()
{
  if(mPrintConfig) printConfig();
  if(mFillTree) bookTree();
  bookHistos();

  return kStOK;
}

//_____________________________________________________________________________
Int_t StMtdQAMaker::Finish()
{  
  if(fOutTreeFile)
    {
      fOutTreeFile->Write();
      fOutTreeFile->Close();
      LOG_INFO << "StMtdQAMaker::Finish() -> write out tree in " << mOutTreeFileName.Data() << endm;
    }

  return kStOK;
}

//_____________________________________________________________________________
Int_t StMtdQAMaker::Make()
{

  StTimer timer;
  if (mPrintMemory) StMemoryInfo::instance()->snapshot();
  if (mPrintCpu)    timer.start();

  // reset the structure
  memset(&mMtdData, 0, sizeof(mMtdData));

  mhRunId->Fill(mRunCount-0.5);

  // Check the availability of input data
  Int_t iret;
  StMuDstMaker *muDstMaker = (StMuDstMaker*) GetMaker("MuDst");
  if(muDstMaker) 
    {
      mMuDst = muDstMaker->muDst();
      if(mMuDst)
	{
	  LOG_DEBUG << "Running on MuDst ..." << endm;
	  mMuDstIn = kTRUE;
	  iret = processMuDst();
	}
      else
	{
	  LOG_ERROR << "No muDST is available ... "<< endm;
	  return kStErr;
	}
    }
  else
    {
      mStEvent = (StEvent*) GetInputDS("StEvent");
      if(mStEvent)
	{
	  LOG_DEBUG << "Running on StEvent ..." << endm;
	  mMuDstIn = kFALSE;
	  iret = processStEvent();
	}
      else
	{
	  LOG_ERROR << "No StEvent is available ..." << endm;
	  return kStErr;
	}
    }

  if(iret == kStOK)
    {
      if(mTriggerData) processTriggerData();
      if(mFillTree) mQATree->Fill();
      fillHistos();
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

  return kStOK;
}


//_____________________________________________________________________________
Int_t StMtdQAMaker::processStEvent()
{
  // Event statistics
  mhEventTrig->Fill(0.5);
  return kStOK;
}


//_____________________________________________________________________________
Int_t StMtdQAMaker::processMuDst()
{
  // Event statistics
  mhEventTrig->Fill(0.5);

  // select valid triggers
  mTriggerData = 0;
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

  if(!isGoodTrigger) 
    {
      LOG_WARN << "No valid trigger in MuDst... " << endm;
      return kStWarn;
    }
  mhEventTrig->Fill(1.5);
  mRunId   = mMuDst->event()->runId();
  mRunYear = mRunId / 1e6 + 1999;

  //========== Select vertex ==========
  mVertexIndex = -1;
  Int_t nPrim = mMuDst->numberOfPrimaryVertices();
  StMuPrimaryVertex* priVertex = NULL;
  if(mIsCosmic)
    {
      if(nPrim>=1) 
	{
	  mVertexIndex = 0;
	  priVertex = mMuDst->primaryVertex(mVertexIndex);
	  if(priVertex)
	    {
	      StThreeVectorF verPos = priVertex->position();
	      mMtdData.vertexX = verPos.x();
	      mMtdData.vertexY = verPos.y();
	      mMtdData.vertexZ = verPos.z();
	    }
	}
    }
  else
    {
      if(nPrim == 0) 
	{
	  LOG_WARN << "No reconstructed vertex in MuDst... " << endm;
	  return kStWarn;
	}

      // start time & VPD vz
      StBTofHeader *tofHeader = mMuDst->btofHeader();
      Double_t tStart = -999;
      Double_t vpdz   = -999;
      if(tofHeader)
	{
	  tStart = tofHeader->tStart();
	  vpdz   = tofHeader->vpdVz();
	}
      mMtdData.tofStartTime = tStart;
      mMtdData.vpdVz        = vpdz;

      Int_t index1 = -1, index2 = -1;
      if(tofHeader)
	{
	  // constrain vertex with VPD
	  Double_t min_dz = 999;
	  for(Int_t i=0; i<nPrim; i++)
	    {
	      StMuPrimaryVertex *vertex = mMuDst->primaryVertex(i);
	      Double_t dz = TMath::Abs(vertex->position().z()-mMtdData.vpdVz);
	      if(dz<mMaxVtxDz && index1==-1)
		{
		  index1 = i;
		}
	      if(dz<min_dz)
		{
		  min_dz = dz;
		  index2 = i;
		}
	    }
	  mhVtxIndClosestVsRank->Fill(index2,index1);
	  
	  double default_z = mMuDst->primaryVertex(0)->position().z();
	  mhVtxZvsVpdVzDefault->Fill(default_z, mMtdData.vpdVz);
	  mhVtxZDiffDefault->Fill(default_z - mMtdData.vpdVz);
	  if(index2>-1)
	    {
	      double cloest_z = mMuDst->primaryVertex(index2)->position().z();
	      mhVtxZvsVpdVzClosest->Fill(cloest_z, mMtdData.vpdVz);
	      mhVtxZDiffClosest->Fill(cloest_z - mMtdData.vpdVz);
	    }
	}
      
      if(mVertexMode==0) mVertexIndex = 0;
      else if(mVertexMode==1) mVertexIndex = index1;
      else if(mVertexMode==2) mVertexIndex = index2;
      else
	{
	  LOG_WARN << "No vertex mode is set. Use default vertex!" << endm;
	  mVertexIndex = 0;
	}

      if(mVertexIndex<0) return kStOK;
      priVertex = mMuDst->primaryVertex(mVertexIndex);
      if(!priVertex) return kStWarn;
      StThreeVectorF verPos = priVertex->position();
      mMtdData.vertexX = verPos.x();
      mMtdData.vertexY = verPos.y();
      mMtdData.vertexZ = verPos.z();
      if(TMath::Abs(mMtdData.vertexZ)>mMaxVtxZ) return kStOK;
      if(fabs(mMtdData.vpdVz-mMtdData.vertexZ)>mMaxVtxDz) return kStOK;
    }
  mhEventTrig->Fill(2.5);
  mhVertexZ->Fill(mMtdData.vertexZ);
  mhVertexXY->Fill(mMtdData.vertexX,mMtdData.vertexY);
  mhVertexXZ->Fill(mMtdData.vertexZ,mMtdData.vertexX);
  mhVertexYZ->Fill(mMtdData.vertexZ,mMtdData.vertexY);
  //====================================

  mMtdData.runId   = mRunId;
  mMtdData.eventId = mMuDst->event()->eventId();

  // collect trigger information
  Int_t nTrigger = 0;
  for(UInt_t i=0; i<mTriggerIDs.size(); i++)
    {
      if(mMuDst->event()->triggerIdCollection().nominal().isTrigger(mTriggerIDs[i]))
	{
	  mMtdData.triggerId[nTrigger] = mTriggerIDs[i];
	  mhEventTrig->Fill(3.5+i);
	  nTrigger++;
	}
    }
  mMtdData.nTrigger = nTrigger;
  mTriggerData = const_cast<StTriggerData*>(mMuDst->event()->triggerData());

  // MTD trigger time
  StMuMtdHeader *muMtdHeader = mMuDst->mtdHeader();
  if(muMtdHeader)
    {
      mMtdData.mtdTriggerTime[0] = 25.*(muMtdHeader->triggerTime(1)&0xfff);
      mMtdData.mtdTriggerTime[1] = 25.*(muMtdHeader->triggerTime(2)&0xfff);
    }
  else
    {
      mMtdData.mtdTriggerTime[0] = -99999;
      mMtdData.mtdTriggerTime[1] = -99999;
    }
  for(Int_t i=0; i<2; i++)
    mTrigTime[i] = mMtdData.mtdTriggerTime[i];

  // MTD raw hits
  Int_t nMtdRawHits = mMuDst->numberOfBMTDRawHit();
  LOG_DEBUG << nMtdRawHits << " raw MTD hits" << endm;
  for(Int_t i=0; i<nMtdRawHits; i++)
    {
      StMuMtdRawHit *rawHit = (StMuMtdRawHit*)mMuDst->mtdRawHit(i);
      if(!rawHit) continue;
      Int_t backleg = rawHit->backleg();
      if(backleg<1 || backleg>30) continue;
      mMtdData.mtdRawHitFlag[i]    = rawHit->flag();
      mMtdData.mtdRawHitBackleg[i] = backleg;
      mMtdData.mtdRawHitChan[i]    = rawHit->channel();
      mMtdData.mtdRawHitModule[i]  = (rawHit->channel()-1)/gMtdNChannels+1;
      mMtdData.mtdRawHitTdc[i]     = rawHit->tdc()*gMtdConvertTdcToNs;
      Double_t tDiff = mMtdData.mtdRawHitTdc[i] - mMtdData.mtdTriggerTime[rawHit->fiberId()];
      while(tDiff<0) tDiff += 51200;
      mMtdData.mtdRawHitTimdDiff[i] = tDiff;
    }
  mMtdData.nMtdRawHits = nMtdRawHits;

  // Tracks
  map<Short_t, UShort_t> globalIndex;
  Int_t nNodes = mMuDst->numberOfGlobalTracks();
  for(Int_t i=0; i<nNodes; i++)
    {
      StMuTrack* gTrack = mMuDst->globalTracks(i);
      if(!gTrack) continue;
      globalIndex[gTrack->id()] = i;
      mhTrkDca->Fill(gTrack->pt(), gTrack->dcaGlobal().mag());
    }

  // MTD hits
  Int_t nMtdHits = mMuDst->numberOfMTDHit();
  Int_t nMatchMtdHit = 0;
  LOG_DEBUG << "# of mtd hits: " << nMtdHits << endm;
  for(Int_t i=0; i<nMtdHits; i++)
    {
      StMuMtdHit *hit = mMuDst->mtdHit(i);
      if(!hit) continue;
      Int_t backleg = hit->backleg();
      if(backleg<1 || backleg>30) continue;
      mMtdData.mtdHitBackleg[i]    = backleg;
      mMtdData.mtdHitModule[i]     = hit->module();
      mMtdData.mtdHitChan[i]       = hit->cell();
      mMtdData.mtdHitLeTimeWest[i] = hit->leadingEdgeTime().first;
      mMtdData.mtdHitLeTimeEast[i] = hit->leadingEdgeTime().second;
      mMtdData.mtdHitTotWest[i]    = hit->tot().first;
      mMtdData.mtdHitTotEast[i]    = hit->tot().second;
      Int_t tHub = getMtdHitTHUB(backleg);
      Double_t tDiff = (mMtdData.mtdHitLeTimeWest[i]+mMtdData.mtdHitLeTimeEast[i])/2 - mMtdData.mtdTriggerTime[tHub-1];
      while(tDiff<0) tDiff += 51200;
      //cout << (mMtdData.mtdHitLeTimeWest[i]+mMtdData.mtdHitLeTimeEast[i])/2 << " - " << mMtdData.mtdTriggerTime[tHub-1] << " = " << tDiff << endl;
      mMtdData.mtdHitTrigTime[i] = tDiff;
      mMtdData.isGoodMtdHit[i] = kFALSE;

      mMtdData.mtdHitPhi[i]        = getMtdHitGlobalPhi(hit);
      mMtdData.mtdHitZ[i]          = getMtdHitGlobalZ(hit);
      mMtdData.isMatched[i] = kFALSE;
      Short_t trackId = hit->associatedTrackKey();
      if(trackId<1) continue;
      Int_t index = globalIndex[trackId];
      if(index<0) continue;
      StMuTrack *gTrack = mMuDst->globalTracks(index);
      if(!gTrack || !isValidTrack(gTrack)) continue;
      StThreeVectorF trkMom = gTrack->momentum();
      const StMuMtdPidTraits mtdPid = gTrack->mtdPidTraits();
      StThreeVectorF projPos = mtdPid.position();
      mMtdData.isMatched[i] = kTRUE;
      if(gTrack->primaryTrack()->vertexIndex()==mVertexIndex) 
	mMtdData.isMatchedPrim[i] = kTRUE;
      else                       
	mMtdData.isMatchedPrim[i] = kFALSE;
      mMtdData.mtdMatchTrkPathLength[i] = mtdPid.pathLength();
      mMtdData.mtdMatchTrkTof[i]        = mtdPid.timeOfFlight();
      mMtdData.mtdMatchTrkExpTof[i]     = mtdPid.expTimeOfFlight();
      mMtdData.mtdMatchTrkLocaly[i]     = mtdPid.yLocal();
      mMtdData.mtdMatchTrkLocalz[i]     = mtdPid.zLocal();
      mMtdData.mtdMatchTrkDeltay[i]     = mtdPid.deltaY();
      mMtdData.mtdMatchTrkDeltaz[i]     = mtdPid.deltaZ();
      mMtdData.mtdMatchTrkProjPhi[i]    = rotatePhi(projPos.phi());
      mMtdData.mtdMatchTrkProjZ[i]      = projPos.z();
      mMtdData.mtdMatchTrkPt[i]         = trkMom.perp();
      mMtdData.mtdMatchTrkEta[i]        = trkMom.pseudoRapidity();
      mMtdData.mtdMatchTrkPhi[i]        = rotatePhi(trkMom.phi());
      mMtdData.mtdMatchTrkDedx[i]       = gTrack->dEdx() * 1e6;
      mMtdData.mtdMatchTrkNsigmaPi[i]   = gTrack->nSigmaPion();
      nMatchMtdHit++;
      LOG_DEBUG << "MTD hit " << i << ", and is matched to track " << index << endm; 
    }
  mMtdData.nMtdHits = nMtdHits;
  mMtdData.nMatchMtdHits = nMatchMtdHit;

  // Tracks
  Int_t goodTrack = 0;
  Double_t projPhi = -999, projZ = -999;
  Int_t backleg = -1, module = -1, cell = -1;
  for(Int_t i=0; i<nNodes; i++)
    {
      StMuTrack* gTrack = mMuDst->globalTracks(i);
      if(!gTrack || !isValidTrack(gTrack)) continue;
      mMtdData.trkPt[goodTrack]           = gTrack->pt();
      mMtdData.trkEta[goodTrack]          = gTrack->eta();
      mMtdData.trkPhi[goodTrack]          = rotatePhi(gTrack->phi());
      mMtdData.trkNsigmaPi[goodTrack]     = gTrack->nSigmaPion();

      // TOF matching
      mMtdData.isTrkTofMatched[goodTrack]  = kFALSE;
      const StMuBTofHit *tofHit = gTrack->tofHit();
      if(tofHit) 
	{
	  const StMuBTofPidTraits &tofPid = gTrack->btofPidTraits();
	  mMtdData.isTrkTofMatched[goodTrack]  = kTRUE;
	  mMtdData.trkMthTofTray[goodTrack]    = tofHit->tray();
	  mMtdData.trkMthTofModule[goodTrack]  = tofHit->module();
	  mMtdData.trkMthTofCell[goodTrack]    = tofHit->cell();
	  mMtdData.trkMthTofLocaly[goodTrack]  = tofPid.yLocal();
	  mMtdData.trkMthTofLocalz[goodTrack]  = tofPid.zLocal();
	  LOG_DEBUG << "TOF tray = " << tofHit->tray() << ", module = " << tofHit->module() << ", cell = " << tofHit->cell() << ", local y = " << tofPid.yLocal() << ", local z = " << tofPid.zLocal() << endm;
	}

      // MTD matching
      mMtdData.isTrkProjected[goodTrack]  = kFALSE;
      mMtdData.isTrkMtdMatched[goodTrack] = kFALSE;
      mMtdData.isGoodMthMtdHit[goodTrack] = kFALSE;
      StPhysicalHelixD gHelix = gTrack->outerHelix();
      if(propagateHelixToMtd(gHelix, projPhi, projZ))
	{
	  getMtdPosFromProj(projPhi, projZ, backleg, module, cell);
	  mMtdData.isTrkProjected[goodTrack] = kTRUE;
	  mMtdData.trkProjPhi[goodTrack]     = projPhi;
	  mMtdData.trkProjZ[goodTrack]       = projZ;
	  mMtdData.trkProjBackleg[goodTrack] = backleg;
	  mMtdData.trkProjModule[goodTrack]  = module;
	  mMtdData.trkProjChannel[goodTrack] = cell;
	  Int_t iMtd = gTrack->index2MtdHit();
	  backleg = -1, module = -1, cell = -1;
	  if(iMtd>-1)
	    {
	      mMtdData.isTrkMtdMatched[goodTrack] = kTRUE;
	      StMuMtdHit *hit = mMuDst->mtdHit(iMtd);
	      if(hit)
		{
		  mMtdData.isGoodMthMtdHit[goodTrack] = mMtdData.isGoodMtdHit[iMtd];
		  backleg = hit->backleg();
		  module  = hit->module();
		  cell = hit->cell();
		  LOG_DEBUG << "Track " << i << " is matched to MTD hit " << iMtd << endm;
		}
	    }
	  mMtdData.trkMthBackleg[goodTrack] = backleg;
	  mMtdData.trkMthModule[goodTrack]  = module;
	  mMtdData.trkMthChannel[goodTrack] = cell;
	}
      goodTrack++;  
    }
  mMtdData.nGoodTrack = goodTrack;


  return kStOK;
}

//_____________________________________________________________________________
void StMtdQAMaker::processTriggerData()
{
  Int_t pre = mTriggerData->numberOfPreXing();
  Int_t post = mTriggerData->numberOfPostXing();
  Int_t prepost = pre + post + 1;

  // VPD tac information
  const Int_t ip = 0;
  mMtdData.fasteastHi = mTriggerData->vpdEarliestTDCHighThr(east,ip);
  mMtdData.fastwestHi = mTriggerData->vpdEarliestTDCHighThr(west,ip);

  for(Int_t i=0; i<kMaxVpdChan/4; i++)
    {
      mMtdData.vpdHi[i/8*8+i] = mTriggerData->vpdADCHighThr(east,i+1,ip);
      mMtdData.vpdHi[i/8*8+8+i] = mTriggerData->vpdTDCHighThr(east,i+1,ip);
      
      mMtdData.vpdHi[i/8*8+32+i] = mTriggerData->vpdADCHighThr(west,i+1,ip);
      mMtdData.vpdHi[i/8*8+40+i] = mTriggerData->vpdTDCHighThr(west,i+1,ip);
    }

  // MTD QT information
  for(Int_t i=0; i<kMaxMtdQTchan; i++)
    {
      Int_t type = (i/4)%2;
      for(int im=0; im<kNQTboard; im++)
	{
	  if(mRunYear!=2016 && im>=4) continue;
	  if(type==0) mMtdData.mtdQTadc[im][i-i/4*2]   = mTriggerData->mtdQtAtCh(im+1,i,0);
	  else        mMtdData.mtdQTtac[im][i-i/4*2-2] = mTriggerData->mtdQtAtCh(im+1,i,0);
	}
    }

  // MTD MIX trigger information
  for(Int_t i=0; i<32; i++)
    {
      mMtdData.mixMtdTacSum[i] =  mTriggerData->mtdDsmAtCh(i,ip);
    }						
  mMtdData.pre = pre;
  mMtdData.post = post;
  mMtdData.prepost = prepost;
}

//_____________________________________________________________________________
void StMtdQAMaker::fillHistos()
{
  // TOF histograms
  mhTofStartTime->Fill(mMtdData.tofStartTime);

  // QT histograms
  Double_t vpdTacSum = mMtdData.fasteastHi + mMtdData.fastwestHi;

  for(Int_t i=0; i<kMaxVpdChan; i++)
    {
      Int_t type = (i/8)%2;
      if(type==0) mhVpdQTadc->Fill(i+1,mMtdData.vpdHi[i]);
      else        mhVpdQTtac->Fill(i+1,mMtdData.vpdHi[i]);
    }  

  // trigger performance
  Int_t maxm = -1, maxi = -1;
  UShort_t maxTac = 0;
  UShort_t mxq_mtdtacsum[kNQTboard][2];
  Int_t    mxq_tacsum_pos[kNQTboard][2];
  for(Int_t i=0; i<kNQTboard; i++)
    {
      for(Int_t j=0; j<2; j++)
	{
	  mxq_mtdtacsum[i][j] = 0;
	  mxq_tacsum_pos[i][j] = -1;
	}
    }

  UShort_t mtd_qt_tac_min = 100;
  if(mRunId >= 16045067) mtd_qt_tac_min = 80;
  UShort_t mtd_qt_tac_diff_range_abs = 600;
  if(mRunYear == 2015) mtd_qt_tac_diff_range_abs = 1023;
  if(mhEventCuts->GetBinContent(7)==0)
    {
      mhEventCuts->SetBinContent(7,mtd_qt_tac_min);
      mhEventCuts->SetBinContent(9,mtd_qt_tac_diff_range_abs);
    }

  Int_t nMtdHits = mMtdData.nMtdHits;
  Int_t j[2], a[2];
  for(Int_t im=0; im<kNQTboard; im++)
    {
      for(Int_t i=0; i<8; i++)
	{
	  if(mRunYear!=2016 && im>=4)  continue;
	  if(mRunYear==2016 && i%2==0) continue;
	  for(Int_t k=0; k<2; k++)
	    {
	      j[k] = mMtdData.mtdQTtac[im][i*2+k];
	      a[k] = mMtdData.mtdQTadc[im][i*2+k];
	      
	      int index = 0;
	      if(mRunYear!=2016) index = im*16 + i*2 + k + 1;
	      else               index = im*8 + (i/2)*2 + k + 1;
	      if(a[k]>100) mhMtdQTadc->Fill(index,a[k]);
	      if(j[k]>mtd_qt_tac_min) mhMtdQTAllTac->Fill(index,j[k]);

	      // slewing correction
	      int slew_bin = -1;
	      if(a[k]>=0 && a[k]<=mQTSlewBinEdge[im][i*2+k][0]) slew_bin = 0;
	      else
		{
		  for(int l=1; l<8; l++)
		    {
		      if(a[k]>mQTSlewBinEdge[im][i*2+k][l-1] && a[k]<=mQTSlewBinEdge[im][i*2+k][l])
			{
			  slew_bin = l;
			  break;
			}
		    }
		}
	      if(slew_bin>=0)
		j[k] += mQTSlewCorr[im][i*2+k][slew_bin];
	    }

	  if(j[0]<mtd_qt_tac_min || j[0]>mtd_qt_tac_max || 
	     j[1]<mtd_qt_tac_min || j[1]>mtd_qt_tac_max ||
	     TMath::Abs(j[0]-j[1])>mtd_qt_tac_diff_range_abs) continue;

	  // position correction
	  int module = mQTtoModule[im][i];
	  Int_t sumTac = int( j[0] + j[1] + abs(module-3)*1./8 * (j[0]-j[1]) );

	  if(maxTac < sumTac)
	    {
	      maxm = im;
	      maxi = i;
	      maxTac = sumTac;
	    }

	  if(mxq_mtdtacsum[im][0] < sumTac)
	    {
	      mxq_mtdtacsum[im][1] = mxq_mtdtacsum[im][0];
	      mxq_mtdtacsum[im][0] = sumTac;

	      mxq_tacsum_pos[im][1] = mxq_tacsum_pos[im][0];
	      mxq_tacsum_pos[im][0] = i+1;
	    }
	  else if (mxq_mtdtacsum[im][1] < sumTac)
	    {
	      mxq_mtdtacsum[im][1]  = sumTac;
	      mxq_tacsum_pos[im][1] = i+1;
	    }
	  
	  int bin = 0;
	  if(mRunYear!=2016) bin = im*8+i+1;
	  else               bin = im*4+i/2+1;
	  mhMtdQTJ2J3Diff->Fill(bin,j[1]-j[0]);
	  mhMtdVpdTacDiffMT001->Fill(bin,sumTac-vpdTacSum);

	  // find corresponding MTD hits
	  for(Int_t i=0; i<nMtdHits; i++)
	    {
	      Int_t backleg = mMtdData.mtdHitBackleg[i];
	      Int_t tray    = mMtdData.mtdHitModule[i];
	      if(backleg<1 || backleg>30 || tray<1 || tray>5) continue;
	      Int_t qt = mModuleToQT[backleg-1][tray-1];
	      Int_t chan = mModuleToQTPos[backleg-1][tray-1];
	      if(qt<0 || chan<0) continue;
	      mhMtdQTvsHit->Fill(bin,chan+(qt-1)*8);
	    }
	}
    }
  
  if(maxm>=0 && maxi>=0)
    {
      if(mRunYear!=2016)
	{
	  mhMtdQTBestTac->Fill(maxm*16+maxi*2+1,mMtdData.mtdQTtac[maxm][maxi*2]);
	  mhMtdQTBestTac->Fill(maxm*16+maxi*2+2,mMtdData.mtdQTtac[maxm][maxi*2+1]);
	}
      else
	{
	  mhMtdQTBestTac->Fill(maxm*8+(maxi/2)*2+1,mMtdData.mtdQTtac[maxm][maxi*2]);
	  mhMtdQTBestTac->Fill(maxm*8+(maxi/2)*2+2,mMtdData.mtdQTtac[maxm][maxi*2+1]);
	}
    }

  UShort_t mix_mtdtacsum[kNQTboard][2];
  for(Int_t im=0; im<kNQTboard; im++)
    {
      if(mRunYear!=2016 && im>=4)  continue;

      int idx = 0;
      if(mRunYear == 2016) idx = im/2*3 + im%2*16;
      else                 idx = im*3;
      mix_mtdtacsum[im][0] = (mMtdData.mixMtdTacSum[idx]) + ((mMtdData.mixMtdTacSum[idx+1]&0x3)<<8);
      mix_mtdtacsum[im][1] = (mMtdData.mixMtdTacSum[idx+1]>>4) + ((mMtdData.mixMtdTacSum[idx+2]&0x3f)<<4);
      for(Int_t j=0; j<2; j++)
	{
	  if(mix_mtdtacsum[im][j]>0) 
	    {
	      mhMixMtdTacSumvsMxqMtdTacSum[im][j]->Fill(mxq_mtdtacsum[im][j]/8,mix_mtdtacsum[im][j]);

	      int bin = 0;
	      if(mRunYear!=2016) bin = im*8+mxq_tacsum_pos[im][j];
	      else               bin = im*4+mxq_tacsum_pos[im][j]/2;
	      mhMtdVpdTacDiffMT101->Fill(bin,mix_mtdtacsum[im][j]*8-vpdTacSum);
	    }
	}
    }

  // MTD histograms
  mhMtdTriggerTime[0]->Fill(mMtdData.mtdTriggerTime[0]);
  mhMtdTriggerTime[1]->Fill(mMtdData.mtdTriggerTime[1]);
  Int_t nMtdRawHits = mMtdData.nMtdRawHits;
  mhMtdNRawHits->Fill(nMtdRawHits); 
  Int_t nDiffLe[kNTotalCells] = {0};
  Int_t nDiffTr[kNTotalCells] = {0};

  for(Int_t i=0; i<nMtdRawHits; i++)
    {
      Int_t backleg = (Int_t)mMtdData.mtdRawHitBackleg[i];
      if(backleg<1 || backleg>30) continue;
      Int_t gChan = (backleg-1)*120 + mMtdData.mtdRawHitChan[i];
      mhMtdRawHitMap->Fill(backleg,mMtdData.mtdRawHitChan[i]);
      Int_t flag = (Int_t)mMtdData.mtdRawHitFlag[i];
      if(flag>0) 
	{
	  mhMtdRawHitLeTime->Fill(gChan,mMtdData.mtdRawHitTdc[i]);
	  mhMtdRawHitTrigTime->Fill(gChan,mMtdData.mtdRawHitTimdDiff[i]);
	}
      else       
	mhMtdRawHitTrTime->Fill(gChan,mMtdData.mtdRawHitTdc[i]);

      Int_t localChan = mMtdData.mtdRawHitChan[i] - (mMtdData.mtdRawHitModule[i]-1) * 24;
      Int_t gCell = (backleg-1)*60 + (mMtdData.mtdRawHitModule[i]-1)*12 + localChan;
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
	  LOG_WARN << "Weird local channel number: " << localChan << " from global channel " << (Int_t)mMtdData.mtdRawHitChan[i] << " and module " << (Int_t)mMtdData.mtdRawHitModule[i] << endm;
	}
    }
  for(Int_t i=0; i<kNTotalCells; i++)
    {
      mhMtdRawHitLeNDiff->Fill(i+1,nDiffLe[i]);
      mhMtdRawHitTrNDiff->Fill(i+1,nDiffTr[i]);
    }
  
  for(Int_t i=0; i<nMtdHits; i++)
    {
      Int_t backleg = mMtdData.mtdHitBackleg[i];
      Int_t module = mMtdData.mtdHitModule[i];
      Int_t lChan = (module-1)*12+mMtdData.mtdHitChan[i];
      Int_t gChan = (backleg-1)*60 + lChan;
      if(backleg<1 || backleg>30) continue;
      mhMtdHitTrigTime->Fill(gChan,mMtdData.mtdHitTrigTime[i]);
      if(!mMtdData.isGoodMtdHit[i]) continue;
      mhMtdHitMap->Fill(backleg,lChan);
      mhMtdHitLeTimeWest->Fill(gChan,mMtdData.mtdHitLeTimeWest[i]);
      mhMtdHitLeTimeEast->Fill(gChan,mMtdData.mtdHitLeTimeEast[i]);
      mhMtdHitLeTimeDiff->Fill(gChan,mMtdData.mtdHitLeTimeEast[i]-mMtdData.mtdHitLeTimeWest[i]);
      mhMtdHitTotWest   ->Fill(gChan,mMtdData.mtdHitTotWest[i]);
      mhMtdHitTotEast   ->Fill(gChan,mMtdData.mtdHitTotEast[i]);
      if(mMtdData.isMatched[i] == 1)
	{
	  double dtof = mMtdData.mtdMatchTrkTof[i]-mMtdData.mtdMatchTrkExpTof[i];
	  double trkPt = mMtdData.mtdMatchTrkPt[i];
	  mhMtdMatchHitMap         ->Fill(backleg,lChan);
	  mhMtdMatchPhi            ->Fill(mMtdData.mtdMatchTrkProjPhi[i],mMtdData.mtdHitPhi[i]);
	  mhMtdMatchTrkPt          ->Fill(trkPt);
	  mhMtdMatchDzVsPt         ->Fill(trkPt,mMtdData.mtdMatchTrkDeltaz[i]);
	  mhMtdMatchDyVsPt         ->Fill(trkPt,mMtdData.mtdMatchTrkDeltay[i]);
	  mhMtdMatchDtofVsPt       ->Fill(trkPt,dtof);
	  mhMtdMatchTrkPhiEta      ->Fill(mMtdData.mtdMatchTrkEta[i],mMtdData.mtdMatchTrkPhi[i]);
	  mhMtdMatchTrkDedx        ->Fill(trkPt,mMtdData.mtdMatchTrkDedx[i]);
	  mhMtdMatchDzVsChan       ->Fill(gChan,mMtdData.mtdMatchTrkDeltaz[i]);
	  mhMtdMatchDyVsChan       ->Fill(gChan,mMtdData.mtdMatchTrkDeltay[i]);
	  mhMtdMatchDtofVsChan     ->Fill(gChan,dtof);
	  mhMtdMatchLocalyVsChan   ->Fill(gChan,mMtdData.mtdMatchTrkLocaly[i]);
	  mhMtdMatchLocalzVsChan   ->Fill(gChan,mMtdData.mtdMatchTrkLocalz[i]);

	  if(mMtdData.isMatchedPrim[i])
	    {
	      mhPrimDzVsChan      ->Fill(gChan, mMtdData.mtdMatchTrkDeltaz[i]);
	      mhPrimDyVsChan      ->Fill(gChan, mMtdData.mtdMatchTrkDeltay[i]);
	      mhPrimDtofVsChan    ->Fill(gChan, dtof);
	      mhPrimMtdTofVsChan  ->Fill(gChan, mMtdData.mtdMatchTrkTof[i]);
	      mhPrimExpTofVsChan  ->Fill(gChan, mMtdData.mtdMatchTrkExpTof[i]);
	    }

	  Int_t qt = mModuleToQT[backleg-1][module-1];
	  Int_t pos = mModuleToQTPos[backleg-1][module-1];
	  if(qt>=1 && qt<=4 && pos>=1 && pos<=8)
	    {
	      UShort_t j2 = mMtdData.mtdQTtac[qt-1][(pos-1)*2];
	      UShort_t j3 = mMtdData.mtdQTtac[qt-1][(pos-1)*2+1];
	      mhMtdMthQTTac->Fill( (qt-1)*16+(pos-1)*2+1, j2);
	      mhMtdMthQTTac->Fill( (qt-1)*16+(pos-1)*2+2, j3);
	      mhMtdVpdMthTacDiffMT001->Fill((qt-1)*8+pos,j2+j3-vpdTacSum+(j2-j3)*abs(module-3)*1./8);

	      Int_t index = -1;
	      if(pos == mxq_tacsum_pos[qt-1][0]) index = 0;
	      if(pos == mxq_tacsum_pos[qt-1][1]) index = 1;
	      if( (index==0 || index==1) && mix_mtdtacsum[qt-1][index]>0 )
		mhMtdVpdMthTacDiffMT101->Fill( (qt-1)*8+pos, mix_mtdtacsum[qt-1][index]*8-vpdTacSum);
	    }
	}
    }

  mhMtdNHits->Fill(nMtdHits);
  mhMtdNMatchHits->Fill(mMtdData.nMatchMtdHits);

  // Track matching
  Int_t nTrack = mMtdData.nGoodTrack;
  for(Int_t i=0; i<nTrack; i++)
    {
      mhTrkPhiEta->Fill(mMtdData.trkEta[i], mMtdData.trkPhi[i]);
      if(mMtdData.isTrkTofMatched[i])
	{
	  Int_t tofTray = mMtdData.trkMthTofTray[i];
	  Int_t tofModule = mMtdData.trkMthTofModule[i];
	  if(tofTray>60 && tofTray<=120) tofModule += 32;
	  
	  mhTofMthTrkLocaly->Fill(tofTray,mMtdData.trkMthTofLocaly[i]);
	  mhTofMthTrkLocalz->Fill(tofModule,mMtdData.trkMthTofLocalz[i]);
	}
      if(!mMtdData.isTrkProjected[i]) continue;
      mhTrkProjPhiZAtMtd->Fill(mMtdData.trkProjZ[i],mMtdData.trkProjPhi[i]);

      Int_t backleg = mMtdData.trkProjBackleg[i];
      Int_t module  = mMtdData.trkProjModule[i];
      Int_t cell    = mMtdData.trkProjChannel[i];
      if(backleg>0 && backleg<=30 && module>=1 && module<=5 && cell>=0 && cell<=11)
	{
	  mhMtdTrackProjMap->Fill(backleg, (module-1)*12+cell);
	}

      for(Int_t j=0; j<nMtdHits; j++)
	{
	  Int_t backleg = mMtdData.mtdHitBackleg[j];
	  if(backleg<1 || backleg>30) continue;
	  if(!mMtdData.isGoodMtdHit[j]) continue;
	  mhTrkPhiVsMtdPhi->Fill(mMtdData.mtdHitPhi[j],mMtdData.trkProjPhi[i]);
	}
    }
}

//_____________________________________________________________________________
void StMtdQAMaker::bookHistos()
{
  // event histograms


//this array describe the Channel input from which backleg & position & direction 

  const  char qtlabel[64][100] = {"QT1-1  25-1-J2","QT1-1  25-1-J3","QT1-2  25-5-J2","QT1-2  25-5-J3","QT1-3  25-2-J2","QT1-3  25-2-J3","QT1-4  25-4-J2","QT1-4  25-4-J3","QT1-5  25-3-J2","QT1-5  25-3-J3","QT1-6  30-3-J2","QT1-6  30-3-J3","QT1-7  30-1-J2","QT1-7  30-1-J3","QT1-8  30-5-J2","QT1-8  30-5-J3","QT2-1  05-1-J2","QT2-1  05-1-J3","QT2-2  05-5-J2","QT2-2  05-5-J3","QT2-3  05-2-J2","QT2-3  05-2-J3","QT2-4  05-4-J2","QT2-4  05-4-J3","QT2-5  05-3-J2","QT2-5  05-3-J3","QT2-6          ","QT2-6          ","QT2-7  30-2-J2","QT2-7  30-2-J3","QT2-8  30-4-J2","QT2-8  30-4-J3","QT3-1  10-1-J2","QT3-1  10-1-J3","QT3-2  10-5-J2","QT3-2  10-5-J3","QT3-3  10-2-J2","QT3-3  10-2-J3","QT3-4  10-4-J2","QT3-4  10-4-J3","QT3-5  10-3-J2","QT3-5  10-3-J3","QT3-6  15-3-J2","QT3-6  15-3-J3","QT3-7          ","QT3-7          ","QT3-8          ","QT3-8          ","QT4-1  21-1-J2","QT4-1  21-1-J3","QT4-2  21-5-J2","QT4-2  21-5-J3","QT4-3  20-2-J2","QT4-3  20-2-J3","QT4-4  20-4-J2","QT4-4  20-4-J3","QT4-5  20-3-J2","QT4-5  20-3-J3","QT4-6          ","QT4-6          ","QT4-7  15-2-J2","QT4-7  15-2-J3","QT4-8  15-4-J2","QT4-8  15-4-J3"};

  const char qtlabel2[32][100] = {"QT1-1  25-1","QT1-2  25-5","QT1-3  25-2","QT1-4  25-4","QT1-5  25-3","QT1-6  30-3","QT1-7  30-1","QT1-8  30-5","QT2-1  05-1","QT2-2  05-5","QT2-3  05-2","QT2-4  05-4","QT2-5  05-3","QT2-6      ","QT2-7  30-2","QT2-8  30-4","QT3-1  10-1","QT3-2  10-5","QT3-3  10-2","QT3-4  10-4","QT3-5  10-3","QT3-6  15-3","QT3-7      ","QT3-8      ","QT4-1  21-1","QT4-2  21-5","QT4-3  20-2","QT4-4  20-4","QT4-5  20-3","QT4-6      ","QT4-7  15-2","QT4-8  15-4"};

  mhEventCuts = new TH1F("hEventCuts","Cuts used for analysis",20,0,20);
  AddHist(mhEventCuts);
  mhEventCuts->GetXaxis()->SetBinLabel(1,"|vtx_z|");
  mhEventCuts->SetBinContent(1,mMaxVtxZ);
  mhEventCuts->GetXaxis()->SetBinLabel(2,"trk_pt_min");
  mhEventCuts->SetBinContent(2,mMinTrkPt);
  mhEventCuts->GetXaxis()->SetBinLabel(3,"trk_pt_max");
  mhEventCuts->SetBinContent(3,mMaxTrkPt);
  mhEventCuts->GetXaxis()->SetBinLabel(4,"trk_eta");
  mhEventCuts->SetBinContent(4,mMaxTrkEta);
  mhEventCuts->GetXaxis()->SetBinLabel(5,"MinNHitsFit");
  mhEventCuts->SetBinContent(5,mMinNHitsFit);
  mhEventCuts->GetXaxis()->SetBinLabel(6,"MinNHitsDedx");
  mhEventCuts->SetBinContent(6,mMinNHitsDedx);
  mhEventCuts->GetXaxis()->SetBinLabel(7,"mtd_qt_tac_min");
  mhEventCuts->GetXaxis()->SetBinLabel(8,"mtd_qt_tac_max");
  mhEventCuts->SetBinContent(8,mtd_qt_tac_max);
  mhEventCuts->GetXaxis()->SetBinLabel(9,"mtd_qt_tac_diff_range_abs");
  mhEventCuts->GetXaxis()->SetBinLabel(12,"mMaxDca");
  mhEventCuts->SetBinContent(12,mMaxDca);
  mhEventCuts->GetXaxis()->SetBinLabel(13,"mMinNsigmaPi");
  mhEventCuts->SetBinContent(13,mMinNsigmaPi);
  mhEventCuts->GetXaxis()->SetBinLabel(14,"mMaxNsigmaPi");
  mhEventCuts->SetBinContent(14, mMaxNsigmaPi);
  mhEventCuts->GetXaxis()->SetBinLabel(15,"mMinFitHitsFraction");
  mhEventCuts->SetBinContent(15, mMinFitHitsFraction);
  mhEventCuts->GetXaxis()->SetBinLabel(16,"mMaxVtxDz");
  mhEventCuts->SetBinContent(16, mMaxVtxDz);


  const Int_t nbins = 3 + mTriggerIDs.size();
  mhEventTrig = new TH1F("hEventStat","Event statistics",nbins,0.,(Float_t)nbins);
  mhEventTrig->GetXaxis()->SetBinLabel(1,"All events");
  mhEventTrig->GetXaxis()->SetBinLabel(2,"Good trigger");
  mhEventTrig->GetXaxis()->SetBinLabel(3,Form("|vtx_z|<%2.0f cm",mMaxVtxZ));
  for(UInt_t i=0; i<mTriggerIDs.size(); i++)
    {
      mhEventTrig->GetXaxis()->SetBinLabel(i+4,Form("%d",mTriggerIDs[i]));
    }
  AddHist(mhEventTrig);
  
  mhRunId = new TH1F("hRunId","Statistics per run",100,0,100);
  AddHist(mhRunId);

  mhVertexXY = new TH2F("hVertexXY","Primary vertex y vs x (TPC);x (cm);y (cm)",100,-5,5,100,-5,5);
  AddHist(mhVertexXY);

  mhVertexXZ = new TH2F("hVertexXZ","Primary vertex x vs z (TPC);z (cm);x (cm)",200,-200,200,100,-5,5);
  AddHist(mhVertexXZ);

  mhVertexYZ = new TH2F("hVertexYZ","Primary vertex y vs z (TPC);z (cm);y (cm)",200,-200,200,100,-5,5);
  AddHist(mhVertexYZ);

  mhVertexZ = new TH1F("hVertexZ","Primary vertex z (TPC); z",200,-200,200);
  AddHist(mhVertexZ);

  mhVtxZvsVpdVzDefault = new TH2F("hVtxZvsVpdVzDefault","Primary vertex z: VPD vs TPC (default);TPC z_{vtx} (cm);VPD z_{vtx} (cm)",201,-201,201,201,-201,201);
  AddHist(mhVtxZvsVpdVzDefault);

  mhVtxZDiffDefault = new TH1F("hVtxZDiffDefault","TPC vz - VPD vz (default); #Deltavz (cm)",400,-20,20);
  AddHist(mhVtxZDiffDefault);

  mhVtxZvsVpdVzClosest = new TH2F("hVtxZvsVpdVzClosest","Primary vertex z: VPD vs TPC (closest);TPC z_{vtx} (cm);VPD z_{vtx} (cm)",201,-201,201,201,-201,201);
  AddHist(mhVtxZvsVpdVzClosest);

  mhVtxZDiffClosest = new TH1F("hVtxZDiffClosest","TPC vz - VPD vz (closest); #Deltavz (cm)",400,-20,20);
  AddHist(mhVtxZDiffClosest);

  mhVtxIndClosestVsRank = new TH2F("hVtxIndClosestVsRank","Vertex indices: close/ranking vs. closest;closest;close/ranking",20,0,20,20,0,20);
  AddHist(mhVtxIndClosestVsRank);

  // TOF histograms
  mhTofStartTime = new TH1F("hTofStartTime","Start time from TOF; t_{start}",40,0,2e5);
  AddHist(mhTofStartTime);

  // QT information
  mhVpdQTadc = new TH2F("hVpdQTadc","VPD QT: ADC vs channel;channel;ADC",64,0.5,64.5,250,0,2500);
  AddHist(mhVpdQTadc);

  mhVpdQTtac = new TH2F("hVpdQTtac","VPD QT: TAC vs channel;channel;TAC",64,0.5,64.5,200,500,2500);
  AddHist(mhVpdQTtac);

  mhMtdQTadc = new TH2F("hMtdQTadc","MTD QT: ADC vs channel;;ADC",64,0.5,64.5,350,0,3500);
  AddHist(mhMtdQTadc);

  mhMtdQTAllTac = new TH2F("hMtdQTAllTac","MTD QT: TAC vs channel (all);;TAC",64,0.5,64.5,150,0,1500);
  AddHist(mhMtdQTAllTac);

  mhMtdQTBestTac = new TH2F("hMtdQTBestTac","MTD QT: TAC vs channel (fastest);;TAC",64,0.5,64.5,150,0,1500);
  AddHist(mhMtdQTBestTac);

  mhMtdMthQTTac = new TH2F("hMtdMthQTTac","MTD QT: TAC vs channel (track-matched);;TAC",64,0.5,64.5,150,0,1500);
  AddHist(mhMtdMthQTTac);
  
  mhMtdQTvsHit = new TH2F("hMtdQTvsHit","MTD QT: QT channel from hits vs from QT",32,0.5,32.5,32,0.5,32.5);
  AddHist(mhMtdQTvsHit);

  mhMtdVpdTacDiffMT001 = new TH2F("hMtdVpdTacDiffMT001","QT: MTD-VPD tac difference with position correction (all);;tac_{MTD}-tac_{VPD}+pos.corr.",32,0.5,32.5,3000,-3000,0);
  AddHist(mhMtdVpdTacDiffMT001);

  mhMtdVpdMthTacDiffMT001 = new TH2F("hMtdVpdMthTacDiffMT001","QT: MTD-VPD tac difference with position correction (track-matched);;tac_{MTD}-tac_{VPD}+pos.corr.",32,0.5,32.5,3000,-3000,0);
  AddHist(mhMtdVpdMthTacDiffMT001);

  mhMtdVpdTacDiffMT101 = new TH2F("hMtdVpdTacDiffMT101","MT101: MTD-VPD tac difference;;tac_{MTD}-tac_{VPD}",32,0.5,32.5,3000,-3000,0);
  AddHist(mhMtdVpdTacDiffMT101);

  mhMtdVpdMthTacDiffMT101 = new TH2F("hMtdVpdMthTacDiffMT101","MT101: MTD-VPD tac difference (track-matched);;tac_{MTD}-tac_{VPD}",32,0.5,32.5,3000,-3000,0);
  AddHist(mhMtdVpdMthTacDiffMT101);

  mhMtdQTJ2J3Diff = new TH2F("hMtdQTJ2J3Diff","MTD QT: J3-J2 TAC vs channel;;TAC (J3-J2)",32,0.5,32.5,80,-400,400);
  AddHist(mhMtdQTJ2J3Diff);

  for(Int_t i=0; i<64; i++)
    {
      mhMtdQTadc->GetXaxis()->SetBinLabel(i+1,qtlabel[i]);
      mhMtdQTAllTac->GetXaxis()->SetBinLabel(i+1,qtlabel[i]);
      mhMtdQTBestTac->GetXaxis()->SetBinLabel(i+1,qtlabel[i]);
      mhMtdMthQTTac->GetXaxis()->SetBinLabel(i+1,qtlabel[i]);
    }
  for(Int_t i=0; i<32; i++)
    {
      mhMtdQTvsHit->GetXaxis()->SetBinLabel(i+1,qtlabel2[i]);
      mhMtdQTvsHit->GetYaxis()->SetBinLabel(i+1,qtlabel2[i]);
      mhMtdVpdTacDiffMT001->GetXaxis()->SetBinLabel(i+1,qtlabel2[i]);
      mhMtdVpdMthTacDiffMT001->GetXaxis()->SetBinLabel(i+1,qtlabel2[i]);
      mhMtdVpdTacDiffMT101->GetXaxis()->SetBinLabel(i+1,qtlabel2[i]);
      mhMtdVpdMthTacDiffMT101->GetXaxis()->SetBinLabel(i+1,qtlabel2[i]);
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

  // MTD histograms
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

  mhMtdRawHitTrigTime = new TH2F("hMtdRawHitTrigTime","MTD: trigger time of raw hit;channel;tdc-t_{trigger} (ns)",3601,-0.5,3600.5,450,2400,3300);
  AddHist(mhMtdRawHitTrigTime);

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
  mhMtdNHits = new TH1F("hMtdNHits","Number of MTD hits per event;N",100,0,100);
  AddHist(mhMtdNHits);

  mhMtdHitMap = new TH2F("hMtdHitMap","MTD: channel vs backleg of hits;backleg;channel",30,0.5,30.5,60,-0.5,59.5);
  AddHist(mhMtdHitMap);

  mhMtdHitLeTimeWest = new TH2F("hMtdHitLeTimeWest","MTD: west leading time of hits;channel;t_{leading} (ns)",1801,-0.5,1800.5,216,0,51200);
  AddHist(mhMtdHitLeTimeWest);

  mhMtdHitLeTimeEast = new TH2F("hMtdHitLeTimeEast","MTD: east leading time of hits;channel;t_{leading} (ns)",1801,-0.5,1800.5,216,0,51200);
  AddHist(mhMtdHitLeTimeEast);

  mhMtdHitLeTimeDiff = new TH2F("hMtdHitLeTimeDiff","MTD: (east-west) leading time of hits;channel;#Deltat_{leading} (ns)",1801,-0.5,1800.5,41,-20.5,20.5);
  AddHist(mhMtdHitLeTimeDiff);

  mhMtdHitTotWest = new TH2F("hMtdHitTotWest","MTD: west TOT of hits;channel;tot (ns)",1801,-0.5,1800.5,50,0,50);
  AddHist(mhMtdHitTotWest);

  mhMtdHitTotEast = new TH2F("hMtdHitTotEast","MTD: east TOT of hits;channel;tot (ns)",1801,-0.5,1800.5,50,0,50);
  AddHist(mhMtdHitTotEast);

  mhMtdHitTrigTime = new TH2F("hMtdHitTrigTime","MTD: trigger time of hit (west+east)/2;channel;tdc-t_{trigger} (ns)",1801,-0.5,1800.5,750,2000,3500);
  AddHist(mhMtdHitTrigTime);

  // ===== matched hits
  mhMtdNMatchHits = new TH1F("mhMtdNMatchHits","Number of matched MTD hits per event;N",100,0,100);
  AddHist(mhMtdNMatchHits);

  mhMtdMatchHitMap = new TH2F("hMtdMatchHitMap","MTD: channel vs backleg of matched hits;backleg;channel",30,0.5,30.5,60,-0.5,59.5);
  AddHist(mhMtdMatchHitMap);
 
  mhMtdMatchPhi = new TH2F("hMtdMatchPhi","MTD hit #varphi vs track #varphi for matched pairs;#varphi_{proj};#varphi_{hit}",90,0,2*pi,90,0,2*pi);
  AddHist(mhMtdMatchPhi);

  mhMtdMatchDzVsChan = new TH2F("hMtdMatchDzVsChan","MTD: #Deltaz distribution;channel;#Deltaz = z_{proj}-z_{hit} (cm)",1801,-0.5,1800.5,201,-201,201);
  AddHist(mhMtdMatchDzVsChan);

  mhMtdMatchDyVsChan = new TH2F("hMtdMatchDyVsChan","MTD: #Deltay distribution;channel;#Deltay = y_{proj}-y_{hit} (cm)",1801,-0.5,1800.5,101,-101,101);
  AddHist(mhMtdMatchDyVsChan);

  mhMtdMatchDtofVsChan = new TH2F("hMtdMatchDtofVsChan","MTD: #Deltatof distribution;channel;TOF_{measured}-TOF_{expected} (ns)",1801,-0.5,1800.5,200,-10,10);
  AddHist(mhMtdMatchDtofVsChan);

  mhMtdMatchLocalyVsChan = new TH2F("hMtdMatchLocalyVsChan","MTD: local y of matched tracks;channel;y (cm)",1801,-0.5,1800.5,100,-50.5,49.5);
  AddHist(mhMtdMatchLocalyVsChan);

  mhMtdMatchLocalzVsChan = new TH2F("hMtdMatchLocalzVsChan","MTD: local z of matched tracks;channel;z (cm)",1801,-0.5,1800.5,100,-50.5,49.5);
  AddHist(mhMtdMatchLocalzVsChan);

  mhMtdMatchDzVsPt = new TH2F("hMtdMatchDzVsPt","MTD: #Deltaz distribution;p_{T} (GeV/c);#Deltaz = z_{proj}-z_{hit} (cm)",100,0,20,201,-201,201);
  AddHist(mhMtdMatchDzVsPt);

  mhMtdMatchDyVsPt = new TH2F("hMtdMatchDyVsPt","MTD: #Deltay distribution;p_{T} (GeV/c);#Deltay = y_{proj}-y_{hit} (cm)",100,0,20,101,-101,101);
  AddHist(mhMtdMatchDyVsPt);

  mhMtdMatchDtofVsPt = new TH2F("hMtdMatchDtofVsPt","MTD: #Deltatof distribution;p_{T} (GeV/c);TOF_{measured}-TOF_{expected} (ns)",100,0,20,200,-10,10);
  AddHist(mhMtdMatchDtofVsPt);

  mhMtdMatchTrkPt = new TH1F("hMtdMatchTrkPt","MTD: p_{T} of matched global tracks;p_{T} (GeV/c)",100,0,20);
  AddHist(mhMtdMatchTrkPt);

  mhMtdMatchTrkPhiEta = new TH2F("hMtdMatchTrkPhiEta","MTD: #varphi vs #eta of matched global tracks;#eta;#varphi",12,-1.2,1.2,360,0,2*pi);
  AddHist(mhMtdMatchTrkPhiEta);

  mhMtdMatchTrkDedx = new TH2F("hMtdMatchTrkDedx","MTD: dE/dx of matched global tracks;p_{T} (GeV);dE/dx",100,0,20,200,0,20);
  AddHist(mhMtdMatchTrkDedx);

  // ====== projection
  mhTrkPt = new TH1F("hTrkPt","p_{T} of global tracks;p_{T} (GeV/c)",100,0,20);
  AddHist(mhTrkPt);

  mhTrkDca = new TH2F("hTrkDca","Dca vs p_{T} of global tracks;p_{T} (GeV/c);dca (cm)",100,0,20,100,0,50);
  AddHist(mhTrkDca);

  mhTrkPhiEta = new TH2F("hTrkPhiEta","#varphi vs #eta of global tracks at primary vertex;#eta;#varphi",50,-1,1,150,0,2*pi);
  AddHist(mhTrkPhiEta);

  mhMtdTrackProjMap = new TH2F("hMtdTrackProjMap","MTD: channel vs backleg of projected tracks;backleg;channel",30,0.5,30.5,60,-0.5,59.5);
  AddHist(mhMtdTrackProjMap);

  mhTrkProjPhiZAtMtd = new TH2F("hTrkProjPhiZAtMtd","#varphi vs z of global tracks at MTD radius;z;#varphi",50,-300,300,30,0,2*pi);
  AddHist(mhTrkProjPhiZAtMtd);

  mhTrkPhiVsMtdPhi = new TH2F("hTrkPhiVsMtdPhi","Projected track #varphi vs MTD hit #varphi;#varphi_{hit};#varphi_{proj}",90,0,2*pi,90,0,2*pi);
  AddHist(mhTrkPhiVsMtdPhi);

  mhTofMthTrkLocaly = new TH2F("hTofMthTrkLocaly","TOF match: local y vs tray;tray;local y (cm)",120,0.5,120.5,100,-5,5);
  AddHist(mhTofMthTrkLocaly);

  mhTofMthTrkLocalz = new TH2F("hTofMthTrkLocalz","TOF match: local z vs module;module;local z (cm)",64,0.5,64.5,100,-5,5);
  AddHist(mhTofMthTrkLocalz);

  //====== check calibration
  mhPrimDzVsChan = new TH2F("hPrimDzVsChan","MTD: #Deltaz vs channel of primary tracks;channel;#Deltaz = z_{proj}-z_{hit} (cm)",1801,-0.5,1800.5,201,-201,201);
  AddHist(mhPrimDzVsChan);

  mhPrimDyVsChan = new TH2F("hPrimDyVsChan","MTD: #Deltay vs channel of primary tracks;channel;#Deltay = y_{proj}-y_{hit} (cm)",1801,-0.5,1800.5,101,-101,101);
  AddHist(mhPrimDyVsChan);

  mhPrimDtofVsChan = new TH2F("hPrimDtofVsChan","MTD: #Deltatof vs channel of primary tracks;channel;TOF_{measured}-TOF_{expected} (ns)",1800,-0.5,1799.5,1000,-5,5);
  AddHist(mhPrimDtofVsChan);

  mhPrimMtdTofVsChan = new TH2F("hPrimMtdTofVsChan","MTD: MTD time vs channel of primary tracks;channel;TOF_{measured} (ns)",1800,-0.5,1799.5,500,-10,40);
  AddHist(mhPrimMtdTofVsChan);

  mhPrimExpTofVsChan = new TH2F("hPrimExpTofVsChan","MTD: TPC time vs channel of primary tracks;channel;TOF_{expected} (ns)",1800,-0.5,1799.5,500,-10,40);
  AddHist(mhPrimExpTofVsChan);
}

//_____________________________________________________________________________
void StMtdQAMaker::bookTree()
{
  if(!mOutTreeFileName.Length())
    {
      LOG_ERROR << "StMtdQAMaker:: no output file specified for trees." << endm;
      return;
    }

  fOutTreeFile = new TFile(mOutTreeFileName.Data(),"recreate");
  LOG_INFO << "StMtdQAMaker:: create the output to store the QA tree: " << mOutTreeFileName.Data() << endm;
  LOG_INFO << "StMtdQAMaker:: book the QA trees to be filled." << endm;

  mQATree = new TTree("mtdQAData","Mtd QA data");
  mQATree->SetAutoSave(100000); // 100 MB

  // event information
  mQATree->Branch("runId",             &mMtdData.runId,            "runId/I");
  mQATree->Branch("eventId",           &mMtdData.eventId,          "eventId/I");
  mQATree->Branch("nTrigger",          &mMtdData.nTrigger,         "nTrigger/I");
  mQATree->Branch("triggerId",         &mMtdData.triggerId,        "triggerId[nTrigger]/I");
  mQATree->Branch("vertexX",           &mMtdData.vertexX,          "vertexX/F");
  mQATree->Branch("vertexY",           &mMtdData.vertexY,          "vertexY/F");
  mQATree->Branch("vertexZ",           &mMtdData.vertexZ,          "vertexZ/F");
  mQATree->Branch("vpdVz",             &mMtdData.vpdVz,            "vpdVz/F");

  // VPD information
  mQATree->Branch("pre",               &mMtdData.pre,               "pre/B");
  mQATree->Branch("post",              &mMtdData.post,              "post/B");
  mQATree->Branch("prepost",           &mMtdData.prepost,           "prepost/B");
  mQATree->Branch("fasteastHi",        &mMtdData.fasteastHi,        "fasteastHi/s");
  mQATree->Branch("fastwestHi",        &mMtdData.fastwestHi,        "fastwestHi/s");
  mQATree->Branch("vpdHi",             &mMtdData.vpdHi,             "vpdHi[64]/s");
  mQATree->Branch("mixMtdTacSum",      &mMtdData.mixMtdTacSum,      "mixMtdTacSum[16]/s");
  mQATree->Branch("mtdQTadc",          &mMtdData.mtdQTadc,          "mtdQTadc[64]/s");
  mQATree->Branch("mtdQTtac",          &mMtdData.mtdQTtac,          "mtdQTtac[64]/s");

  // TOF information
  mQATree->Branch("tofStartTime",      &mMtdData.tofStartTime,      "tofStartTime/I");


  // Tracks
  mQATree->Branch("nGoodTrack",        &mMtdData.nGoodTrack,         "nGoodTrack/I");
  mQATree->Branch("trkPt",             &mMtdData.trkPt,              "trkPt[nGoodTrack]/D");
  mQATree->Branch("trkEta",            &mMtdData.trkEta,             "trkEta[nGoodTrack]/D");
  mQATree->Branch("trkPhi",            &mMtdData.trkPhi,             "trkPhi[nGoodTrack]/D");
  mQATree->Branch("trkNsigmaPi",       &mMtdData.trkNsigmaPi,        "trkNsigmaPi[nGoodTrack]/D");
  mQATree->Branch("isTrkTofMatched",   &mMtdData.isTrkTofMatched,    "isTrkTofMatched[nGoodTrack]/O");
  mQATree->Branch("trkMthTofTray",     &mMtdData.trkMthTofTray,      "trkMthTofTray[nGoodTrack]/I");
  mQATree->Branch("trkMthTofModule",   &mMtdData.trkMthTofModule,    "trkMthTofModule[nGoodTrack]/I");
  mQATree->Branch("trkMthTofCell",     &mMtdData.trkMthTofCell,      "trkMthTofCell[nGoodTrack]/I");
  mQATree->Branch("trkMthTofLocaly",   &mMtdData.trkMthTofLocaly,    "trkMthTofLocaly[nGoodTrack]/D");
  mQATree->Branch("trkMthTofLocalz",   &mMtdData.trkMthTofLocalz,    "trkMthTofLocalz[nGoodTrack]/D");
  mQATree->Branch("isTrkProjected",    &mMtdData.isTrkProjected,     "isTrkProjected[nGoodTrack]/O");
  mQATree->Branch("trkProjPhi",        &mMtdData.trkProjPhi,         "trkProjPhi[nGoodTrack]/D");
  mQATree->Branch("trkProjZ",          &mMtdData.trkProjZ,           "trkProjZ[nGoodTrack]/D");
  mQATree->Branch("trkProjBackleg",    &mMtdData.trkProjBackleg,     "trkProjBackleg[nGoodTrack]/B");
  mQATree->Branch("trkProjModule",     &mMtdData.trkProjModule,      "trkProjModule[nGoodTrack]/B");
  mQATree->Branch("trkProjChannel",    &mMtdData.trkProjChannel,     "trkProjChannel[nGoodTrack]/B");
  mQATree->Branch("isTrkMtdMatched",   &mMtdData.isTrkMtdMatched,    "isTrkMtdMatched[nGoodTrack]/O");
  mQATree->Branch("isGoodMthMtdHit",   &mMtdData.isGoodMthMtdHit,    "isGoodMthMtdHit[nGoodTrack]/O");
  mQATree->Branch("trkMthBackleg",     &mMtdData.trkMthBackleg,      "trkMthBackleg[nGoodTrack]/B");
  mQATree->Branch("trkMthModule",      &mMtdData.trkMthModule,       "trkMthModule[nGoodTrack]/B");
  mQATree->Branch("trkMthChannel",     &mMtdData.trkMthChannel,      "trkMthChannel[nGoodTrack]/B");

  // MTD information
  mQATree->Branch("mtdTriggerTime",    &mMtdData.mtdTriggerTime,    "mtdTriggerTime[2]/D");
  //raw hit
  mQATree->Branch("mMtdRawHits",       &mMtdData.nMtdRawHits,       "nMtdRawHits/I");
  mQATree->Branch("mtdRawHitFlag",     &mMtdData.mtdRawHitFlag,     "mtdRawHitFlag[nMtdRawHits]/B");
  mQATree->Branch("mtdRawHitBackleg",  &mMtdData.mtdRawHitBackleg,  "mtdRawHitBackleg[nMtdRawHits]/B");
  mQATree->Branch("mtdRawHitModule",   &mMtdData.mtdRawHitModule,   "mtdRawHitModule[nMtdRawHits]/B");
  mQATree->Branch("mtdRawHitChan",     &mMtdData.mtdRawHitChan,     "mtdRawHitChan[nMtdRawHits]/B");
  mQATree->Branch("mtdRawHitTdc",      &mMtdData.mtdRawHitTdc,      "mtdRawHitTdc[nMtdRawHits]/D");
  mQATree->Branch("mtdRawHitTimdDiff", &mMtdData.mtdRawHitTimdDiff, "mtdRawHitTimdDiff[nMtdRawHits]/D");
  // hit
  mQATree->Branch("nMtdHits",          &mMtdData.nMtdHits,          "nMtdHits/I");
  mQATree->Branch("isGoodMtdHit",      &mMtdData.isGoodMtdHit,      "isGoodMtdHit[nMtdHits]/O");
  mQATree->Branch("mtdHitBackleg",     &mMtdData.mtdHitBackleg,     "mtdHitBackleg[nMtdHits]/B");
  mQATree->Branch("mtdHitModule",      &mMtdData.mtdHitModule,      "mtdHitModule[nMtdHits]/B");
  mQATree->Branch("mtdHitChan",        &mMtdData.mtdHitChan,        "mtdHitChan[nMtdHits]/B");
  mQATree->Branch("mtdHitLeTimeWest",  &mMtdData.mtdHitLeTimeWest,  "mtdHitLeTimeWest[nMtdHits]/D");
  mQATree->Branch("mtdHitLeTimeEast",  &mMtdData.mtdHitLeTimeEast,  "mtdHitLeTimeEast[nMtdHits]/D");
  mQATree->Branch("mtdHitTotWest",     &mMtdData.mtdHitTotWest,     "mtdHitTotWest[nMtdHits]/D");
  mQATree->Branch("mtdHitTotEast",     &mMtdData.mtdHitTotEast,     "mtdHitTotEast[nMtdHits]/D");
  mQATree->Branch("mtdHitTrigTime",    &mMtdData.mtdHitTrigTime,    "mtdHitTrigTime[nMtdHits]/D");
  mQATree->Branch("mtdHitPhi",         &mMtdData.mtdHitPhi,         "mtdHitPhi[nMtdHits]/D");
  mQATree->Branch("mtdHitZ",           &mMtdData.mtdHitZ,           "mtdHitZ[nMtdHits]/D");
  // matching
  mQATree->Branch("isMatched",         &mMtdData.isMatched,         "isMatched[nMtdHits]/O");
  mQATree->Branch("isMatchedPrim",     &mMtdData.isMatchedPrim,     "isMatchedPrim[nMtdHits]/O");
  mQATree->Branch("nMatchMtdHits",     &mMtdData.nMatchMtdHits,     "nMatchMtdHits/I");
  mQATree->Branch("mtdMatchTrkPathLength",&mMtdData.mtdMatchTrkPathLength,"mtdMatchTrkPathLength[nMtdHits]/D");
  mQATree->Branch("mtdMatchTrkExpTof", &mMtdData.mtdMatchTrkExpTof, "mtdMatchTrkExpTof[nMtdHits]/D");
  mQATree->Branch("mtdMatchTrkTof",    &mMtdData.mtdMatchTrkTof,    "mtdMatchTrkTof[nMtdHits]/D");
  mQATree->Branch("mtdMatchTrkLocaly", &mMtdData.mtdMatchTrkLocaly, "mtdMatchTrkLocaly[nMtdHits]/D");
  mQATree->Branch("mtdMatchTrkLocalz", &mMtdData.mtdMatchTrkLocalz, "mtdMatchTrkLocalz[nMtdHits]/D");
  mQATree->Branch("mtdMatchTrkDeltay", &mMtdData.mtdMatchTrkDeltay, "mtdMatchTrkDeltay[nMtdHits]/D");
  mQATree->Branch("mtdMatchTrkDeltaz", &mMtdData.mtdMatchTrkDeltaz, "mtdMatchTrkDeltaz[nMtdHits]/D");
  mQATree->Branch("mtdMatchTrkProjPhi",&mMtdData.mtdMatchTrkProjPhi,"mtdMatchTrkProjPhi[nMtdHits]/D");
  mQATree->Branch("mtdMatchTrkProjZ",  &mMtdData.mtdMatchTrkProjZ,  "mtdMatchTrkProjZ[nMtdHits]/D");
  mQATree->Branch("mtdMatchTrkPt",     &mMtdData.mtdMatchTrkPt,     "mtdMatchTrkPt[nMtdHits]/D");
  mQATree->Branch("mtdMatchTrkPhi",    &mMtdData.mtdMatchTrkPhi,    "mtdMatchTrkPhi[nMtdHits]/D");
  mQATree->Branch("mtdMatchTrkEta",    &mMtdData.mtdMatchTrkEta,    "mtdMatchTrkEta[nMtdHits]/D");
  mQATree->Branch("mtdMatchTrkDedx",   &mMtdData.mtdMatchTrkDedx,   "mtdMatchTrkDedx[nMtdHits]/D");
  mQATree->Branch("mtdMatchTrkNsigmaPi",&mMtdData.mtdMatchTrkNsigmaPi,"mtdMatchTrkNsigmaPi[nMtdHits]/D");

  return;
}

//_____________________________________________________________________________
void StMtdQAMaker::printConfig()
{
  const char *decision[2] = {"no","yes"};
  const char *runtype[2] = {"Physics","Cosmic"};
  printf("=== Configuration for StMtdQAMaker ===\n");
  printf("Data type: %s\n",runtype[mIsCosmic]);
  printf("Force to run muDst: %s\n",decision[mMuDstIn]);
  printf("Fill the QA tree: %s\n",decision[mFillTree]);
  printf("Maximum vertex z: %1.0f\n",mMaxVtxZ);
  printf("Maximum vz diff: %1.0f\n",mMaxVtxDz);
  printf("Track pt  range: [%1.2f, %1.2f]\n",mMinTrkPt,mMaxTrkPt);
  printf("Track phi range: [%1.2f, %1.2f]\n",mMinTrkPhi,mMaxTrkPhi);
  printf("Track eta range: [%1.2f, %1.2f]\n",mMinTrkEta,mMaxTrkEta);
  printf("Minimum number of fit hits: %d\n",mMinNHitsFit);
  printf("Minimum number of dedx hits: %d\n",mMinNHitsDedx);
  printf("Minimum fraction of fit hits: %4.2f\n",mMinFitHitsFraction);
  printf("Maximum dca: %1.2f\n",mMaxDca);
  printf("NsigmaPi range: [%1.2f, %1.2f]\n",mMinNsigmaPi,mMaxNsigmaPi);
  printf("=======================================\n");
}

//_____________________________________________________________________________
Bool_t StMtdQAMaker::propagateHelixToMtd(StPhysicalHelixD helix, Double_t &projPhi, Double_t &projZ) const
{
  // helix extrapolation
  // no energy loss

  projPhi = -999; projZ = -999;
  pairD sMtd = helix.pathLength(gMtdMinRadius);
  if(sMtd.first<=0 && sMtd.second<=0) return kFALSE;
  Double_t rMtd = (sMtd.first<0 || sMtd.second<0) ? max(sMtd.first,sMtd.second) : min(sMtd.first,sMtd.second);
  StThreeVector<double> pMtd = helix.at(rMtd);
  projPhi = rotatePhi(pMtd.phi());
  projZ   = pMtd.z();
  return kTRUE;
}

//_____________________________________________________________________________
Int_t StMtdQAMaker::getMtdHitTHUB(const Int_t backleg) const
{
  if(backleg>=1 && backleg<=15)        return 2;
  else if (backleg>=16 && backleg<=30) return 1;
  else return -1;
}


//_____________________________________________________________________________
Double_t StMtdQAMaker::getMtdHitGlobalZ(StMuMtdHit *hit) const
{
  Int_t backleg = hit->backleg();
  if(backleg<1 || backleg>30)
    {
      LOG_WARN << "Wrong backleg id: " << backleg << endm;
      return -999;
    }
  return getMtdHitGlobalZ(hit->leadingEdgeTime().first, hit->leadingEdgeTime().second, hit->module());
}

//_____________________________________________________________________________
Double_t StMtdQAMaker::getMtdHitGlobalZ(StMtdHit *hit) const
{
  Int_t backleg = hit->backleg();
  if(backleg<1 || backleg>30)
    {
      LOG_WARN << "Wrong backleg id: " << backleg << endm;
      return -999;
    }
  return getMtdHitGlobalZ(hit->leadingEdgeTime().first, hit->leadingEdgeTime().second, hit->module());
}

//_____________________________________________________________________________
Double_t StMtdQAMaker::getMtdHitGlobalZ(Double_t leadingWestTime, Double_t leadingEastTime, Int_t module) const
{
  Double_t z = (module-3)*gMtdCellLength - (leadingWestTime-leadingEastTime)/2./gMtdCellDriftV*1e3;
  return z;
}

//_____________________________________________________________________________
Double_t StMtdQAMaker::getMtdHitGlobalPhi(StMuMtdHit *hit) const
{
  Int_t backleg = hit->backleg();
  if(backleg<1 || backleg>30)
    {
      LOG_WARN << "Wrong backleg id: " << backleg << endm;
      return -999;
    }
  return getMtdHitGlobalPhi(backleg, hit->module(), hit->cell());
}

//_____________________________________________________________________________
Double_t StMtdQAMaker::getMtdHitGlobalPhi(StMtdHit *hit) const
{
  Int_t backleg = hit->backleg();
  if(backleg<1 || backleg>30)
    {
      LOG_WARN << "Wrong backleg id: " << backleg << endm;
      return -999;
    }

  return getMtdHitGlobalPhi(backleg, hit->module(), hit->cell());
}

//_____________________________________________________________________________
Double_t StMtdQAMaker::getMtdHitGlobalPhi(Int_t backleg, Int_t module, Int_t channel) const
{
  Double_t backlegPhiCen = gMtdFirstBacklegPhiCenter + (backleg-1) * (gMtdBacklegPhiWidth+gMtdBacklegPhiGap);
  if(backlegPhiCen>2*pi) backlegPhiCen -= 2*pi;
  Double_t stripPhiCen = 0;
  if(module>0 && module<4)
    {
      stripPhiCen = backlegPhiCen - (gMtdNChannels/4.-0.5-channel)*(gMtdCellWidth+gMtdCellGap)/gMtdMinRadius;
    }
  else
    {
      stripPhiCen = backlegPhiCen + (gMtdNChannels/4.-0.5-channel)*(gMtdCellWidth+gMtdCellGap)/gMtdMinRadius;
    }
  return rotatePhi(stripPhiCen);
}

//_____________________________________________________________________________
Int_t StMtdQAMaker::getMtdBackleg(const Double_t projPhi) const
{
  Double_t phi = rotatePhi(projPhi);
  Int_t backleg = (Int_t)(phi/(gMtdBacklegPhiWidth+gMtdBacklegPhiGap));
  backleg += 24;
  if(backleg>30) backleg -= 30;
  if(backleg>=1 && backleg<=30)
    return backleg;
  else
    return -1;
}

//_____________________________________________________________________________
Int_t StMtdQAMaker::getMtdModule(const Double_t projZ) const
{
  Int_t module = -1;
  Double_t temp = (projZ+2.5*gMtdCellLength)/gMtdCellLength;
  if(temp>0) module = (Int_t)temp + 1;
  return module;
}

//_____________________________________________________________________________
Int_t StMtdQAMaker::getMtdCell(const Double_t projPhi, const Double_t projZ) const
{
  Int_t backleg = getMtdBackleg(projPhi);
  Int_t module  = getMtdModule(projZ);
  return getMtdCell(projPhi,backleg,module);
}

//_____________________________________________________________________________
Int_t StMtdQAMaker::getMtdCell(const Double_t projPhi, const Int_t backleg, const Int_t module) const
{
  // module: 1-5
  Double_t lowEdge = gMtdFirstBacklegPhiCenter + (backleg-1)*(gMtdBacklegPhiWidth+gMtdBacklegPhiGap) - (gMtdNChannels/4.)*(gMtdCellWidth+gMtdCellGap)/gMtdMinRadius; //approximation
  lowEdge = rotatePhi(lowEdge);
  Double_t cellPhi = projPhi - lowEdge;
  cellPhi = rotatePhi(cellPhi);
  Int_t cell = (Int_t) ( cellPhi/((gMtdCellWidth+gMtdCellGap)/gMtdMinRadius));
  if(module>3) cell = 11 - cell;
  if(cell>=0 && cell<=11)
    return cell;
  else
    return -1;
}

//_____________________________________________________________________________
void StMtdQAMaker::getMtdPosFromProj(const Double_t projPhi, const Double_t projZ, Int_t &backleg, Int_t &module, Int_t&cell) const
{
  backleg = getMtdBackleg(projPhi);
  module  = getMtdModule(projZ);
  cell    = getMtdCell(projPhi,backleg,module);
}

//_____________________________________________________________________________
Bool_t StMtdQAMaker::isValidTrack(StTrack *track, StVertex *vtx) const 
{
  StThreeVectorF mom = track->geometry()->momentum();
  Float_t pt = mom.perp();
  Float_t eta = mom.pseudoRapidity();
  Float_t phi = mom.phi();
  if(phi<0) phi += 2*pi;

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

  static StPionPlus* Pion = StPionPlus::instance();
  Double_t nSigmaPi = pidAlgorithm.numberOfSigma(Pion);
  if(nSigmaPi<mMinNsigmaPi || nSigmaPi>mMaxNsigmaPi) return kFALSE;

  if(!mIsCosmic)
    {
      StGlobalTrack *globalTrack = dynamic_cast<StGlobalTrack*>(track);
      if(!globalTrack) return kFALSE;
      THelixTrack    thelix      = globalTrack->dcaGeometry()->thelix();
      const Double_t *pos        = thelix.Pos();
      StThreeVectorF dcaGlobal   = StThreeVectorF(pos[0],pos[1],pos[2]) - vtx->position();
      if(dcaGlobal.mag()>mMaxDca)  return kFALSE;
    }

  return kTRUE;
}


//_____________________________________________________________________________
Bool_t StMtdQAMaker::isValidTrack(StMuTrack *track) const 
{
  StThreeVectorF mom = track->momentum();
  Float_t pt = mom.perp();
  Float_t eta = mom.pseudoRapidity();
  Float_t phi = mom.phi();
  if(phi<0) phi += 2*pi;
  Double_t nSigmaPi = track->nSigmaPion();

  if(pt < mMinTrkPt   || pt > mMaxTrkPt)             return kFALSE;
  if(eta < mMinTrkEta || eta > mMaxTrkEta)           return kFALSE;
  if(phi < mMinTrkPhi || phi > mMaxTrkPhi)           return kFALSE;
  if(track->nHitsFit(kTpcId)<mMinNHitsFit)           return kFALSE;
  if(track->nHitsDedx()<mMinNHitsDedx)               return kFALSE;
  if(!mIsCosmic && track->dca().mag()>mMaxDca)       return kFALSE;
  if(nSigmaPi<mMinNsigmaPi || nSigmaPi>mMaxNsigmaPi) return kFALSE;
  if(track->nHitsFit(kTpcId)/(1.0*track->nHitsPoss(kTpcId))<mMinFitHitsFraction) return kFALSE;
  return kTRUE;
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
//// $Id: StMtdQAMaker.cxx,v 1.16 2017/03/10 20:16:42 marr Exp $
//// $Log: StMtdQAMaker.cxx,v $
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
