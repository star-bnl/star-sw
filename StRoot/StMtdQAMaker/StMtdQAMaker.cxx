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
  mIsCosmic(kFALSE), mStEvent(0), mMuDst(0), mVertexMode(0), mVertexIndex(-1), mRunId(-1), mRunYear(-1), mCollisionSystem("pp"), mStartRun(0), mEndRun(999999),
  mTriggerData(0),
  mMuDstIn(kFALSE), mPrintMemory(kFALSE), mPrintCpu(kFALSE), mPrintConfig(kFALSE),
  mTriggerIDs(0),
  mApplyQTTacOffset(kFALSE), mFileQTTacOffset(""),
  mMaxVtxZ(100.), mMaxVtxDz(5.),
  mMinTrkPt(0.2), mMaxTrkPt(1e4), mMinTrkPhi(0.), mMaxTrkPhi(2*pi), mMinTrkEta(-1), mMaxTrkEta(1),
  mMinNHitsFit(15), mMinNHitsDedx(10), mMinFitHitsFraction(0), mMaxDca(3.), mMinNsigmaPi(-10), mMaxNsigmaPi(10), mMatchToTof(false),
  mMtd_qt_tac_min(80), mMtd_qt_tac_max(4096), mMtd_qt_tac_diff_range_abs(600),
  mHistoInit(kFALSE), mFillTree(kFALSE), fOutTreeFile(0), mOutTreeFileName(""), mQATree(NULL)
{
  // default constructor
  mTrigTime[0] = -1;
  mTrigTime[1] = -1;

  for(int im=0; im<kNQTboard; im++)
    {
      for(int j=0; j<16; j++)
	mQTTacOffset[im][j] = 0;
    }

  memset(&mMtdData, 0, sizeof(mMtdData));

  mhEventTrig              = NULL;
  mhEventCuts              = NULL;
  mhRunId                  = NULL;
  mhRefMult                = NULL;
  mhgRefMult               = NULL;
  mhVertexXY               = NULL;
  mhVertexXZ               = NULL;
  mhVertexYZ               = NULL;
  mhVertexZ                = NULL;
  mhVtxZvsVpdVzDefault     = NULL;
  mhVtxZDiffDefault        = NULL;
  mhVtxZvsVpdVzClosest     = NULL;
  mhVtxZDiffClosest        = NULL;
  mhVtxClosestIndex        = NULL;
  mhTofStartTime           = NULL;
  mhVpdQTadc               = NULL;
  mhVpdQTtac               = NULL;

  mhNTrk                   = NULL;
  mhTrkPt                  = NULL;
  mhTrkDcaVsPt             = NULL;
  mhTrkPhiVsPt             = NULL;
  mhTrkEtaVsPt             = NULL;
  mhTrkPhiEta              = NULL;
  mhTrkNHitsFitVsPt        = NULL;
  mhTrkNHitsDedxVsPt       = NULL;
  mhTrkDedxVsPt            = NULL;
  mhTrkNsigmaPiVsPt        = NULL;
  mhTrkNsigmaPiVsPhi       = NULL;
  mhTrkNsigmaPiVsEta       = NULL;
  mhTofMthTrkLocaly        = NULL;
  mhTofMthTrkLocalz        = NULL;
  mhMtdTrackProjMap        = NULL;

  mhMtdQTAdcAll            = NULL;
  mhMtdQTAdcMth            = NULL;
  mhMtdQTAdcMthTof         = NULL;
  mhMtdQTAdcMuon           = NULL;
  mhMtdQTTacAll            = NULL;
  mhMtdQTAdcMth            = NULL;
  mhMtdQTAdcMthTof         = NULL;
  mhMtdQTAdcMuon           = NULL;
  mhMtdQTAdcVsTacAll       = NULL;
  mhMtdQTJ2J3Diff          = NULL;
  mhMtdVpdTacDiffMT001     = NULL;
  mhMtdVpdTacDiffMT001Mth  = NULL;
  mhMtdVpdTacDiffMT001MthTof = NULL;
  mhMtdVpdTacDiffMT001Muon = NULL;
  mhMtdVpdTacDiffMT101     = NULL;
  mhMtdVpdTacDiffMT101Mth  = NULL;
  mhMtdVpdTacDiffMT101MthTof= NULL;
  mhMtdVpdTacDiffMT101Muon = NULL;
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
  mhMtdHitTrigTimeTrkMthTof= NULL;
  mhMtdHitTrigTimeMuon     = NULL;
  mhMtdHitTrigTimeGoodQT   = NULL;
  mhMtdHitTrigTimeTrig     = NULL;
  mhMtdHitTrigTimeVsQtAdc[0]= NULL;
  mhMtdHitTrigTimeVsQtAdc[1]= NULL;
  mhMtdHitTrigTimeVsQtTac[0]= NULL;
  mhMtdHitTrigTimeVsQtTac[1]= NULL;

  mhMtdNMatchHits          = NULL;
  mhMtdMatchHitMap         = NULL;
  mhMtdMatchTrkPt          = NULL;
  mhMtdMatchTrkPhiEta      = NULL;
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

  mhNQtSignal              = NULL;
  mhNMT101Signal           = NULL;
  mhNTF201Signal           = NULL;
  mhMtdTrigNHits           = NULL;
  mhMtdTrigHitMap          = NULL;
  mhMtdTrigMthNHits        = NULL;
  mhMtdTrigMthHitMap       = NULL;
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
  mRunYear = runNumber / 1e6 + 1999;
  if(!mHistoInit) 
    {
      mHistoInit = true;
      initHistos();
      readQTTacOffsetFile();
    }

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
  return kStOK;
}

//_____________________________________________________________________________
Int_t StMtdQAMaker::readQTTacOffsetFile()
{
  // Read in QT tac offset if needed
  if(mApplyQTTacOffset)
    {
      if(mFileQTTacOffset.Length()==0)
	{
	  LOG_ERROR << "QT tac offset file not set but required!" << endm;
	  return kStWarn;
	}
      ifstream infile;
      infile.open(mFileQTTacOffset.Data());
      char tmp[256];
      int board, chan, pedestal, gain;
      for(int im=0; im<kNQTboard; im++)
	{
	  if(mRunYear!=2016 && im>3) continue;

	  infile.getline(tmp,256);
	  for(int j=0; j<16; j++)
	    {
	      infile >> board >> chan >> pedestal >> gain;
	      mQTTacOffset[im][j] = pedestal;
	      LOG_DEBUG << im << "  " << j << "  " << mQTTacOffset[im][j] << endm;
	    }
	  infile.getline(tmp,256);
	}
    }
  return kStOK;
}


//_____________________________________________________________________________
Int_t StMtdQAMaker::initHistos()
{
  if(mRunYear<=2014)  mMtd_qt_tac_min = 100;
  else if(mRunYear>=2017) mMtd_qt_tac_min = 150;
  if(mRunYear==2015)  mMtd_qt_tac_diff_range_abs = 1023;
  mStartRun = (mRunYear-1999)*1e6;
  mEndRun   = mStartRun + 200000;

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
  //cout << "New Event !" << endl;
  // Run statistics
  mRunId   = mMuDst->event()->runId();
  mhRunId->Fill(mRunId + 0.5);
  
  int mass_east = mMuDst->event()->runInfo().beamMassNumber(east);
  int mass_west = mMuDst->event()->runInfo().beamMassNumber(west);
  if(mass_east == mass_west)
    {
      if(mass_east == 1)
	mCollisionSystem = "pp";
      else
	mCollisionSystem = "AA";
    }
  else
    mCollisionSystem = "pA";

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

  //========== Select vertex ==========
  mVertexIndex = -1;
  Int_t nPrim = mMuDst->numberOfPrimaryVertices();
  StMuPrimaryVertex* priVertex = NULL;
  mMtdData.vertexX = -999.;
  mMtdData.vertexY = -999.;
  mMtdData.vertexZ = -999.;
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

      Int_t index = -1;
      if(tofHeader)
	{
	  // constrain vertex with VPD
	  Double_t min_dz = 999;
	  for(Int_t i=0; i<nPrim; i++)
	    {
	      StMuPrimaryVertex *vertex = mMuDst->primaryVertex(i);
	      double ranking = vertex->ranking();
	      if(mCollisionSystem=="pp" && ranking<0) continue;
	      Double_t dz = fabs(vertex->position().z()-mMtdData.vpdVz);
	      if(dz<min_dz)
		{
		  min_dz = dz;
		  index = i;
		}
	    }
	  mhVtxClosestIndex->Fill(index);
	  
	  double default_z = mMuDst->primaryVertex(0)->position().z();
	  mhVtxZvsVpdVzDefault->Fill(default_z, mMtdData.vpdVz);
	  mhVtxZDiffDefault->Fill(default_z - mMtdData.vpdVz);
	  if(index>-1)
	    {
	      double cloest_z = mMuDst->primaryVertex(index)->position().z();
	      mhVtxZvsVpdVzClosest->Fill(cloest_z, mMtdData.vpdVz);
	      mhVtxZDiffClosest->Fill(cloest_z - mMtdData.vpdVz);
	    }
	}
      
      if(mVertexMode==0) mVertexIndex = 0;
      else if(mVertexMode==1) mVertexIndex = index;
      else
	{
	  LOG_WARN << "No vertex mode is set. Use default vertex!" << endm;
	  mVertexIndex = 0;
	}

      if(mVertexIndex<0) return kStWarn;
      priVertex = mMuDst->primaryVertex(mVertexIndex);
      if(!priVertex) return kStWarn;
      StThreeVectorF verPos = priVertex->position();
      mMtdData.vertexX = verPos.x();
      mMtdData.vertexY = verPos.y();
      mMtdData.vertexZ = verPos.z();
      if(TMath::Abs(mMtdData.vertexZ)>mMaxVtxZ) return kStWarn;
      if(fabs(mMtdData.vpdVz-mMtdData.vertexZ)>mMaxVtxDz) return kStWarn;
    }
  mhEventTrig->Fill(2.5);
  //====================================

  mMtdData.runId   = mRunId;
  mMtdData.eventId = mMuDst->event()->eventId();
  mMtdData.refMult = mMuDst->event()->refMult(mVertexIndex);
  mMtdData.gRefMult = mMuDst->event()->grefmult(mVertexIndex);

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
  if(mTriggerData) processTriggerData(mTriggerData);

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
  Int_t goodTrack = 0;
  Double_t projPhi = -999, projZ = -999;
  Int_t backleg = -1, module = -1, cell = -1;
  map<Short_t, UShort_t> primaryIndex;
  Int_t nPrimTrks = mMuDst->numberOfPrimaryTracks();
  for(Int_t i=0; i<nPrimTrks; i++)
    {
      StMuTrack* pTrack = mMuDst->primaryTracks(i);
      if(!pTrack) continue;
      primaryIndex[pTrack->id()] = i;
      if(!isValidTrack(pTrack)) continue;
      mMtdData.trkPt[goodTrack]           = pTrack->pt();
      mMtdData.trkEta[goodTrack]          = pTrack->eta();
      mMtdData.trkPhi[goodTrack]          = rotatePhi(pTrack->phi());
      mMtdData.trkDca[goodTrack]          = pTrack->dcaGlobal().mag();
      mMtdData.trkNHitsFit[goodTrack]     = pTrack->nHitsFit(kTpcId);
      mMtdData.trkNHitsDedx[goodTrack]    = pTrack->nHitsDedx();
      mMtdData.trkDedx[goodTrack]         = pTrack->dEdx() * 1e6;
      mMtdData.trkNsigmaPi[goodTrack]     = pTrack->nSigmaPion();

      // TOF matching
      mMtdData.isTrkTofMatched[goodTrack]  = kFALSE;
      const StMuBTofHit *tofHit = pTrack->tofHit();
      if(tofHit) 
	{
	  const StMuBTofPidTraits &tofPid = pTrack->btofPidTraits();
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
      StPhysicalHelixD gHelix = pTrack->outerHelix();
      if(propagateHelixToMtd(gHelix, projPhi, projZ))
	{
	  getMtdPosFromProj(projPhi, projZ, backleg, module, cell);
	  mMtdData.isTrkProjected[goodTrack] = kTRUE;
	  mMtdData.trkProjPhi[goodTrack]     = projPhi;
	  mMtdData.trkProjZ[goodTrack]       = projZ;
	  mMtdData.trkProjBackleg[goodTrack] = backleg;
	  mMtdData.trkProjModule[goodTrack]  = module;
	  mMtdData.trkProjChannel[goodTrack] = cell;
	  Int_t iMtd = pTrack->index2MtdHit();
	  backleg = -1, module = -1, cell = -1;
	  if(iMtd>-1)
	    {
	      mMtdData.isTrkMtdMatched[goodTrack] = kTRUE;
	      StMuMtdHit *hit = mMuDst->mtdHit(iMtd);
	      if(hit)
		{
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
      mMtdData.mtdHitTrigTime[i]   = tDiff;
      mMtdData.mtdHitPhi[i]        = getMtdHitGlobalPhi(hit);
      mMtdData.mtdHitZ[i]          = getMtdHitGlobalZ(hit);

      // check if the hit fires the MTD trigger
      mMtdData.isMtdTrig[i] = kFALSE;
      int qt = mModuleToQT[backleg-1][hit->module()-1];
      int qt_pos = mModuleToQTPos[backleg-1][hit->module()-1];
      if(qt_pos==mTrigQTpos[qt-1][0] || qt_pos==mTrigQTpos[qt-1][1])
	mMtdData.isMtdTrig[i] = kTRUE;

      // check if the hit matches to a TPC track
      mMtdData.isMatched[i] = kFALSE;
      Short_t trackId = hit->associatedTrackKey();
      if(trackId<1) continue;
      Int_t index = (primaryIndex.find(trackId)!=primaryIndex.end()) ? primaryIndex.find(trackId)->second : -1;
      if(index<0) continue;
      StMuTrack *pTrack = mMuDst->primaryTracks(index);
      if(!pTrack || !isValidTrack(pTrack) || pTrack->vertexIndex()!=mVertexIndex) continue;
      mMtdData.isMatched[i] = kTRUE;

      StThreeVectorF trkMom = pTrack->momentum();
      const StMuMtdPidTraits mtdPid = pTrack->mtdPidTraits();
      StThreeVectorF gPos = mtdPid.position();
      mMtdData.mtdMatchTrkPathLength[i] = mtdPid.pathLength();
      mMtdData.mtdMatchTrkTof[i]        = mtdPid.timeOfFlight();
      mMtdData.mtdMatchTrkExpTof[i]     = mtdPid.expTimeOfFlight();
      mMtdData.mtdMatchTrkLocaly[i]     = mtdPid.yLocal();
      mMtdData.mtdMatchTrkLocalz[i]     = mtdPid.zLocal();
      mMtdData.mtdMatchTrkDeltay[i]     = mtdPid.deltaY();
      mMtdData.mtdMatchTrkDeltaz[i]     = mtdPid.deltaZ();
      mMtdData.mtdMatchTrkProjPhi[i]    = rotatePhi(gPos.phi());
      mMtdData.mtdMatchTrkProjZ[i]      = gPos.z();
      mMtdData.mtdMatchTrkPt[i]         = trkMom.perp();
      mMtdData.mtdMatchTrkDca[i]        = pTrack->dcaGlobal().mag();
      mMtdData.mtdMatchTrCharge[i]      = pTrack->charge();
      mMtdData.mtdMatchTrkEta[i]        = trkMom.pseudoRapidity();
      mMtdData.mtdMatchTrkPhi[i]        = rotatePhi(trkMom.phi());
      mMtdData.mtdMatchTrkNsigmaPi[i]   = pTrack->nSigmaPion();
      mMtdData.mtdMatchTrkTofHit[i]     = pTrack->tofHit() ? true: false;
      nMatchMtdHit++;
      LOG_DEBUG << "MTD hit " << i << ", and is matched to track " << index << endm; 
    }
  mMtdData.nMtdHits = nMtdHits;
  mMtdData.nMatchMtdHits = nMatchMtdHit;


  return kStOK;
}

//_____________________________________________________________________________
void StMtdQAMaker::processTriggerData(StTriggerData *trigData)
{
  if(!trigData) return;
  Int_t pre = trigData->numberOfPreXing();
  Int_t post = trigData->numberOfPostXing();
  Int_t prepost = pre + post + 1;
  mMtdData.pre = pre;
  mMtdData.post = post;
  mMtdData.prepost = prepost;

  // VPD tac information
  const Int_t ip = 0;
  mMtdData.vpdTacSum = trigData->vpdEarliestTDCHighThr(east,ip)+trigData->vpdEarliestTDCHighThr(west,ip);

  for(Int_t i=0; i<kMaxVpdChan/4; i++)
    {
      mMtdData.vpdHi[i/8*8+i] = trigData->vpdADCHighThr(east,i+1,ip);
      mMtdData.vpdHi[i/8*8+8+i] = trigData->vpdTDCHighThr(east,i+1,ip);
      
      mMtdData.vpdHi[i/8*8+32+i] = trigData->vpdADCHighThr(west,i+1,ip);
      mMtdData.vpdHi[i/8*8+40+i] = trigData->vpdTDCHighThr(west,i+1,ip);
    }

  // MTD QT information
  for(Int_t i=0; i<kMaxMtdQTchan; i++)
    {
      Int_t type = (i/4)%2;
      if(mRunYear<=2015)
	{
	  if(type==1)
	    {
	      mMtdData.mtdQTtac[0][i-i/4*2-2] = trigData->mtdAtAddress(i,0);
	      mMtdData.mtdQTtac[1][i-i/4*2-2] = trigData->mtdgemAtAddress(i,0);
	      mMtdData.mtdQTtac[2][i-i/4*2-2] = trigData->mtd3AtAddress(i,0);
	      mMtdData.mtdQTtac[3][i-i/4*2-2] = trigData->mtd4AtAddress(i,0);
	    }
	  else
	    {
	      mMtdData.mtdQTadc[0][i-i/4*2] = trigData->mtdAtAddress(i,0);
	      mMtdData.mtdQTadc[1][i-i/4*2] = trigData->mtdgemAtAddress(i,0);
	      mMtdData.mtdQTadc[2][i-i/4*2] = trigData->mtd3AtAddress(i,0);
	      mMtdData.mtdQTadc[3][i-i/4*2] = trigData->mtd4AtAddress(i,0);
	    }
	}
      else
	{
	  for(int im=0; im<kNQTboard; im++)
	    {
	      if(mRunYear!=2016 && im>=4) continue;
	      if(type==0) mMtdData.mtdQTadc[im][i-i/4*2]   = trigData->mtdQtAtCh(im+1,i,0);
	      else        mMtdData.mtdQTtac[im][i-i/4*2-2] = trigData->mtdQtAtCh(im+1,i,0);
	    }
	}

    }

  UShort_t mxq_mtdtacsum[kNQTboard][2];
  for(int im=0; im<kNQTboard; im++)
    {
      for(int j=0; j<kMaxMtdQTchan/4; j++)
	{
	  mMtdData.mtdQTtacSum[im][j] = 0;
	}
      for(int j=0; j<2; j++)
	{
	  mMtdData.mtdQThigh2Pos[im][j] = -1;
	  mxq_mtdtacsum[im][j] = 0;
	}
    }
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

	      // TAC offset
	      if(mApplyQTTacOffset)
		{
		  j[k] -= mQTTacOffset[im][i*2+k];
		  if(j[k]<0) j[k] = 0;
		}

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

	  if(j[0]<mMtd_qt_tac_min || j[0]>mMtd_qt_tac_max || 
	     j[1]<mMtd_qt_tac_min || j[1]>mMtd_qt_tac_max ||
	     TMath::Abs(j[0]-j[1])>mMtd_qt_tac_diff_range_abs) continue;

	  // position correction
	  int module = mQTtoModule[im][i];
	  Int_t sumTac = int( j[0] + j[1] + abs(module-3)*1./8 * (j[0]-j[1]) );
	  mMtdData.mtdQTtacSum[im][i] = sumTac;

	  if(mxq_mtdtacsum[im][0] < sumTac)
	    {
	      mxq_mtdtacsum[im][1] = mxq_mtdtacsum[im][0];
	      mxq_mtdtacsum[im][0] = sumTac;

	      mMtdData.mtdQThigh2Pos[im][1] = mMtdData.mtdQThigh2Pos[im][0];
	      mMtdData.mtdQThigh2Pos[im][0] = i+1;
	    }
	  else if (mxq_mtdtacsum[im][1] < sumTac)
	    {
	      mxq_mtdtacsum[im][1]  = sumTac;
	      mMtdData.mtdQThigh2Pos[im][1] = i+1;
	    }

	  int bin = 0;
	  if(mRunYear!=2016) bin = im*8+i+1;
	  else               bin = im*4+i/2+1;
	  mhMtdQTJ2J3Diff->Fill(bin,j[1]-j[0]);
	}
    }

  // MTD MIX trigger information
  for(Int_t im=0; im<kNQTboard; im++)
    {
      if(mRunYear!=2016 && im>=4)  continue;

      int idx = 0;
      if(mRunYear == 2016) idx = im/2*3 + im%2*16;
      else                 idx = im*3;
      mMtdData.mixMtdTacSum[im][0] = (trigData->mtdDsmAtCh(idx,ip)) + ((trigData->mtdDsmAtCh(idx+1,ip)&0x3)<<8);
      mMtdData.mixMtdTacSum[im][1] = (trigData->mtdDsmAtCh(idx+1,ip)>>4) + ((trigData->mtdDsmAtCh(idx+2,ip)&0x3f)<<4);
    }	

  // TF201
  for(int im=0; im<kNQTboard; im++)
    {
      for(int j=0; j<2; j++)
	{
	  mTrigQTpos[im][j] = -1;
	}
    }
  mMtdData.TF201Bit = trigData->dsmTF201Ch(0);
  mMtdData.TF201Bit2 = 0;
  if(mRunYear==2016) mMtdData.TF201Bit2 = trigData->dsmTF201Ch(6);
  for(Int_t i = 0; i < 4; i++)
    {
      for(Int_t j=0; j<2; j++)
	{
	  if(mRunYear==2016)
	    {
	      if((mMtdData.TF201Bit>>(i*2+j+4))&0x1)
		{
		  int qt = i*2;
		  mTrigQTpos[qt][j] = mMtdData.mtdQThigh2Pos[qt][j];
		}
	      if((mMtdData.TF201Bit2>>(i*2+j+4))&0x1)
		{
		  int qt = i*2+1;
		  mTrigQTpos[qt][j] = mMtdData.mtdQThigh2Pos[qt][j];
		}
	    }
	  else
	    {
	      if((mMtdData.TF201Bit>>(i*2+j+4))&0x1)
		{
		  mTrigQTpos[i][j] = mMtdData.mtdQThigh2Pos[i][j];
		}
	    }
	}
    }			
}

//_____________________________________________________________________________
void StMtdQAMaker::fillHistos()
{
  // event
  mhRefMult->Fill(mMtdData.refMult);
  mhgRefMult->Fill(mMtdData.gRefMult);

  // vertex
  mhVertexZ->Fill(mMtdData.vertexZ);
  mhVertexXY->Fill(mMtdData.vertexX,mMtdData.vertexY);
  mhVertexXZ->Fill(mMtdData.vertexZ,mMtdData.vertexX);
  mhVertexYZ->Fill(mMtdData.vertexZ,mMtdData.vertexY);

  // TOF
  mhTofStartTime->Fill(mMtdData.tofStartTime);

  // VPD
  for(Int_t i=0; i<kMaxVpdChan; i++)
    {
      Int_t type = (i/8)%2;
      if(type==0) mhVpdQTadc->Fill(i+1,mMtdData.vpdHi[i]);
      else        mhVpdQTtac->Fill(i+1,mMtdData.vpdHi[i]);
    }  

  // MTD trigger
  Int_t vpdTacSum = mMtdData.vpdTacSum;
  Int_t nQTsignal = 0;
  for(Int_t im=0; im<kNQTboard; im++)
    {
      for(Int_t i=0; i<8; i++)
	{
	  if(mRunYear!=2016 && im>=4)  continue;
	  if(mRunYear==2016 && i%2==0) continue;
	  for(Int_t k=0; k<2; k++)
	    {	      
	      int index = 0;
	      if(mRunYear!=2016) index = im*16 + i*2 + k + 1;
	      else               index = im*8 + (i/2)*2 + k + 1;
	      if(mMtdData.mtdQTtac[im][i*2+k]>mMtd_qt_tac_min)
		{
		  mhMtdQTAdcAll->Fill(index,mMtdData.mtdQTadc[im][i*2+k]);
		  mhMtdQTTacAll->Fill(index,mMtdData.mtdQTtac[im][i*2+k]);
		}
	      mhMtdQTAdcVsTacAll->Fill(mMtdData.mtdQTtac[im][i*2+k],mMtdData.mtdQTadc[im][i*2+k]);
	    }
	  Int_t mtdTacSum = mMtdData.mtdQTtacSum[im][i];
	  int bin = 0;
	  if(mRunYear!=2016) bin = im*8+i+1;
	  else               bin = im*4+i/2+1;
	  if(mtdTacSum>0)
	    {
	      nQTsignal++;
	      mhMtdVpdTacDiffMT001->Fill(bin,mtdTacSum-vpdTacSum);
	    }
	}
    }
  mhNQtSignal->Fill(nQTsignal);

  Int_t nMT101signal = 0;
  for(Int_t im=0; im<kNQTboard; im++)
    {
      if(mRunYear!=2016 && im>=4)  continue;
      for(Int_t j=0; j<2; j++)
	{
	  Int_t mix_tacSum = mMtdData.mixMtdTacSum[im][j];
	  if(mix_tacSum>0) 
	    {
	      nMT101signal++;
	      Int_t mxq_tacPos = mMtdData.mtdQThigh2Pos[im][j];
	      Int_t mxq_tacSum = mMtdData.mtdQTtacSum[im][mxq_tacPos-1];
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
  for(int im=0; im<kNQTboard; im++)
    {
      for(int j=0; j<2; j++)
	{
	  if(mTrigQTpos[im][j]>0) nTF201signal++;
	}
    }
  mhNTF201Signal->Fill(nTF201signal);

  // TPC tracks
  int nGoodTracks = mMtdData.nGoodTrack;
  mhNTrk->Fill(nGoodTracks);
  for(int i=0; i<nGoodTracks; i++)
    {
      double pt = mMtdData.trkPt[i];
      mhTrkPt           ->Fill(pt);
      mhTrkDcaVsPt      ->Fill(pt, mMtdData.trkDca[i]);
      mhTrkPhiVsPt      ->Fill(pt, mMtdData.trkPhi[i]);
      mhTrkEtaVsPt      ->Fill(pt, mMtdData.trkEta[i]);
      mhTrkNHitsFitVsPt ->Fill(pt, mMtdData.trkNHitsFit[i]);
      mhTrkNHitsDedxVsPt->Fill(pt, mMtdData.trkNHitsDedx[i]);
      mhTrkDedxVsPt     ->Fill(pt, mMtdData.trkDedx[i]);
      mhTrkNsigmaPiVsPt ->Fill(pt, mMtdData.trkNsigmaPi[i]);
      if(pt>1) 
	{
	  mhTrkPhiEta       ->Fill(mMtdData.trkEta[i], mMtdData.trkPhi[i]);
	  mhTrkNsigmaPiVsPhi->Fill(mMtdData.trkPhi[i], mMtdData.trkNsigmaPi[i]);
	  mhTrkNsigmaPiVsEta->Fill(mMtdData.trkEta[i], mMtdData.trkNsigmaPi[i]);
	}
      if(mMtdData.isTrkTofMatched[i])
	{
	  Int_t tofTray = mMtdData.trkMthTofTray[i];
	  Int_t tofModule = mMtdData.trkMthTofModule[i];
	  if(tofTray>60 && tofTray<=120) tofModule += 32;
	  
	  mhTofMthTrkLocaly->Fill(tofTray,mMtdData.trkMthTofLocaly[i]);
	  mhTofMthTrkLocalz->Fill(tofModule,mMtdData.trkMthTofLocalz[i]);
	}

      Int_t backleg = mMtdData.trkProjBackleg[i];
      Int_t module  = mMtdData.trkProjModule[i];
      Int_t cell    = mMtdData.trkProjChannel[i];
      if(backleg>0 && backleg<=30 && module>=1 && module<=5 && cell>=0 && cell<=11)
	{
	  mhMtdTrackProjMap->Fill(backleg, (module-1)*12+cell);
	}
    }

  // MTD raw hits
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
      if(flag>0) mhMtdRawHitLeTime->Fill(gChan,mMtdData.mtdRawHitTdc[i]);
      else       mhMtdRawHitTrTime->Fill(gChan,mMtdData.mtdRawHitTdc[i]);

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
  
  LOG_DEBUG << "+++++ New event +++++\n" << endm;  
  // MTD hits
  int nMtdHits = mMtdData.nMtdHits;
  int nTrigMtdHit = 0, nTrigMtdHitMth = 0;
  mhMtdNHits->Fill(nMtdHits);
  mhMtdNMatchHits->Fill(mMtdData.nMatchMtdHits);
  for(Int_t i=0; i<nMtdHits; i++)
    {
      Int_t backleg   = mMtdData.mtdHitBackleg[i];
      Int_t module    = mMtdData.mtdHitModule[i];
      Int_t lChan     = (module-1)*12+mMtdData.mtdHitChan[i];
      Int_t gChan     = (backleg-1)*60 + lChan;
      Int_t qt        = mModuleToQT[backleg-1][module-1];
      Int_t pos       = mModuleToQTPos[backleg-1][module-1];
      double trigTime = mMtdData.mtdHitTrigTime[i];
      bool isMatched  = mMtdData.isMatched[i];
      bool isTrig     = mMtdData.isMtdTrig[i];
      double dz       = mMtdData.mtdMatchTrkDeltaz[i];
      double dy       = mMtdData.mtdMatchTrkDeltay[i];
      double dtof     = mMtdData.mtdMatchTrkTof[i]-mMtdData.mtdMatchTrkExpTof[i];
      double nsigPi   = mMtdData.mtdMatchTrkNsigmaPi[i];
      double dca      = mMtdData.mtdMatchTrkDca[i];
      bool isMthTof   = (isMatched && mMtdData.mtdMatchTrkTofHit[i]);
      bool isMuon     = false;
      if(isMatched == 1 && fabs(dca) < 1 &&
	 fabs(dz) < 20 && fabs(dy) < 20 && nsigPi > -1 && nsigPi < 3 && dtof < 1)
	{
	  isMuon = true;
	}

      mhMtdHitMap       ->Fill(backleg,lChan);
      mhMtdHitLeTimeDiff->Fill(gChan,mMtdData.mtdHitLeTimeEast[i]-mMtdData.mtdHitLeTimeWest[i]);
      mhMtdHitTotWest   ->Fill(gChan,mMtdData.mtdHitTotWest[i]);
      mhMtdHitTotEast   ->Fill(gChan,mMtdData.mtdHitTotEast[i]);
      mhMtdHitTrigTime  ->Fill(gChan,mMtdData.mtdHitTrigTime[i]);
      if(isMatched) mhMtdHitTrigTimeTrkMth->Fill(gChan,trigTime);
      if(isMthTof)  mhMtdHitTrigTimeTrkMthTof->Fill(gChan,trigTime);
      if(isMuon)    mhMtdHitTrigTimeMuon->Fill(gChan,trigTime);
      if(isTrig==1) mhMtdHitTrigTimeTrig->Fill(gChan,trigTime);
      if(qt>=1 && qt<=kNQTboard && pos>=1 && pos<=8)
	{
	  Int_t tHub = getMtdHitTHUB(backleg);
	  double mtdTime[2];
	  mtdTime[0] = mMtdData.mtdHitLeTimeEast[i]-mMtdData.mtdTriggerTime[tHub-1];
	  if(mtdTime[0]<0) mtdTime[0] += 51200;
	  mtdTime[1] = mMtdData.mtdHitLeTimeWest[i]-mMtdData.mtdTriggerTime[tHub-1];
	  if(mtdTime[1]<0) mtdTime[1] += 51200;
	  for(int k=0; k<2; k++)
	    {
	      if(module<4)
		{
		  if(mMtdData.mtdQTtac[(qt-1)][(pos-1)*2+k]>mMtd_qt_tac_min)
		    {
		      mhMtdHitTrigTimeVsQtAdc[k]->Fill(mMtdData.mtdQTadc[(qt-1)][(pos-1)*2+k], mtdTime[k]);
		      mhMtdHitTrigTimeVsQtTac[k]->Fill(mMtdData.mtdQTtac[(qt-1)][(pos-1)*2+k], mtdTime[k]);
		    }
		}
	      else
		{
		  if(mMtdData.mtdQTtac[(qt-1)][(pos-1)*2+1-k]>mMtd_qt_tac_min)
		    {
		      mhMtdHitTrigTimeVsQtAdc[k]->Fill(mMtdData.mtdQTadc[(qt-1)][(pos-1)*2+1-k], mtdTime[k]);
		      mhMtdHitTrigTimeVsQtTac[k]->Fill(mMtdData.mtdQTtac[(qt-1)][(pos-1)*2+1-k], mtdTime[k]);
		    }
		}
	    }
	  if(mMtdData.mtdQTadc[(qt-1)][(pos-1)*2]>100 && 
	     mMtdData.mtdQTadc[(qt-1)][(pos-1)*2+1]>100 &&
	     mMtdData.mtdQTtac[(qt-1)][(pos-1)*2]>mMtd_qt_tac_min && 
	     mMtdData.mtdQTtac[(qt-1)][(pos-1)*2+1]>mMtd_qt_tac_min)
	    {
	      mhMtdHitTrigTimeGoodQT->Fill(gChan,trigTime);
	    }
	}

      if(isMatched == 1)
	{
	  double trkPt = mMtdData.mtdMatchTrkPt[i];
	  int charge = mMtdData.mtdMatchTrCharge[i];
	  mhMtdMatchHitMap         ->Fill(backleg,lChan);
	  mhMtdMatchTrkPt          ->Fill(trkPt);
	  mhMtdMatchTrkPhiEta      ->Fill(mMtdData.mtdMatchTrkEta[i],mMtdData.mtdMatchTrkPhi[i]);
	  mhMtdMatchDzVsChan       ->Fill(gChan, dz);
	  mhMtdMatchDyVsChan       ->Fill(gChan, dy);
	  mhMtdMatchDtofVsPt       ->Fill(trkPt,dtof);
	  mhMtdMatchMtdTofVsChan   ->Fill(gChan, mMtdData.mtdMatchTrkTof[i]);
	  mhMtdMatchExpTofVsChan   ->Fill(gChan, mMtdData.mtdMatchTrkExpTof[i]);
	  mhMtdMatchDtofVsChan     ->Fill(gChan,dtof);
	  mhMtdMatchLocalyVsChan   ->Fill(gChan,mMtdData.mtdMatchTrkLocaly[i]);
	  mhMtdMatchLocalzVsChan   ->Fill(gChan,mMtdData.mtdMatchTrkLocalz[i]);

	  if(charge>0) mhMtdMatchDzVsPtPos->Fill(trkPt,dz);
	  else         mhMtdMatchDzVsPtNeg->Fill(trkPt,dz);
	  if(charge>0) mhMtdMatchDyVsPtPos->Fill(trkPt,dy);
	  else         mhMtdMatchDyVsPtNeg->Fill(trkPt,dy);

	  if(qt>=1 && qt<=kNQTboard && pos>=1 && pos<=8)
	    {
	      for(Int_t k=0; k<2; k++)
		{	      
		  int bin = 0;
		  if(mRunYear!=2016) bin = (qt-1)*16 + (pos-1)*2 + k + 1;
		  else               bin = (qt-1)*8 + ((pos-1)/2)*2 + k + 1;
		  int adc = mMtdData.mtdQTadc[(qt-1)][(pos-1)*2+k];
		  int tac = mMtdData.mtdQTtac[(qt-1)][(pos-1)*2+k];
		  if(tac>mMtd_qt_tac_min)
		    {
		      mhMtdQTAdcMth->Fill(bin,adc);
		      mhMtdQTTacMth->Fill(bin,tac);
		      if(isMthTof)
			{
			  mhMtdQTAdcMthTof->Fill(bin,adc);
			  mhMtdQTTacMthTof->Fill(bin,tac);
			}
		      if(isMuon)
			{
			  mhMtdQTAdcMuon->Fill(bin,adc);
			  mhMtdQTTacMuon->Fill(bin,tac);
			}
		    }
		}
	      int mtdTacSum = mMtdData.mtdQTtacSum[qt-1][pos-1];
	      int bin = 0;
	      if(mRunYear!=2016) bin = (qt-1)*8+pos;
	      else               bin = (qt-1)*4+pos/2;
	      if(mtdTacSum>0)
		{
		  mhMtdVpdTacDiffMT001Mth->Fill(bin,mtdTacSum-vpdTacSum);
		  if(isMthTof)
		    mhMtdVpdTacDiffMT001MthTof->Fill(bin,mtdTacSum-vpdTacSum);
		  if(isMuon)
		    mhMtdVpdTacDiffMT001Muon->Fill(bin,mtdTacSum-vpdTacSum);
		}

	      Int_t index = -1;
	      if(pos == mMtdData.mtdQThigh2Pos[qt-1][0]) index = 0;
	      if(pos == mMtdData.mtdQThigh2Pos[qt-1][1]) index = 1;
	      if( index>=0 && mMtdData.mixMtdTacSum[qt-1][index]>0 )
		{
		  mhMtdVpdTacDiffMT101Mth->Fill(bin, mMtdData.mixMtdTacSum[qt-1][index]-vpdTacSum/8+1024);
		  if(isMthTof)
		    mhMtdVpdTacDiffMT101MthTof->Fill(bin, mMtdData.mixMtdTacSum[qt-1][index]-vpdTacSum/8+1024);
		  if(isMuon)
		    mhMtdVpdTacDiffMT101Muon->Fill(bin, mMtdData.mixMtdTacSum[qt-1][index]-vpdTacSum/8+1024);
		}
	    }
	}

      if(isTrig == 1)
	{
	  nTrigMtdHit ++;
	  mhMtdTrigHitMap->Fill(backleg,lChan);
	  if(isMatched==1)
	    {
	      nTrigMtdHitMth++;
	      mhMtdTrigMthHitMap->Fill(backleg,lChan);
	    }
	}
    }
  mhMtdTrigNHits->Fill(nTrigMtdHit);
  mhMtdTrigMthNHits->Fill(nTrigMtdHitMth);
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
  mhEventCuts->SetBinContent(7,mMtd_qt_tac_min);
  mhEventCuts->GetXaxis()->SetBinLabel(8,"mtd_qt_tac_max");
  mhEventCuts->SetBinContent(8,mMtd_qt_tac_max);
  mhEventCuts->GetXaxis()->SetBinLabel(9,"mtd_qt_tac_diff_range_abs");
  mhEventCuts->SetBinContent(9,mMtd_qt_tac_diff_range_abs);
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
  mhEventCuts->GetXaxis()->SetBinLabel(17,"mVertexMode");
  mhEventCuts->SetBinContent(17, mVertexMode);

  const Int_t nbins = 3 + mTriggerIDs.size();
  mhEventTrig = new TH1F("hEventStat","Event statistics",nbins,0.,(Float_t)nbins);
  mhEventTrig->GetXaxis()->SetBinLabel(1,"All events");
  mhEventTrig->GetXaxis()->SetBinLabel(2,"Good trigger");
  mhEventTrig->GetXaxis()->SetBinLabel(3,"Vtx Cuts");
  for(UInt_t i=0; i<mTriggerIDs.size(); i++)
    {
      mhEventTrig->GetXaxis()->SetBinLabel(i+4,Form("%d",mTriggerIDs[i]));
    }
  AddHist(mhEventTrig);
  
  mhRunId = new TH1F("hRunId","Statistics per run",mEndRun-mStartRun,mStartRun,mEndRun);
  AddHist(mhRunId);

  mhRefMult = new TH1F("hRefMult","RefMult distribution;RefMult",1000,0,1000);
  AddHist(mhRefMult);

  mhgRefMult = new TH1F("hgRefMult","gRefMult distribution;gRefMult",1000,0,1000);
  AddHist(mhgRefMult);

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

  mhVtxClosestIndex = new TH1F("hVtxClosestIndex","Index of TPC vertex closest to VPD vertex (ranking>0)",50,0,50);
  AddHist(mhVtxClosestIndex);

  mhVtxZvsVpdVzClosest = new TH2F("hVtxZvsVpdVzClosest","Primary vertex z: VPD vs TPC (closest);TPC z_{vtx} (cm);VPD z_{vtx} (cm)",201,-201,201,201,-201,201);
  AddHist(mhVtxZvsVpdVzClosest);

  mhVtxZDiffClosest = new TH1F("hVtxZDiffClosest","TPC vz - VPD vz (closest); #Deltavz (cm)",400,-20,20);
  AddHist(mhVtxZDiffClosest);

  // TOF histograms
  mhTofStartTime = new TH1F("hTofStartTime","Start time from TOF; t_{start}",40,0,2e5);
  AddHist(mhTofStartTime);

  // VPD QT information
  mhVpdQTadc = new TH2F("hVpdQTadc","VPD QT: ADC vs channel;channel;ADC",64,0.5,64.5,250,0,2500);
  AddHist(mhVpdQTadc);

  mhVpdQTtac = new TH2F("hVpdQTtac","VPD QT: TAC vs channel;channel;TAC",64,0.5,64.5,200,500,2500);
  AddHist(mhVpdQTtac);

  // Primary tracks
  mhNTrk = new TH1F("hNTrk","Number of good primary tracks per event;N",1000,0,1000);
  AddHist(mhNTrk);

  mhTrkPt = new TH1F("hTrkPt","p_{T} of primary tracks;p_{T} (GeV/c)",100,0,20);
  AddHist(mhTrkPt);

  mhTrkDcaVsPt = new TH2F("hTrkDcaVsPt","Dca vs p_{T} of primary tracks;p_{T} (GeV/c);dca (cm)",100,0,20,35,0,3.5);
  AddHist(mhTrkDcaVsPt);

  mhTrkPhiVsPt = new TH2F("hTrkPhiVsPt","#varphi vs p_{T} of primary tracks;p_{T} (GeV/c);#varphi",100,0,20,150,0,2*pi);
  AddHist(mhTrkPhiVsPt);

  mhTrkEtaVsPt = new TH2F("hTrkEtaVsPt","#eta vs p_{T} of primary tracks;p_{T} (GeV/c);#eta",100,0,20,50,-1,1);
  AddHist(mhTrkEtaVsPt);

  mhTrkPhiEta = new TH2F("hTrkPhiEta","#varphi vs #eta of primary tracks (p_{T} > 1 GeV/c);#eta;#varphi",50,-1,1,150,0,2*pi);
  AddHist(mhTrkPhiEta);

  mhTrkNHitsFitVsPt = new TH2F("hTrkNHitsFitVsPt","NHitsFit vs p_{T} of primary tracks;p_{T} (GeV/c);NHitsFit",100,0,20,45,0,45);
  AddHist(mhTrkNHitsFitVsPt);

  mhTrkNHitsDedxVsPt = new TH2F("hTrkNHitsDedxVsPt","NHitsDedx vs p_{T} of primary tracks;p_{T} (GeV/c);NHitsDedx",100,0,20,45,0,45);
  AddHist(mhTrkNHitsDedxVsPt);

  mhTrkDedxVsPt = new TH2F("hTrkDedxVsPt","dE/dx vs p_{T} of primary tracks;p_{T} (GeV/c);dE/dx (keV/cm)",100,0,20,100,0,10);
  AddHist(mhTrkDedxVsPt);

  mhTrkNsigmaPiVsPt = new TH2F("hTrkNsigmaPiVsPt","n#sigma_{#pi} vs p_{T} of primary tracks;p_{T} (GeV/c);n#sigma_{#pi}",100,0,20,100,-5,5);
  AddHist(mhTrkNsigmaPiVsPt);

  mhTrkNsigmaPiVsPhi = new TH2F("hTrkNsigmaPiVsPhi","n#sigma_{#pi} vs #varphi of primary tracks (p_{T} > 1 GeV/c);#varphi;n#sigma_{#pi}",150,0,2*pi,100,-5,5);
  AddHist(mhTrkNsigmaPiVsPhi);

  mhTrkNsigmaPiVsEta = new TH2F("hTrkNsigmaPiVsEta","n#sigma_{#pi} vs #eta of primary tracks (p_{T} > 1 GeV/c);#eta;n#sigma_{#pi}",50,-1,1,100,-5,5);
  AddHist(mhTrkNsigmaPiVsEta);

  mhTofMthTrkLocaly = new TH2F("hTofMthTrkLocaly","TOF match: local y vs tray;tray;local y (cm)",120,0.5,120.5,100,-5,5);
  AddHist(mhTofMthTrkLocaly);

  mhTofMthTrkLocalz = new TH2F("hTofMthTrkLocalz","TOF match: local z vs module;module;local z (cm)",64,0.5,64.5,100,-5,5);
  AddHist(mhTofMthTrkLocalz);

  mhMtdTrackProjMap = new TH2F("hMtdTrackProjMap","MTD: channel vs backleg of projected primary tracks;backleg;channel",30,0.5,30.5,60,-0.5,59.5);
  AddHist(mhMtdTrackProjMap);

  // MTD QT information
  mhMtdQTAdcAll = new TH2F("hMtdQTAdcAll","MTD QT: ADC vs channel (All);;ADC",64,0.5,64.5,350,0,3500);
  AddHist(mhMtdQTAdcAll);

  mhMtdQTAdcMth = new TH2F("hMtdQTAdcMth","MTD QT: ADC vs channel (Track matched);;ADC",64,0.5,64.5,350,0,3500);
  AddHist(mhMtdQTAdcMth);

  mhMtdQTAdcMthTof = new TH2F("hMtdQTAdcMthTof","MTD QT: ADC vs channel (TOF track matched);;ADC",64,0.5,64.5,350,0,3500);
  AddHist(mhMtdQTAdcMthTof);

  mhMtdQTAdcMuon = new TH2F("hMtdQTAdcMuon","MTD QT: ADC vs channel (Muon PID);;ADC",64,0.5,64.5,350,0,3500);
  AddHist(mhMtdQTAdcMuon);

  mhMtdQTTacAll = new TH2F("hMtdQTTacAll","MTD QT: TAC vs channel (All);;TAC",64,0.5,64.5,300,0,3000);
  AddHist(mhMtdQTTacAll);

  mhMtdQTTacMth = new TH2F("hMtdQTTacMth","MTD QT: TAC vs channel (Track matched);;TAC",64,0.5,64.5,300,0,3000);
  AddHist(mhMtdQTTacMth);

  mhMtdQTTacMthTof = new TH2F("hMtdQTTacMthTof","MTD QT: TAC vs channel (TOF track matched);;TAC",64,0.5,64.5,300,0,3000);
  AddHist(mhMtdQTTacMthTof);

  mhMtdQTTacMuon = new TH2F("hMtdQTTacMuon","MTD QT: TAC vs channel (Muon PID);;TAC",64,0.5,64.5,300,0,3000);
  AddHist(mhMtdQTTacMuon);

  mhMtdQTAdcVsTacAll = new TH2F("hMtdQTAdcVsTacAll","MTD QT: ADC vs. TAC (All);TAC;ADC",350,0,3500,350,0,3500);
  AddHist(mhMtdQTAdcVsTacAll);

  mhMtdQTJ2J3Diff = new TH2F("hMtdQTJ2J3Diff","MTD QT: J3-J2 TAC vs channel;;TAC (J3-J2)",32,0.5,32.5,160,-800,800);
  AddHist(mhMtdQTJ2J3Diff);

  mhMtdVpdTacDiffMT001 = new TH2F("hMtdVpdTacDiffMT001","QT: MTD-VPD tac difference (All);;tac_{MTD}-tac_{VPD}",32,0.5,32.5,600,-6000,6000);
  AddHist(mhMtdVpdTacDiffMT001);

  mhMtdVpdTacDiffMT001Mth = new TH2F("hMtdVpdTacDiffMT001Mth","QT: MTD-VPD tac difference (Track matched);;tac_{MTD}-tac_{VPD}",32,0.5,32.5,600,-6000,6000);
  AddHist(mhMtdVpdTacDiffMT001Mth);

  mhMtdVpdTacDiffMT001MthTof = new TH2F("hMtdVpdTacDiffMT001MthTof","QT: MTD-VPD tac difference (TOF track matched);;tac_{MTD}-tac_{VPD}",32,0.5,32.5,600,-6000,6000);
  AddHist(mhMtdVpdTacDiffMT001MthTof);

  mhMtdVpdTacDiffMT001Muon = new TH2F("hMtdVpdTacDiffMT001Muon","QT: MTD-VPD tac difference (Muon PID);;tac_{MTD}-tac_{VPD}",32,0.5,32.5,600,-6000,6000);
  AddHist(mhMtdVpdTacDiffMT001Muon);

  mhMtdVpdTacDiffMT101 = new TH2F("hMtdVpdTacDiffMT101","MT101: MTD-VPD tac difference;;tac_{MTD}-tac_{VPD}+1024",32,0.5,32.5,1024,0,2048);
  AddHist(mhMtdVpdTacDiffMT101);

  mhMtdVpdTacDiffMT101Mth = new TH2F("hMtdVpdTacDiffMT101Mth","MT101: MTD-VPD tac difference (Track matched);;tac_{MTD}-tac_{VPD}+1024",32,0.5,32.5,1024,0,2048);
  AddHist(mhMtdVpdTacDiffMT101Mth);

  mhMtdVpdTacDiffMT101MthTof = new TH2F("hMtdVpdTacDiffMT101MthTof","MT101: MTD-VPD tac difference (TOF track matched);;tac_{MTD}-tac_{VPD}+1024",32,0.5,32.5,1024,0,2048);
  AddHist(mhMtdVpdTacDiffMT101MthTof);

  mhMtdVpdTacDiffMT101Muon = new TH2F("hMtdVpdTacDiffMT101Muon","MT101: MTD-VPD tac difference (Muon PID);;tac_{MTD}-tac_{VPD}+1024",32,0.5,32.5,1024,0,2048);
  AddHist(mhMtdVpdTacDiffMT101Muon);

  for(Int_t i=0; i<64; i++)
    {
      mhMtdQTAdcAll->GetXaxis()->SetBinLabel(i+1,qtlabel[i]);
      mhMtdQTAdcMth->GetXaxis()->SetBinLabel(i+1,qtlabel[i]);
      mhMtdQTAdcMthTof->GetXaxis()->SetBinLabel(i+1,qtlabel[i]);
      mhMtdQTAdcMuon->GetXaxis()->SetBinLabel(i+1,qtlabel[i]);
      mhMtdQTTacAll->GetXaxis()->SetBinLabel(i+1,qtlabel[i]);
      mhMtdQTTacMth->GetXaxis()->SetBinLabel(i+1,qtlabel[i]);
      mhMtdQTTacMthTof->GetXaxis()->SetBinLabel(i+1,qtlabel[i]);
      mhMtdQTTacMuon->GetXaxis()->SetBinLabel(i+1,qtlabel[i]);
    }
  for(Int_t i=0; i<32; i++)
    {
      mhMtdVpdTacDiffMT001->GetXaxis()->SetBinLabel(i+1,qtlabel2[i]);
      mhMtdVpdTacDiffMT001Mth->GetXaxis()->SetBinLabel(i+1,qtlabel2[i]);
      mhMtdVpdTacDiffMT001MthTof->GetXaxis()->SetBinLabel(i+1,qtlabel2[i]);
      mhMtdVpdTacDiffMT001Muon->GetXaxis()->SetBinLabel(i+1,qtlabel2[i]);
      mhMtdVpdTacDiffMT101->GetXaxis()->SetBinLabel(i+1,qtlabel2[i]);
      mhMtdVpdTacDiffMT101Mth->GetXaxis()->SetBinLabel(i+1,qtlabel2[i]);
      mhMtdVpdTacDiffMT101MthTof->GetXaxis()->SetBinLabel(i+1,qtlabel2[i]);
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
  mhMtdNHits = new TH1F("hMtdNHits","Number of MTD hits per event;N",10,0,10);
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

  mhMtdHitTrigTimeTrkMthTof = new TH2F("hMtdHitTrigTimeTrkMthTof","MTD: trigger time of hits (TOF track matched);channel;tdc-t_{trigger} (ns)",1801,-0.5,1800.5,nBinsTrigTime,minTrigTime,maxTrigTime);
  AddHist(mhMtdHitTrigTimeTrkMthTof);

  mhMtdHitTrigTimeMuon = new TH2F("hMtdHitTrigTimeMuon","MTD: trigger time of hits (Muon PID);channel;tdc-t_{trigger} (ns)",1801,-0.5,1800.5,nBinsTrigTime,minTrigTime,maxTrigTime);
  AddHist(mhMtdHitTrigTimeMuon);

  mhMtdHitTrigTimeGoodQT = new TH2F("hMtdHitTrigTimeGoodQT","MTD: trigger time of hits (Good QT);channel;tdc-t_{trigger} (ns)",1801,-0.5,1800.5,nBinsTrigTime,minTrigTime,maxTrigTime);
  AddHist(mhMtdHitTrigTimeGoodQT);

  mhMtdHitTrigTimeTrig = new TH2F("hMtdHitTrigTimeTrig","MTD: trigger time of triggering hits;channel;tdc-t_{trigger} (ns)",1801,-0.5,1800.5,nBinsTrigTime,minTrigTime,maxTrigTime);
  AddHist(mhMtdHitTrigTimeTrig);

  mhMtdHitTrigTimeVsQtAdc[0] = new TH2F("hMtdHitTrigTimeVsQtAdcE","MTD: trigger time vs. QT ADC (East);QT Adc;tdc_{east}-t_{trigger} (ns)",350,0,3500,nBinsTrigTime,minTrigTime,maxTrigTime);
  AddHist(mhMtdHitTrigTimeVsQtAdc[0]);

  mhMtdHitTrigTimeVsQtAdc[1] = new TH2F("hMtdHitTrigTimeVsQtAdcW","MTD: trigger time vs. QT ADC (West);QT Adc;tdc_{west}-t_{trigger} (ns)",350,0,3500,nBinsTrigTime,minTrigTime,maxTrigTime);
  AddHist(mhMtdHitTrigTimeVsQtAdc[1]);

  mhMtdHitTrigTimeVsQtTac[0] = new TH2F("hMtdHitTrigTimeVsQtTacE","MTD: trigger time vs. QT TAC (East);QT Tac;tdc_{east}-t_{trigger} (ns)",300,0,3000,nBinsTrigTime,minTrigTime,maxTrigTime);
  AddHist(mhMtdHitTrigTimeVsQtTac[0]);

  mhMtdHitTrigTimeVsQtTac[1] = new TH2F("hMtdHitTrigTimeVsQtTacW","MTD: trigger time vs. QT TAC (West);QT Tac;tdc_{west}-t_{trigger} (ns)",300,0,3000,nBinsTrigTime,minTrigTime,maxTrigTime);
  AddHist(mhMtdHitTrigTimeVsQtTac[1]);


  // ===== matched hits
  mhMtdNMatchHits = new TH1F("mhMtdNMatchHits","Number of matched MTD hits per event;N",100,0,100);
  AddHist(mhMtdNMatchHits);

  mhMtdMatchHitMap = new TH2F("hMtdMatchHitMap","MTD: channel vs backleg of matched hits;backleg;channel",30,0.5,30.5,60,-0.5,59.5);
  AddHist(mhMtdMatchHitMap);

  mhMtdMatchTrkPt = new TH1F("hMtdMatchTrkPt","MTD: p_{T} of matched primary tracks;p_{T} (GeV/c)",100,0,20);
  AddHist(mhMtdMatchTrkPt);

  mhMtdMatchTrkPhiEta = new TH2F("hMtdMatchTrkPhiEta","MTD: #varphi vs #eta of matched primary tracks;#eta;#varphi",50,-1,1,150,0,2*pi);
  AddHist(mhMtdMatchTrkPhiEta);

  mhMtdMatchDzVsChan = new TH2F("hMtdMatchDzVsChan","MTD: #Deltaz distribution;channel;#Deltaz = z_{proj}-z_{hit} (cm)",1801,-0.5,1800.5,200,-100,100);
  AddHist(mhMtdMatchDzVsChan);

  mhMtdMatchDzVsPtPos = new TH2F("hMtdMatchDzVsPtPos","MTD: #Deltaz vs p_{T} for positive tracks;p_{T} (GeV/c);#Deltaz (cm)",100,0,20,200,-100,100);
  AddHist(mhMtdMatchDzVsPtPos);

  mhMtdMatchDzVsPtNeg = new TH2F("hMtdMatchDzVsPtNeg","MTD: #Deltaz vs p_{T} for negative tracks;p_{T} (GeV/c);#Deltaz (cm)",100,0,20,200,-100,100);
  AddHist(mhMtdMatchDzVsPtNeg);

  mhMtdMatchDyVsChan = new TH2F("hMtdMatchDyVsChan","MTD: #Deltay distribution;channel;#Deltay = y_{proj}-y_{hit} (cm)",1801,-0.5,1800.5,200,-100,100);
  AddHist(mhMtdMatchDyVsChan);

  mhMtdMatchDyVsPtPos = new TH2F("hMtdMatchDyVsPtPos","MTD: #Deltay vs p_{T} for positive tracks;p_{T} (GeV/c);#Deltay (cm)",100,0,20,200,-100,100);
  AddHist(mhMtdMatchDyVsPtPos);

  mhMtdMatchDyVsPtNeg = new TH2F("hMtdMatchDyVsPtNeg","MTD: #Deltay vs p_{T} for negative tracks;p_{T} (GeV/c);#Deltay (cm)",100,0,20,200,-100,100);
  AddHist(mhMtdMatchDyVsPtNeg);

  mhMtdMatchDtofVsPt = new TH2F("hMtdMatchDtofVsPt","MTD: #Deltatof vs p_{T} distribution;p_{T} (GeV/c);#Deltatof (ns)",100,0,20,1000,-5,5);
  AddHist(mhMtdMatchDtofVsPt);

  mhMtdMatchMtdTofVsChan = new TH2F("hMtdMatchMtdTofVsChan","MTD: MTD time vs channel of primary tracks;channel;tof_{MTD} (ns)",1800,-0.5,1799.5,300,0,30);
  AddHist(mhMtdMatchMtdTofVsChan);

  mhMtdMatchExpTofVsChan = new TH2F("hMtdMatchExpTofVsChan","MTD: TPC time vs channel of primary tracks;channel;tof_{expected} (ns)",1800,-0.5,1799.5,300,0,30);
  AddHist(mhMtdMatchExpTofVsChan);

  mhMtdMatchDtofVsChan = new TH2F("hMtdMatchDtofVsChan","MTD: #Deltatof distribution;channel;#Deltatof (ns)",1801,-0.5,1800.5,1000,-5,5);
  AddHist(mhMtdMatchDtofVsChan);

  mhMtdMatchLocalyVsChan = new TH2F("hMtdMatchLocalyVsChan","MTD: local y of matched tracks;channel;y (cm)",1801,-0.5,1800.5,100,-50.5,49.5);
  AddHist(mhMtdMatchLocalyVsChan);

  mhMtdMatchLocalzVsChan = new TH2F("hMtdMatchLocalzVsChan","MTD: local z of matched tracks;channel;z (cm)",1801,-0.5,1800.5,100,-50.5,49.5);
  AddHist(mhMtdMatchLocalzVsChan);

  mhNQtSignal = new TH1F("hNQtSignal","Number of good QT signals;N",10,0,10);
  AddHist(mhNQtSignal);

  mhNMT101Signal = new TH1F("hNMT101Signal","Number of good MT101 signals;N",10,0,10);
  AddHist(mhNMT101Signal);

  mhNTF201Signal = new TH1F("hNTF201Signal","Number of good TF201 signals;N",10,0,10);
  AddHist(mhNTF201Signal);

  mhMtdTrigNHits = new TH1F("hMtdTrigNHits","Number of triggering MTD hits per event;N",10,0,10);
  AddHist(mhMtdTrigNHits);

  mhMtdTrigHitMap = new TH2F("hMtdTrigHitMap","MTD: channel vs backleg of triggering hits;backleg;channel",30,0.5,30.5,60,-0.5,59.5);
  AddHist(mhMtdTrigHitMap);

  mhMtdTrigMthNHits = new TH1F("hMtdTrigMthNHits","Number of triggering MTD hits matched to tracks;N",10,0,10);
  AddHist(mhMtdTrigMthNHits);

  mhMtdTrigMthHitMap = new TH2F("hMtdTrigMthHitMap","MTD: channel vs backleg of triggering hits matched to tracks;backleg;channel",30,0.5,30.5,60,-0.5,59.5);
  AddHist(mhMtdTrigMthHitMap);
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
  mQATree->Branch("refMult",           &mMtdData.refMult,          "refMult/I");
  mQATree->Branch("gRefMult",          &mMtdData.gRefMult,         "gRefMult/I");
  mQATree->Branch("vertexX",           &mMtdData.vertexX,          "vertexX/F");
  mQATree->Branch("vertexY",           &mMtdData.vertexY,          "vertexY/F");
  mQATree->Branch("vertexZ",           &mMtdData.vertexZ,          "vertexZ/F");
  mQATree->Branch("vpdVz",             &mMtdData.vpdVz,            "vpdVz/F");

  // VPD information
  mQATree->Branch("pre",               &mMtdData.pre,               "pre/B");
  mQATree->Branch("post",              &mMtdData.post,              "post/B");
  mQATree->Branch("prepost",           &mMtdData.prepost,           "prepost/B");
  mQATree->Branch("vpdTacSum",         &mMtdData.vpdTacSum,         "vpdTacSum/s");
  mQATree->Branch("vpdHi",             &mMtdData.vpdHi,             "vpdHi[64]/s");
  mQATree->Branch("mtdQTadc",          &mMtdData.mtdQTadc,          "mtdQTadc[128]/s");
  mQATree->Branch("mtdQTtac",          &mMtdData.mtdQTtac,          "mtdQTtac[128]/s");
  mQATree->Branch("mtdQTtacSum",       &mMtdData.mtdQTtacSum,       "mtdQTtacSum[64]/s");
  mQATree->Branch("mtdQThigh2Pos",     &mMtdData.mtdQThigh2Pos,     "mtdQThigh2Pos[16]/s");
  mQATree->Branch("mixMtdTacSum",      &mMtdData.mixMtdTacSum,      "mixMtdTacSum[16]/s");
  mQATree->Branch("TF201Bit",          &mMtdData.TF201Bit,          "TF201Bit/I");
  mQATree->Branch("TF201Bit2",         &mMtdData.TF201Bit2,         "TF201Bit2/I");

  // TOF information
  mQATree->Branch("tofStartTime",      &mMtdData.tofStartTime,      "tofStartTime/I");


  // Tracks
  mQATree->Branch("nGoodTrack",        &mMtdData.nGoodTrack,         "nGoodTrack/I");
  mQATree->Branch("trkPt",             &mMtdData.trkPt,              "trkPt[nGoodTrack]/D");
  mQATree->Branch("trkEta",            &mMtdData.trkEta,             "trkEta[nGoodTrack]/D");
  mQATree->Branch("trkPhi",            &mMtdData.trkPhi,             "trkPhi[nGoodTrack]/D");
  mQATree->Branch("trkDca",            &mMtdData.trkDca,             "trkDca[nGoodTrack]/D");
  mQATree->Branch("trkNHitsFit",       &mMtdData.trkNHitsFit,        "trkNHitsFit[nGoodTrack]/I");
  mQATree->Branch("trkNHitsDedx",      &mMtdData.trkNHitsDedx,       "trkNHitsDedx[nGoodTrack]/I");
  mQATree->Branch("trkNsigmaPi",       &mMtdData.trkNsigmaPi,        "trkNsigmaPi[nGoodTrack]/D");
  mQATree->Branch("trkDedx",           &mMtdData.trkDedx,            "trkDedx[nGoodTrack]/D");
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
  mQATree->Branch("isMtdTrig",         &mMtdData.isMtdTrig,         "isMtdTrig[nMtdHits]/O");
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
  mQATree->Branch("mtdMatchTrkDca",    &mMtdData.mtdMatchTrkDca,    "mtdMatchTrkDca[nMtdHits]/D");
  mQATree->Branch("mtdMatchTrCharge",  &mMtdData.mtdMatchTrCharge,  "mtdMatchTrCharge[nMtdHits]/I");
  mQATree->Branch("mtdMatchTrkPhi",    &mMtdData.mtdMatchTrkPhi,    "mtdMatchTrkPhi[nMtdHits]/D");
  mQATree->Branch("mtdMatchTrkEta",    &mMtdData.mtdMatchTrkEta,    "mtdMatchTrkEta[nMtdHits]/D");
  mQATree->Branch("mtdMatchTrkNsigmaPi",&mMtdData.mtdMatchTrkNsigmaPi,"mtdMatchTrkNsigmaPi[nMtdHits]/D");
  mQATree->Branch("mtdMatchTrkTofHit", &mMtdData.mtdMatchTrkTofHit, "mtdMatchTrkTofHit[nMtdHits]/O");

  return;
}

//_____________________________________________________________________________
void StMtdQAMaker::printConfig()
{
  const char *decision[2] = {"no","yes"};
  const char *runtype[2] = {"Physics","Cosmic"};
  const char *vtxmode[2] = {"default","closest to VPD"};
  printf("=== Configuration for StMtdQAMaker ===\n");
  printf("Data type: %s\n",runtype[mIsCosmic]);
  printf("Use vertex that is %s\n",vtxmode[mVertexMode]);
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
  printf("Match to TOF: %s\n",decision[mMatchToTof]);
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
Bool_t StMtdQAMaker::isValidTrack(const StMuTrack *track) const 
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
  if(!mIsCosmic && track->dcaGlobal().mag()>mMaxDca) return kFALSE;
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
//// $Id: StMtdQAMaker.cxx,v 1.17 2018/02/20 19:46:48 marr Exp $
//// $Log: StMtdQAMaker.cxx,v $
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
