#include <iostream>
#include <math.h>
#include <vector>
#include <stdlib.h>
#include <iterator>
#include <bitset>

#include "TH1F.h"

#include "PhysicalConstants.h"
#include "StMessMgr.h"

#include "StEvent.h"
#include "StTriggerData.h"
#include "StTrack.h"
#include "StMtdCollection.h"
#include "StMtdHit.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuMtdHit.h"

#include "tables/St_mtdModuleToQTmap_Table.h"
#include "tables/St_mtdQTSlewingCorr_Table.h"
#include "tables/St_trgOfflineFilter_Table.h"
#include "StMtdTrackingMaskMaker.h"

ClassImp(StMtdTrackingMaskMaker)

//_____________________________________________________________________________
StMtdTrackingMaskMaker::StMtdTrackingMaskMaker(const Char_t *name) : StMaker(name),
  mStEvent(NULL), mMuDst(NULL), mIsDiMuon(kFALSE), mTrigData(NULL),
  mTpcSectorsForTracking(0)
{
  // default constructor

  mSaveHistos               = false;
  mhEventStat               = NULL;
  mhNQTsignals              = NULL;
  mhNMIXsignals             = NULL;
  mhNMuons                  = NULL;
  mhNMtdHits                = NULL;  
  mhNTrigMtdHits            = NULL;
  mhNTpcSectorForTracking   = NULL;
}
 
//_____________________________________________________________________________
StMtdTrackingMaskMaker::~StMtdTrackingMaskMaker()
{
  // default destructor
}

//_____________________________________________________________________________
Int_t StMtdTrackingMaskMaker::Init()
{
  // book histograms
  bookHistos();

  return kStOK;
}

//_____________________________________________________________________________
Int_t StMtdTrackingMaskMaker::InitRun(const Int_t runNumber)
{
  // initialize trigger Ids for di-muon triggers
  LOG_INFO << "Retrieving trigger ID from database ..." << endm;
   St_trgOfflineFilter* flaggedTrgs =
     (St_trgOfflineFilter *) GetDataBase("Calibrations/trg/trgOfflineFilter");
   if (!flaggedTrgs) 
     {
       LOG_ERROR << "Could not find Calibrations/trg/trgOfflineFilter in database" << endm;
       return kStErr;
     }

   trgOfflineFilter_st* flaggedTrg = flaggedTrgs->GetTable();
   for (long j = 0;j < flaggedTrgs->GetNRows(); j++, flaggedTrg++) 
     {
       int trigid = flaggedTrg->trigid;
       if (0<trigid && trigid<1e9) mTriggerIDs.push_back(trigid);
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

  for(int i=0; i<gMtdNBacklegs; i++)
    {
      for(int j=0; j<gMtdNModules; j++)
	{
	  int index = i*5 + j;
	  int qt = mtdModuleToQTtable->qtBoardId[index];
	  int channel = mtdModuleToQTtable->qtChannelId[index];
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

  return kStOK;
}

//_____________________________________________________________________________
void StMtdTrackingMaskMaker::Clear(Option_t *option)
{
  // initialize output bit to 0 for each event
  mTpcSectorsForTracking = 0; 
  mFiredSectors.clear();

  memset(mTrigQTpos,-1, sizeof(mTrigQTpos));
  mIsDiMuon = kFALSE;

  SetAttr("TpcSectorsByMtd",mTpcSectorsForTracking);
}

//_____________________________________________________________________________
Int_t StMtdTrackingMaskMaker::Make()
{

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

  SetAttr("TpcSectorsByMtd",mTpcSectorsForTracking);


  if(mStEvent)
    {
      LOG_INFO << "Is di-muon event: " << mIsDiMuon << endm;
      LOG_INFO << "Tracking mask by MTD: " << (bitset<24>)mTpcSectorsForTracking << endm;  
    }

  return iret;
}

//_____________________________________________________________________________
Int_t StMtdTrackingMaskMaker::processStEvent()
{
  mhEventStat->Fill(0.5);

  // check trigger id
  for(unsigned int j=0; j<mTriggerIDs.size(); j++)
    {
      if(mStEvent->triggerIdCollection()->nominal()->isTrigger(mTriggerIDs[j]))
	{
	  mIsDiMuon = kTRUE;
	  break;
	}
    }
  if(!mIsDiMuon) return kStOK;
  mhEventStat->Fill(1.5);

  // trigger data
  mTrigData = (StTriggerData*)mStEvent->triggerData();
  if(!mTrigData) return kStWarn;
  processTriggerData();

  // MTD hits
  StMtdCollection *mtdCollection = mStEvent->mtdCollection();
  if(!mtdCollection) 
    {
      LOG_WARN << "No mtd collection available in StEvent ... " << endm;
      return kStWarn;
    }
  StSPtrVecMtdHit& mtdHits = mtdCollection->mtdHits();
  int nMtdHits = mtdHits.size();
  mhNMtdHits->Fill(nMtdHits);
  int nTrigMtdHits = 0;

  for(int i=0; i<nMtdHits; i++)
    {
      StMtdHit *hit = mtdHits[i];
      if(!hit) continue;
      if(!isMtdHitFiredTrigger(hit)) continue;
      nTrigMtdHits++;

      // depending on the hit position, find the two or four TPC
      // sectors that most likely hosts the corresponding tracks
      int backleg = hit->backleg();
      int module  = hit->module();
      int cell    = hit->cell();
      double hit_phi = getMtdHitGlobalPhi(backleg, module, cell);
      findTpcSectorsForTracking(hit_phi, module);
    }
  determineTpcTrackingMask();
  mhNTrigMtdHits->Fill(nTrigMtdHits);
  return kStOK;
}


//_____________________________________________________________________________
Int_t StMtdTrackingMaskMaker::processMuDst()
{
  mhEventStat->Fill(0.5);

  // check trigger id
  for(unsigned int j=0; j<mTriggerIDs.size(); j++)
    {
      if(mMuDst->event()->triggerIdCollection().nominal().isTrigger(mTriggerIDs[j]))
	{
	  mIsDiMuon = kTRUE;
	  break;
	}
    }
  if(!mIsDiMuon) return kStOK;
  mhEventStat->Fill(1.5);

  // trigger data
  mTrigData = const_cast<StTriggerData*>(mMuDst->event()->triggerData());
  if(!mTrigData) return kStWarn;
  processTriggerData();

  // MTD hits
  int nMtdHits = mMuDst->numberOfMTDHit();
  mhNMtdHits->Fill(nMtdHits);
  int nTrigMtdHits = 0;

  for(int i=0; i<nMtdHits; i++)
    {
      StMuMtdHit *hit = mMuDst->mtdHit(i);
      if(!hit) continue;
      if(!isMtdHitFiredTrigger(hit)) continue;
      nTrigMtdHits++;

      // depending on the hit position, find the two or four TPC
      // sectors that most likely hosts the corresponding tracks
      int backleg = hit->backleg();
      int module  = hit->module();
      int cell    = hit->cell();
      double hit_phi = getMtdHitGlobalPhi(backleg, module, cell);
      findTpcSectorsForTracking(hit_phi, module);
    }
  determineTpcTrackingMask();
  mhNTrigMtdHits->Fill(nTrigMtdHits);
  return kStOK;
}

//_____________________________________________________________________________
void StMtdTrackingMaskMaker::processTriggerData()
{
  ///
  /// De-code the trigger information to determine
  /// which MTD trigger hits/patches actually fire the trigger
  ///

  // TF201 decision
  unsigned short decision = mTrigData->dsmTF201Ch(0);

  // MT101 informaiton (QA purpose)
  int mix_tacsum[4][2];
  int mix_id[4][2];
  int nMixSignal = 0;
  for(int i = 0; i < 4; i++)
    {
      mix_tacsum[i][0] = (mTrigData->mtdDsmAtCh(3*i,0)) + ((mTrigData->mtdDsmAtCh(3*i+1,0)&0x3)<<8);
      mix_id[i][0]     = (mTrigData->mtdDsmAtCh(3*i+1,0)&0xc)>>2;
      mix_tacsum[i][1] = (mTrigData->mtdDsmAtCh(3*i+1,0)>>4) + ((mTrigData->mtdDsmAtCh(3*i+2,0)&0x3f)<<4);
      mix_id[i][1]     = (mTrigData->mtdDsmAtCh(3*i+2,0)&0xc0)>>6;

      for(int j=0; j<2; j++)
	{
	  if(mix_tacsum[i][j]>0) nMixSignal ++;
	}
    }
  mhNMIXsignals->Fill(nMixSignal);

  // QT
  int mxq_tacsum[4][2];
  int mxq_tacsum_pos[4][2];
  for(int i=0; i<4; i++)
    {
      for(int j=0; j<2; j++)
	{
	  mxq_tacsum[i][j] = 0;
	  mxq_tacsum_pos[i][j] = -1;
	}
    }

  // extract tac information for each QT board
  int mtdQTtac[4][16];
  int mtdQTadc[4][16];
  for(int i=0; i<32; i++)
    {
      int type = (i/4)%2;
      if(type==1)
	{
	  mtdQTtac[0][i-i/4*2-2] = mTrigData->mtdAtAddress(i,0);
	  mtdQTtac[1][i-i/4*2-2] = mTrigData->mtdgemAtAddress(i,0);
	  mtdQTtac[2][i-i/4*2-2] = mTrigData->mtd3AtAddress(i,0);
	  mtdQTtac[3][i-i/4*2-2] = mTrigData->mtd4AtAddress(i,0);
	}
      else
	{
	  mtdQTadc[0][i-i/4*2] = mTrigData->mtdAtAddress(i,0);
	  mtdQTadc[1][i-i/4*2] = mTrigData->mtdgemAtAddress(i,0);
	  mtdQTadc[2][i-i/4*2] = mTrigData->mtd3AtAddress(i,0);
	  mtdQTadc[3][i-i/4*2] = mTrigData->mtd4AtAddress(i,0);
	}
    }

  // In each QT board, find the two signals with
  // largest TacSum values
  int nQtSignal = 0;
  int j[2], a[2];
  for(int im=0; im<4; im++)
    {
      for(int i=0; i<8; i++)
	{
	  // slewing correction
	  for(int k=0; k<2; k++)
	    {
	      j[k] = mtdQTtac[im][i*2+k];
	      a[k] = mtdQTadc[im][i*2+k];

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

	  if(j[0]<100 || j[1]<100) continue;
	  if(abs(j[0]-j[1])>600) continue;
	  nQtSignal++;
	  
	  // position correction
	  int module = mQTtoModule[im][i];
	  int sumTac = int( j[0] + j[1] + abs(module-3)*1./8 * (j[0]-j[1]) );

	  if(mxq_tacsum[im][0] < sumTac)
	    {
	      mxq_tacsum[im][1] = mxq_tacsum[im][0];
	      mxq_tacsum[im][0] = sumTac;

	      mxq_tacsum_pos[im][1] = mxq_tacsum_pos[im][0];
	      mxq_tacsum_pos[im][0] = i+1;
	    }
	  else if (mxq_tacsum[im][1] < sumTac)
	    {
	      mxq_tacsum[im][1]  = sumTac;
	      mxq_tacsum_pos[im][1] = i+1;
	    }
	}
    }
  mhNQTsignals->Fill(nQtSignal);
  
  int nMuon = 0;
  for(int i = 0; i < 4; i++)
    {
      for(int j=0; j<2; j++)
	{
	  if((decision>>(i*2+j+4))&0x1)
	    {
	      nMuon ++;
	      mTrigQTpos[i][j] = mxq_tacsum_pos[i][j];
	    }
	}
    }
  mhNMuons->Fill(nMuon);

  LOG_DEBUG << "# of muon candidates = " << nMuon << endm;
}

//_____________________________________________________________________________
bool StMtdTrackingMaskMaker::isMtdHitFiredTrigger(const StMtdHit *hit)
{
  ///
  /// check if a MTD hit fired the trigger. This is done
  /// by looking at whether the corresponding QT board
  /// sends out the trigger signal
  ///

  return isQTFiredTrigger( mModuleToQT[hit->backleg()-1][hit->module()-1], mModuleToQTPos[hit->backleg()-1][hit->module()-1]);
}

//_____________________________________________________________________________
bool StMtdTrackingMaskMaker::isMtdHitFiredTrigger(const StMuMtdHit *hit)
{
  ///
  /// check if a MTD hit fired the trigger. This is done
  /// by looking at whether the corresponding QT board
  /// sends out the trigger signal
  ///

  return isQTFiredTrigger( mModuleToQT[hit->backleg()-1][hit->module()-1], mModuleToQTPos[hit->backleg()-1][hit->module()-1]);
}

//_____________________________________________________________________________
bool StMtdTrackingMaskMaker::isQTFiredTrigger(const int qt, const int pos)
{
  ///
  /// check if the QT board sends out the trigger signal
  ///

  return (pos==mTrigQTpos[qt-1][0] || pos==mTrigQTpos[qt-1][1]);
}

//_____________________________________________________________________________
void StMtdTrackingMaskMaker::determineTpcTrackingMask()
{
  // remove duplicated TPC sectors
  sort(mFiredSectors.begin(),mFiredSectors.end());
  mFiredSectors.erase(unique(mFiredSectors.begin(),mFiredSectors.end()),mFiredSectors.end());

  mhNTpcSectorForTracking->Fill(mFiredSectors.size());
  
  // 24-bit mask for TPC tracking
  for(unsigned int i=0; i<mFiredSectors.size(); i++)
    {
      int bit = mFiredSectors[i] - 1;
      mTpcSectorsForTracking |= (1U << bit);
    }
  LOG_DEBUG << "Output TPC mask = " << (bitset<24>) mTpcSectorsForTracking << endm;
}

//_____________________________________________________________________________
void StMtdTrackingMaskMaker::findTpcSectorsForTracking(const double hit_phi, const int hit_module)
{
  ///
  /// Find the corresponding TPC sector for a given MTD hit
  /// For hits in module 1, only tracking TPC sectors on east
  /// For hits in module 5, only tracking TPC sectors on west
  /// For hits in module 2-4, tracking TPC sectors both on east and west
  ///

  IntVec westTpc, eastTpc;
  westTpc.clear();
  eastTpc.clear();

  if(hit_module<5) eastTpc = findEastTpcSectors(hit_phi);
  if(hit_module>1) westTpc = findWestTpcSectors(hit_phi);

  for(unsigned int i=0; i<eastTpc.size(); i++)
    mFiredSectors.push_back(eastTpc[i]);

  for(unsigned int i=0; i<westTpc.size(); i++)
    mFiredSectors.push_back(westTpc[i]);
}

//_____________________________________________________________________________
vector<int> StMtdTrackingMaskMaker::findWestTpcSectors(const double hit_phi)
{
  ///
  /// Given the azimuthal angle of a MTD hit, find the two spatially closest TPC
  /// sectors on the west side of STAR
  ///
 
  IntVec sectors;
  sectors.clear();

  double tpc_sector_width = pi/6.;

  int tpc_sector_1 = 3 - int(floor(hit_phi/tpc_sector_width));
  if(tpc_sector_1<1) tpc_sector_1 += 12;

  int tpc_sector_2 = tpc_sector_1 - 1;
  if(tpc_sector_2<1) tpc_sector_2 += 12;
  
  sectors.push_back(tpc_sector_1);
  sectors.push_back(tpc_sector_2);

  LOG_DEBUG << "For hit at phi = " << hit_phi/pi*180 << ", west TPC sectors are " 
	    << tpc_sector_1 << " and " << tpc_sector_2 << endm;
  return sectors;
}

//_____________________________________________________________________________
vector<int> StMtdTrackingMaskMaker::findEastTpcSectors(const double hit_phi)
{
  ///
  /// Given the azimuthal angle of a MTD hit, find the two spatially closest TPC
  /// sectors on the east side of STAR
  ///

  IntVec sectors;
  sectors.clear();

  double tpc_sector_width = pi/6.;

  int tpc_sector_1 = int(floor(hit_phi/tpc_sector_width))+21;
  if(tpc_sector_1>24) tpc_sector_1 -= 12;

  int tpc_sector_2 = tpc_sector_1 + 1;
  if(tpc_sector_2>24) tpc_sector_2 -= 12;
  
  sectors.push_back(tpc_sector_1);
  sectors.push_back(tpc_sector_2);

  LOG_DEBUG << "For hit at phi = " << hit_phi/pi*180  << ", east TPC sectors are " 
	    << tpc_sector_1 << " and " << tpc_sector_2 << endm;

  return sectors;
}

//_____________________________________________________________________________
double StMtdTrackingMaskMaker::getMtdHitGlobalPhi(const int backleg, const int module, const int cell)
{
  ///
  /// Approximate phi center of a MTD hit
  ///

  double backlegPhiCen = gMtdFirstBacklegPhiCenter + (backleg-1) * (gMtdBacklegPhiWidth+gMtdBacklegPhiGap);
  if(backlegPhiCen>2*pi) backlegPhiCen -= 2*pi;
  double stripPhiCen = 0;
  if(module>0 && module<4)
    {
      stripPhiCen = backlegPhiCen - (gMtdNChannels/4.-0.5-cell)*(gMtdCellWidth+gMtdCellGap)/gMtdMinRadius;
    }
  else
    {
      stripPhiCen = backlegPhiCen + (gMtdNChannels/4.-0.5-cell)*(gMtdCellWidth+gMtdCellGap)/gMtdMinRadius;
    }
  return rotatePhi(stripPhiCen);
}

//_____________________________________________________________________________
double StMtdTrackingMaskMaker::rotatePhi(const double phi)
{
  double outPhi = phi;
  while(outPhi<0) outPhi += 2*pi;
  while(outPhi>2*pi) outPhi -= 2*pi;
  return outPhi;
}


//_____________________________________________________________________________
void StMtdTrackingMaskMaker::bookHistos()
{

  mhEventStat = new TH1F("hEventStat","Event statistics",5,0,5);
  mhEventStat->GetXaxis()->SetBinLabel(1,"All events");
  mhEventStat->GetXaxis()->SetBinLabel(2,"Good trigger");

  mhNQTsignals = new TH1F("hNQTsignals","Number of QT signals per event;N",10,0,10);

  mhNMIXsignals = new TH1F("hNMIXsignals","Number of MT101 signals per event;N",10,0,10);

  mhNMuons = new TH1F("hNMuons","Number of TF201 signals per event;N",10,0,10);

  mhNMtdHits = new TH1F("hNMtdHits","Number of MTD hits per event;N",50,0,50);

  mhNTrigMtdHits = new TH1F("hNTrigMtdHits","Number of triggering MTD hits per event;N",10,0,10);

  mhNTpcSectorForTracking = new TH1F("hNTpcSectorForTracking","Number of TPC sectors for tracking per event;N",20,0,20);

  if(mSaveHistos)
    {
      AddHist(mhEventStat);
      AddHist(mhNQTsignals); 
      AddHist(mhNMIXsignals); 
      AddHist(mhNMuons); 
      AddHist(mhNMtdHits); 
      AddHist(mhNTrigMtdHits); 
      AddHist(mhNTpcSectorForTracking); 
    }
}


// $Id: StMtdTrackingMaskMaker.cxx,v 1.4 2016/07/27 15:24:30 marr Exp $
// $Log: StMtdTrackingMaskMaker.cxx,v $
// Revision 1.4  2016/07/27 15:24:30  marr
// Fix coverity check: initialization of data members
//
// Revision 1.3  2015/05/01 21:37:20  marr
// Apply online slewing correction and position correction to QT data to make
// sure the correct trigger patches are found offline.
//
// Revision 1.2  2015/04/23 21:09:12  marr
// Add print-out in reconstruction
//
// Revision 1.1  2015/04/07 14:10:37  jeromel
// First version of StMtdEvtFilterMaker - R.Ma - review closed 2015/04/06
//
//
