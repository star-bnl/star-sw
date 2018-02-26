/*******************************************************************
 *
 * $Id: StVpdAnalysisMaker.cxx,v 1.4 2018/02/26 23:26:51 smirnovd Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: Vpd analysis Maker to do the calibration for pVPD 
 *              hit pattern, VzVpd, Tdiff, Tstart
 *
 *******************************************************************/
#include <iostream>
#include "TFile.h"
#include "TProfile.h"
#include "TNtuple.h"
#include "TH1.h"
#include "TH2.h"
#include "TF1.h"

#include "StEvent.h"
#include "StEventTypes.h"
#include "Stypes.h"
#include "StMessMgr.h"
#include "StThreeVectorD.hh"
#include "phys_constants.h"
#include "tables/St_tofTotCorr_Table.h"
#include "tables/St_tofZCorr_Table.h"
#include "tables/St_tofTOffset_Table.h"
#include "tables/St_tofPhaseOffset_Table.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"

#include "StVpdAnalysisMaker.h"

#include "StMessMgr.h"
#include "StMemoryInfo.hh"
#include "StTimer.hh"


//_____________________________________________________________________________
StVpdAnalysisMaker::StVpdAnalysisMaker(const char *name) : StMaker(name)
{
  /// default constructor
  /// Reset the calibration parameters
  mTofINLCorr = 0;
  mDaqMap = 0;
  mVertexZ = -9999.;

  mEvent = 0;
  mMuDst = 0;

  setVPDHitsCut(1,1);
  mSlewingCorr = kTRUE;
  mValidCalibPar = kFALSE;

  resetCalibPars();
  resetVpdPars();
}

//_____________________________________________________________________________
StVpdAnalysisMaker::~StVpdAnalysisMaker()
{ /* noop */ }

//_____________________________________________________________________________
void StVpdAnalysisMaker::resetCalibPars()
{
  for(int i=0;i<2*mNVPD;i++) {
    for(int j=0;j<mNBinMax;j++) {
      mVPDTotEdge[i][j] = 0.0;
      mVPDTotCorr[i][j] = 0.0;
    }
    mVPDTZero[i] = 0.0;
    mVPDLeTime[i] = 0.0;
    mVPDTot[i] = 0.0;
  }
  mPhaseOffset8 = 0.;
}

//_____________________________________________________________________________
void StVpdAnalysisMaker::resetVpdPars()
{
  mTSumEast = 0.;
  mTSumWest = 0.;
  mVPDHitPatternEast = 0;
  mVPDHitPatternWest = 0;
  mNEast = 0;
  mNWest = 0;
  mVPDVtxZ = -9999.;
  mTDiff   = -9999.;
  mTStart  = -9999.;
  mVertexZ = -9999.;
}

//____________________________________________________________________________
Int_t StVpdAnalysisMaker::Init()
{
  return kStOK;
}

//____________________________________________________________________________
Int_t StVpdAnalysisMaker::InitRun(int runnumber)
{
  // tof run configurations
  mYear8 = (runnumber>9000000&&runnumber<10000000);

  mTofINLCorr = new StTofINLCorr();
  mDaqMap = new StTofrDaqMap();
  if(mYear8) {
    mTofINLCorr->setNValidTrays(mNValidTrays_Run8);
    mTofINLCorr->initFromDbase(this);
    gMessMgr->Info("","OS") << " Initialize INL table for run 8 ... " << endm;

    mDaqMap->setNValidTrays(mNValidTrays_Run8);
    mDaqMap->initFromDbaseGeneral(this);
    gMessMgr->Info("","OS") << " Initialize Daq map for run 8 ... " << endm;
  }


  Int_t val = kStOK;
  val = initParameters(runnumber);
  if(val==kStOK) {
    mValidCalibPar = kTRUE;
  } else {
    mValidCalibPar = kFALSE;
  }

  if(mValidCalibPar) {
    gMessMgr->Info(" ==> Good! Valid cali parameters! ","OS");
  } else {
    gMessMgr->Info(" ==> No valid cali parameters! ","OS");
  }

  return kStOK;

}

//_____________________________________________________________________________
Int_t StVpdAnalysisMaker::initParameters(int runnumber)
{
  /// initialize the calibrations parameters from dbase
  /// read in and check the size
  LOG_INFO << "   -- retrieving run parameters from Calibrations_tof" << endm;
  TDataSet *mDbDataSet = GetDataBase("Calibrations/tof/tofTotCorr");
  if (!mDbDataSet){
    gMessMgr->Error("unable to get TOF run parameters","OS");
    return kStErr;
  }
  
  if(mYear8) {
    gMessMgr->Info("","OS") << "     loading parameters for Run VIII" << endm;

    // read tofTotCorr table
    St_tofTotCorr* tofTotCorr = static_cast<St_tofTotCorr*>(mDbDataSet->Find("tofTotCorr"));
    if(!tofTotCorr) {
      gMessMgr->Error("unable to get tof TotCorr table parameters","OS");
      //    assert(tofTotCorr);
      return kStErr;
    }
    tofTotCorr_st* totCorr = static_cast<tofTotCorr_st*>(tofTotCorr->GetArray());
    Int_t numRows = tofTotCorr->GetNRows();

    if(numRows!=mNTray8*mNTDIG+mNVPD*2) {
      gMessMgr->Warning("","OS") << " Mis-matched number of rows in tofTotCorr table! " << endm;
      //      return kStErr;
    }

    if(Debug()) gMessMgr->Info("","OS") << " Number of rows read in: " << numRows << " for ToT correction" << endm;

    for (Int_t i=0;i<mNTray8*mNTDIG+mNVPD*2;i++) {
      short trayId = totCorr[i].trayId;
      short moduleId = totCorr[i].moduleId;
      short boardId = (moduleId-1)/4+1;      // used for trays
      short cellId = totCorr[i].cellId;      // used for vpds

      int index = (trayId-120-1)*mNVPD+(cellId-1);   // used for vpd index

      if(Debug()) gMessMgr->Info("","OS") << " tray " << trayId << " board " << boardId << " cell " << cellId << endm;
      if(trayId!=mWestVpdTrayId&&trayId!=mEastVpdTrayId) continue;
      for(Int_t j=0;j<mNBinMax;j++) {
	if(trayId==mWestVpdTrayId||trayId==mEastVpdTrayId) { // upVPD east west
	  mVPDTotEdge[index][j] = totCorr[i].tot[j];
	  mVPDTotCorr[index][j] = totCorr[i].corr[j];
	} 
      } // end j 0->mNBinMax
    } // end i 0->numRows

    // read tofPhaseOffset table
    St_tofPhaseOffset* tofPhaseOffset = static_cast<St_tofPhaseOffset*>(mDbDataSet->Find("tofPhaseOffset"));
    if(!tofPhaseOffset) {
      gMessMgr->Error("unable to get tof PhaseOffset table parameters","OS");
      //    assert(tofPhaseOffset);
      return kStErr;
    }
    tofPhaseOffset_st* tPhaseDiff = static_cast<tofPhaseOffset_st*>(tofPhaseOffset->GetArray());

    mPhaseOffset8 = tPhaseDiff[0].T0[0] + tPhaseDiff[0].T0[1]; // run 8, only e/w difference, to double precision
    if(Debug()) gMessMgr->Info("","OS") << " PhaseOffset = " << mPhaseOffset8 << endm;
  }

  return kStOK;
}
//____________________________________________________________________________
Int_t StVpdAnalysisMaker::FinishRun(int runnumber)
{
  LOG_INFO << "StVpdAnalysisMaker -- cleaning up (FinishRun)" << endm;

  if(mDaqMap) delete mDaqMap;
  mDaqMap = 0;

  if(mTofINLCorr) delete mTofINLCorr;
  mTofINLCorr = 0;
  
  return kStOK;
}

//_____________________________________________________________________________
Int_t StVpdAnalysisMaker::Finish()
{
  return kStOK;
}

//_____________________________________________________________________________
Int_t StVpdAnalysisMaker::Make()
{
  gMessMgr->Info(" StVpdAnalysisMaker::Maker: starting ...","OS");

  resetVpdPars();

  Int_t iret = kStOK;
  if(mYear8) {
    iret = processEventYear8();
  }
  return kStOK;
}

//____________________________________________________________________________
Int_t StVpdAnalysisMaker::processEventYear8()
{
  mMuDstMaker = (StMuDstMaker *)GetMaker("MuDst");
  if(!mMuDstMaker) {
    LOG_INFO << " StVpdAnalysisMaker -- No MuDstMaker ... bye-bye" << endm;
    return kStOK;
  }

  mMuDst = mMuDstMaker->muDst();
  if(!mMuDst) {
    LOG_INFO << " StVpdAnalysisMaker -- No MuDst ... bye-bye" << endm;
    return kStOK;
  }

  mMuEvent = mMuDst->event();
  if(mMuEvent) {
    mVertexZ = mMuEvent->primaryVertexPosition().z();  //! TPC vertex Z
    if( fabs(mMuEvent->primaryVertexPosition().x())<1.e-4 &&
        fabs(mMuEvent->primaryVertexPosition().y())<1.e-4 &&
        fabs(mMuEvent->primaryVertexPosition().z())<1.e-4 )
    mVertexZ = -9999;
  }
  ///
  /// Read in Vpd data from TofRawData branch
  ///
  mEvent = new StEvent();
  mTofCollection = new StTofCollection();
  mEvent->setTofCollection(mTofCollection);
  int nTofRawData = mMuDst->numberOfTofRawData();
  for(int i=0;i<nTofRawData;i++) {
    StTofRawData *aRawData;
    StTofRawData *aMuRawData = (StTofRawData *)mMuDst->tofRawData(i);
    if(aMuRawData) {
      unsigned short tray = aMuRawData->tray();
      if(tray!=mWestVpdTrayId&&tray!=mEastVpdTrayId) continue;
      unsigned short leteFlag = aMuRawData->leteFlag();
      unsigned short channel = aMuRawData->channel();  
      unsigned int tdc = aMuRawData->tdc();
      unsigned int triggertime = aMuRawData->triggertime();
      unsigned short quality = aMuRawData->quality();
      aRawData = new StTofRawData(leteFlag,tray,channel,tdc,triggertime,quality);
    }
    mTofCollection->addRawData(aRawData);
  }

  mSortTofRawData = new StSortTofRawData(mTofCollection, mDaqMap);

  //-------------------------------------------------
  // upVPD calibration and calculate the start timing
  //-------------------------------------------------
  for(int i=0;i<2*mNVPD;i++) {
    mVPDLeTime[i] = -999.;
    mVPDTot[i] = -999.;
  }
  for(int ivpd=0;ivpd<2;ivpd++) { // west and east sides
    int trayId = (ivpd==0) ? mWestVpdTrayId : mEastVpdTrayId;
    int ewId = trayId - 120; //! 1: west, 2: east

    IntVec validtube = mSortTofRawData->GetValidChannel(trayId);
    if(Debug()) gMessMgr->Info("","OS") << " Number of fired hits on tray(vpd) " << trayId << " = " << validtube.size() << endm;

    if(!validtube.size()) continue;
    for(int i=0;i<mNVPD;i++) {
      int tubeId = i+1;
      int lechan = (ewId==1) ? mDaqMap->WestPMT2TDIGLeChan(tubeId) : mDaqMap->EastPMT2TDIGLeChan(tubeId);
      int techan = (ewId==1) ? mDaqMap->WestPMT2TDIGTeChan(tubeId) : mDaqMap->EastPMT2TDIGTeChan(tubeId);
      IntVec leTdc = mSortTofRawData->GetLeadingTdc(trayId, lechan, kTRUE);
      IntVec teTdc = mSortTofRawData->GetTrailingTdc(trayId, lechan, kTRUE);

      if(!leTdc.size() || !teTdc.size()) continue;

      int letdc = leTdc[0];
      int bin = (int)letdc&0x3ff;
      double letdc_f = letdc + mTofINLCorr->getVpdINLCorr(ewId, lechan, bin);
      double letime = letdc_f*VHRBIN2PS / 1000.;

      int tetdc = teTdc[0];
      bin = (int)tetdc&0x3ff;
      double tetdc_f = tetdc + mTofINLCorr->getVpdINLCorr(ewId, techan, bin);
      double tetime = tetdc_f*VHRBIN2PS / 1000.;

      mVPDLeTime[(trayId-121)*mNVPD+(tubeId-1)] = letime;
      mVPDTot[(trayId-121)*mNVPD+(tubeId-1)] = tetime - letime;
    }
  }
  tsum(mVPDTot, mVPDLeTime);

  gMessMgr->Info("","OS") << " NWest = " << mNWest << " NEast = " << mNEast << " TdcSum West = " << mTSumWest << " East = " << mTSumEast << endm;

  /// Fill vpd information in StTofCollection
  mTofCollection->setVpdEast(mVPDHitPatternEast);
  mTofCollection->setVpdWest(mVPDHitPatternWest);
  mTofCollection->setTdiff(mTDiff);
  mTofCollection->setVzVpd(mVPDVtxZ);
  mTofCollection->setTstart(mTStart);

  mNWest = mTofCollection->numberOfVpdWest();
  mNEast = mTofCollection->numberOfVpdEast();

  gMessMgr->Info("","OS") << " TofCollection: NWest = " << mVPDHitPatternWest << " NEast = " << mVPDHitPatternEast << endm;
  gMessMgr->Info("","OS") << "Tdiff = " << mTDiff <<" vpd vz = " << mVPDVtxZ << " tstart = " << mTStart << endm;
 
  return kStOK;
}

//_____________________________________________________________________________
void StVpdAnalysisMaker::tsum(const Double_t *tot, const Double_t *time)
{
  /// Sum vpd informations
  mTSumEast = 0.;
  mTSumWest = 0.;
  mNEast = 0;
  mNWest = 0;
  mVPDHitPatternWest = 0;
  mVPDHitPatternEast = 0;

  // West
  for(int i=0;i<mNVPD;i++) {
    if( time[i]>0. && tot[i]>0. ) {
      // west no offset
      if(mSlewingCorr) {   // a switch
	int ibin = -1;
	for(int j=0;j<mNBinMax-1;j++) {
	  if(tot[i]>=mVPDTotEdge[i][j] && tot[i]<mVPDTotEdge[i][j+1]) {
	    ibin = j;
	    break;
	  }
	}
	if(ibin>=0&&ibin<mNBinMax) {
	  mNWest++;

	  double x1 = mVPDTotEdge[i][ibin];
	  double x2 = mVPDTotEdge[i][ibin+1];
	  double y1 = mVPDTotCorr[i][ibin];
	  double y2 = mVPDTotCorr[i][ibin+1];
	  double dcorr = y1 + (tot[i]-x1)*(y2-y1)/(x2-x1);

	  mVPDLeTime[i] = time[i] - dcorr;
	  mTSumWest += mVPDLeTime[i];

	  mVPDHitPatternWest |= 1<<i;
	}
      } else {
        gMessMgr->Info("","OS") << " Vpd West tube " << i+1 << " TOT out of range!" << endm;
        // out of range, remove this hit
      }
    }
      
    // East
    if( time[i+mNVPD]>0. && tot[i+mNVPD]>0. ) {
      double tmp = time[i+mNVPD] - mPhaseOffset8;
      while(tmp>TMAX) tmp -= TMAX;
      if(mSlewingCorr) {   // a switch
	int ibin = -1;
	for(int j=0;j<mNBinMax-1;j++) {
	  if(tot[i+mNVPD]>=mVPDTotEdge[i+mNVPD][j] && tot[i+mNVPD]<mVPDTotEdge[i+mNVPD][j+1]) {
	    ibin = j;
	    break;
	  }
	}
	if(ibin>=0&&ibin<mNBinMax) {
	  mNEast++;

	  double x1 = mVPDTotEdge[i+mNVPD][ibin];
	  double x2 = mVPDTotEdge[i+mNVPD][ibin+1];
	  double y1 = mVPDTotCorr[i+mNVPD][ibin];
	  double y2 = mVPDTotCorr[i+mNVPD][ibin+1];
	  double dcorr = y1 + (tot[i+mNVPD]-x1)*(y2-y1)/(x2-x1);

	  mVPDLeTime[i+mNVPD] = tmp - dcorr;
	  mTSumEast += mVPDLeTime[i+mNVPD];

	  mVPDHitPatternEast |= 1<<i;

	}
      } else {
        gMessMgr->Info("","OS") << " Vpd East tube " << i+1 << " TOT out of range!" << endm;
        // out of range, remove this hit
      }
    }
  }

  if ( mNEast>=mVPDEastHitsCut && mNWest>=mVPDWestHitsCut ) {
    mVPDVtxZ = (mTSumEast/mNEast - mTSumWest/mNWest)/2.*(C_C_LIGHT/1.e9);
    if(fabs(mVertexZ)<200.) {
      mTDiff = (mTSumEast/mNEast - mTSumWest/mNWest)/2. - mVertexZ/(C_C_LIGHT/1.e9);
      mTStart = (mTSumEast+mTSumWest-(mNEast-mNWest)*mVertexZ/(C_C_LIGHT/1.e9))/(mNEast+mNWest);;
    } else {
      LOG_INFO << " StVpdAnalysisMaker -- vtx z " << mVertexZ << " is out of range! " << endm;
      mTDiff = -9999.;
      mTStart = -9999.;
    }
  }
  
  return;
}

/*****************************************************************
 *
 * $Log: StVpdAnalysisMaker.cxx,v $
 * Revision 1.4  2018/02/26 23:26:51  smirnovd
 * StTof: Remove outdated ClassImp macro
 *
 * Revision 1.3  2018/02/26 23:13:21  smirnovd
 * Move embedded CVS log messages to the end of file
 *
 * Revision 1.2  2012/12/14 06:35:56  geurts
 * Changed global database calls to direct table access and/or removed deprecated database access code.
 *
 * Revision 1.1  2008/09/02 18:27:21  dongx
 * first release.
 * - Vpd analysis maker from MuDst to extract vz, Tstart, Tdiff etc.
 * - TPC primary vertex used for Tstart and Tdiff calculation
 *
 */
