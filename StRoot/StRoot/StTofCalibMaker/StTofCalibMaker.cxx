/*******************************************************************
 *
 * $Id: StTofCalibMaker.cxx,v 1.24 2018/02/26 23:26:50 smirnovd Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: - Tof Calibration Maker to do the calibration for pVPD 
 *              (start timing) , TOFp and TOFr
 *              - store into StTofHit
 *              - if no valid calibration parameters, store matched hits
 *
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
#include "StTofHit.h"
#include "StTofCell.h"
#include "StTofSlat.h"
#include "StTofData.h"
#include "StTofPidTraits.h"
#include "StEventTypes.h"
#include "Stypes.h"
#include "StMessMgr.h"
#include "StThreeVectorD.hh"
#include "StHelix.hh"
#include "StTrackGeometry.h"
#include "StEventUtilities/StuRefMult.hh"
#include "PhysicalConstants.h"
#include "phys_constants.h"
#include "StPhysicalHelixD.hh"
#include "tables/St_tofTzero_Table.h"
#include "tables/St_tofTACorr_Table.h"
#include "tables/St_tofCorrection_Table.h"
#include "tables/St_tofAdcRange_Table.h"
#include "tables/St_tofResolution_Table.h"

#include "tables/St_tofr5INLtable_Table.h"
#include "tables/St_tofTotCorr_Table.h"
#include "tables/St_tofZCorr_Table.h"

#include "tables/St_vertexSeed_Table.h"

#include "tables/St_tofTOffset_Table.h"
#include "tables/St_tofPhaseOffset_Table.h"

#include "StTofUtil/tofPathLength.hh"
#include "StTofUtil/StTofDataCollection.h"
#include "StTofUtil/StTofSlatCollection.h"
#include "StTofUtil/StTofCellCollection.h"
#include "StTofUtil/StTofHitCollection.h"
#include "StTofUtil/StTofGeometry.h"
#include "StTofCalibMaker.h"

#include "StMessMgr.h"
#include "StMemoryInfo.hh"
#include "StTimer.hh"


//_____________________________________________________________________________
StTofCalibMaker::StTofCalibMaker(const char *name) : StMaker(name)
{
  /// default constructor
  /// set the default parameters for TDC, ADC cut etc.
  /// Reset the calibration parameters
  setTDCLimits(20, 1500);   // TDC range
  setADCCut(300);          // TA correction adc cut
  setTDCWidth(0.05);      // 50 ps

  setPVPDADCLimits(30,1100);
  setPVPDTDCLimits(1,2000);
  setPVPDHitsCut(1,1);
  setOuterGeometry(true);

  mValidStartTime = kTRUE;
  mEastPVPDValid = kTRUE;
  mSlewingCorr = kTRUE;

  StThreeVectorD MomFstPt(0.,0.,9999.);
  StThreeVectorD origin(0.,0.,0.);
  mBeamHelix = new StPhysicalHelixD(MomFstPt,origin,0.5*tesla,1.);

  resetPars();
}

//_____________________________________________________________________________
StTofCalibMaker::~StTofCalibMaker()
{
  resetPars();
}

//_____________________________________________________________________________
void StTofCalibMaker::resetPars()
{
  for(int i=0;i<mNPar;i++) {
    mTofrZPar[i] = 0.;
    mTofpZPar[i] = 0.;
  }
  for(int i=0;i<mNTOFr;i++) {
    mTofrT0[i] = 0.;
  }
  for(int i=0;i<mNTOFr*mNPar;i++) {
    mTofrTAPar[i] = 0.0;
  }
  for(int i=0;i<mNTOFp;i++) {
    mTofpT0[i] = 0.;
  }
  for(int i=0;i<mNTOFp*mNPar;i++) {
    mTofpTAPar[i] = 0.0;
  }
  for(int i=0;i<mNPVPD*mNPar;i++) {
    mPVPDTAPar[i] = 0.0;
  }

  mValidCalibPar = kFALSE;

  /// ADC range, resolution of each channel;
  for(int i=0;i<mNTOFr;i++) {
    mTofrADCMin[i] = 0.;
    mTofrADCMax[i] = 1024.;
    mTofrRes[i] = 999.;
  }
  for(int i=0;i<mNTOFp;i++) {
    mTofpADCMin[i] = 0.;
    mTofpADCMax[i] = 1024.;
    mTofpRes[i] = 999.;
  }
  for(int i=0;i<mNPVPD;i++) {
    mPVPDRes[i] = 999.;
  }

  // run5
  for(int i=0;i<mTdigBoard;i++) {
    for(int j=0;j<mTdcOnBoard;j++) {
      for(int k=0;k<mTdcChannel;k++) {
	mINLtable[i][j][k] = 0.0;
      }
    }
  }

  for(int i=0;i<mNTOFr5;i++) {
    for(int j=0;j<mNBinMax;j++) {
      mTofr5TotEdge[i][j] = 0.0;
      mTofr5TotCorr[i][j] = 0.0;
      mTofr5ZEdge[i][j] = 0.0;
      mTofr5ZCorr[i][j] = 0.0;
    }
  }

  for(int i=0;i<mNPVPD;i++) {
    for(int j=0;j<mNBinMax;j++) {
      mPVPDTotEdge[i][j] = 0.0;
      mPVPDTotCorr[i][j] = 0.0;
    }
  }

  // run8
  for(int i=0;i<mNTray;i++) {
    for(int j=0;j<mNTDIG;j++) {
      for(int k=0;k<mNBinMax;k++) {
	mTofTotEdge[i][j][k] = 0.0;
	mTofTotCorr[i][j][k] = 0.0;
	mTofZEdge[i][j][k] = 0.0;
	mTofZCorr[i][j][k] = 0.0;
      }
    }
    for(int j=0;j<mNModule;j++) {
      for(int k=0;k<mNCell;k++) {
	mTofTZero[i][j][k] = 0.0;
      }
    }
  }
  for(int i=0;i<2*mNVPD;i++) {
    for(int j=0;j<mNBinMax;j++) {
      mVPDTotEdge[i][j] = 0.0;
      mVPDTotCorr[i][j] = 0.0;
    }
    mVPDTZero[i] = 0.0;
    mVPDLeTime[i] = 0.0;
    mVPDTot[i] = 0.0;
  }

  mTStart = -9999.;
  mTDiff = -9999.;
  mVPDVtxZ = -9999.;
  mProjVtxZ = -9999.;
  mVPDHitPatternEast = 0;
  mVPDHitPatternWest = 0;
}

//____________________________________________________________________________
Int_t StTofCalibMaker::Init()
{
  initFormulas();
  return kStOK;
}

//_____________________________________________________________________________
void StTofCalibMaker::initFormulas()
{
  /// define the calibration functions
  mTofrSlewing = new TF1("TofrSlewing", "[0]+[1]/sqrt(x)+[2]/x+[3]/sqrt(x)/x+[4]/x/x");
  // changed back
  mTofpSlewing = new TF1("TofpSlewing", "[0]+[1]/sqrt(x)+[2]/x+[3]/sqrt(x)/x+[4]/x/x");
  // Run 4, AuAu200GeV, Jiansong's calibration function -- removed later
  //  mTofpSlewing = new TF1("TofpSlewing","[0]+[1]*sqrt(x)+[2]*x+[3]*x*sqrt(x)");
  mTofrZCorr = new TF1("TofrZCorr", "pol7");
  //  mTofpZCorr = new TF1("TofpZCorr", "pol7");
  // Run 4, AuAu200GeV, Jiansong's calibration function
  mTofpZCorr = new TF1("TofpZCorr", "[0]+[1]*sqrt(x)+[2]*x+[3]*x*sqrt(x)");
  mPVPDSlewing = new TF1("pVPDSlewing","[0]+[1]/sqrt(x)+[2]/x+[3]*x");
}

//____________________________________________________________________________
Int_t StTofCalibMaker::InitRun(int runnumber)
{
  // tof run configurations

  mTofpGeom = new StTofGeometry();
  mTofpGeom->init(this);

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

  // RUN IV 023-035, east PVPD dead
  if(runnumber>5023000&&runnumber<5035000) {
    mEastPVPDValid = kFALSE;
  } else {
    mEastPVPDValid = kTRUE;
  }

  return kStOK;

}

//_____________________________________________________________________________
Int_t StTofCalibMaker::initParameters(int runnumber)
{
  mYear2 = (runnumber<4000000);
  mYear3 = (runnumber>4000000&&runnumber<5000000);
  mYear4 = (runnumber>5000000&&runnumber<6000000);
  mYear5 = (runnumber>6000000&&runnumber<7000000);
  mYear8 = (runnumber>9000000&&runnumber<10000000);
  /// initialize the calibrations parameters from dbase
  /// read in and check the size
  gMessMgr->Info("","OS") << "   -- retrieving run parameters from Calibrations_tof" << endm;
  TDataSet *mDbDataSet = GetDataBase("Calibrations/tof/tofTzero");
  if (!mDbDataSet){
    gMessMgr->Error("unable to get TOF run parameters","OS");
    return kStErr;
  }
  
  /////////////////////////////
  /// year 2,3,4
  /////////////////////////////
  if(mYear2||mYear3||mYear4) {
    gMessMgr->Info("     loading parameters for Run II/III/IV", "OS");
    // -- T0 parameters --
    St_tofTzero* tofT0 = static_cast<St_tofTzero*>(mDbDataSet->Find("tofTzero"));
    if(!tofT0) {
    gMessMgr->Error("unable to get Tzero table","OS");
    return kStErr;
    } else {
      tofTzero_st* t0 = static_cast<tofTzero_st*>(tofT0->GetArray());
      Int_t nRows = t0[0].entries;
      if(nRows<0||nRows>mNMax) {
	gMessMgr->Error("# of Tzero out of range","OS");
	return kStErr;
      }
      for(Int_t i=0;i<nRows;i++) {
	int daqId = t0[0].daqChannel[i];
	int tdcChan = t0[0].tdcChan[i];
	// check the daqId
	if(daqId<0) continue;
	if(tdcChan<42) {
	  mTofpT0[daqId] =  (Double_t)(t0[0].Tzero[i]);
	  if(Debug()) {
	    gMessMgr->Info("","OS") << " -TOFp- daqId=" << daqId << " tdcChan=" << tdcChan << " T0=" << mTofpT0[daqId] << endm;
	  }
	} else {
	  mTofrT0[daqId] = (Double_t)(t0[0].Tzero[i]);
	  if(Debug()) {
	    gMessMgr->Info("","OS") << " -TOFr- daqId=" << daqId << " tdcChan=" << tdcChan << " T0=" << mTofrT0[daqId] << endm;
	  }
	}
      }
    }
    // -- TA slewing parameters --
    St_tofTACorr *tofTA = static_cast<St_tofTACorr*>(mDbDataSet->Find("tofTACorr"));
    if(!tofTA) {
      gMessMgr->Error("unable to find TA slewing parameters","OS");
      return kStErr;
    }
    tofTACorr_st *tofta = static_cast<tofTACorr_st*>(tofTA->GetArray());
    for(Int_t i=0;i<mNMax;i++) {
      int daqId = tofta[0].daqChannel[i];
      int tdcChan = tofta[0].tdcChan[i];
      // check the daqId
      if(daqId<0) continue;
      for(int j=0;j<mNPar;j++) {
	int ijdaq = daqId*mNPar+j;
	int ij = i*mNPar+j;
	if(tdcChan<42) {
	  if(daqId>=mNTOFp) {
	    gMessMgr->Warning("More than expected TOFp channels read in","OS");
	  } else {
	    mTofpTAPar[ijdaq] = (Double_t)(tofta[0].a[ij]);
	    if(Debug()) {
	      gMessMgr->Info("","OS") << " -TOFp- daqId=" << daqId << " tdcChan=" << tdcChan << " TA Corr[" << j << "]=" << mTofpTAPar[ijdaq] << endm;
	    }	  
	  }
	} else if(tdcChan<48) {
	  if(daqId>=mNPVPD) {
	    gMessMgr->Warning("More than expected pVPD channels read in","OS");
	  } else {
	    mPVPDTAPar[ijdaq] = (Double_t)(tofta[0].a[ij]);
	    if(Debug()) {
	      gMessMgr->Info("","OS") << " -pVPD- daqId=" << daqId << " tdcChan=" << tdcChan << " TA Corr[" << j << "]=" << mPVPDTAPar[ijdaq] << endm;
	    }	  
	  }
	} else {
	  if(daqId>=mNTOFr) {
	    gMessMgr->Warning("More than expected TOFr channels read in","OS");
	  } else {
	    mTofrTAPar[ijdaq] = (Double_t)(tofta[0].a[ij]);
	    if(Debug()) {
	      gMessMgr->Info("","OS") << " -TOFr- daqId=" << daqId << " tdcChan=" << tdcChan << " TA Corr[" << j << "]=" << mTofrTAPar[ij] << endm;
	    }	  
	  }
	}
      }
    }
    
    // -- Z corr parameters --
    St_tofCorrection *tofrZCorr = static_cast<St_tofCorrection*>(mDbDataSet->Find("tofrZhitCorr"));
    St_tofCorrection *tofpZCorr = static_cast<St_tofCorrection*>(mDbDataSet->Find("tofpZhitCorr"));
    if(!tofrZCorr && !tofpZCorr) {
      gMessMgr->Error("unable to find Zhit corr parameters","OS");
      return kStErr;
    }
    if(tofrZCorr) {
      tofCorrection_st *tofrZ = static_cast<tofCorrection_st*>(tofrZCorr->GetArray());
      for(int i=0;i<mNPar;i++) {
	mTofrZPar[i] = tofrZ[0].a[i];
	if(Debug()) {
	  gMessMgr->Info("","OS") << " -TOFr- Zcorr[" << i << "]=" << mTofrZPar[i] << endm;
	} 
      }
    }
    if(tofpZCorr) {
      tofCorrection_st *tofpZ = static_cast<tofCorrection_st*>(tofpZCorr->GetArray());
      for(int i=0;i<mNPar;i++) {
	mTofpZPar[i] = tofpZ[0].a[i];
	if(Debug()) {
	  gMessMgr->Info("","OS") << " -TOFp- Zcorr[" << i << "]=" << mTofpZPar[i] << endm;
	} 
      }
    }
    
    // ADC range parameters ( specially for TOFp in Run IV )
    St_tofAdcRange *tofAdc = static_cast<St_tofAdcRange*>(mDbDataSet->Find("tofAdcRange"));
    if(!tofAdc) {
      gMessMgr->Warning("unable to find ADC range parameters, use default values!","OS");
    }
    tofAdcRange_st *tofadc = static_cast<tofAdcRange_st*>(tofAdc->GetArray());
    for(Int_t i=0;i<mNMax;i++) {
      int daqId = tofadc[0].daqChannel[i];
      int adcChan = tofadc[0].adcChan[i];
      if(daqId<0) continue;
      if(adcChan<42) {
	if(daqId>=mNTOFp) {
	  gMessMgr->Warning("More than expected TOFp channels read in","OS");
	} else {
	  mTofpADCMin[daqId] = tofadc[0].adcMin[i];
	  mTofpADCMax[daqId] = tofadc[0].adcMax[i];
	  if(Debug()) {
	    gMessMgr->Info("","OS") << " -TOFp- daqId=" << daqId << " adcChan=" << adcChan << " min=" << mTofpADCMin[daqId] << " max=" << mTofpADCMax[daqId] << endm;
	  }
	}
      }
      if(adcChan>=60) {
	if(daqId>=mNTOFr) {
	  gMessMgr->Warning("More than expected TOFr channels read in","OS");
	} else {
	  mTofrADCMin[daqId] = tofadc[0].adcMin[i];
	  mTofrADCMax[daqId] = tofadc[0].adcMax[i];
	  if(Debug()) {
	    gMessMgr->Info("","OS") << " -TOFr- daqId=" << daqId << " adcChan=" << adcChan << " min=" << mTofrADCMin[daqId] << " max=" << mTofrADCMax[daqId] << endm;
	  }
	}
      }
    }
    
    // res parameters ( specially for TOFp in Run IV )
    St_tofResolution *tofRes = static_cast<St_tofResolution*>(mDbDataSet->Find("tofResolution"));
    if(!tofRes) {
      gMessMgr->Warning("unable to find resolution parameters, nSimgaTof UNAVAILABLE!","OS");
    }
    tofResolution_st *tofres = static_cast<tofResolution_st*>(tofRes->GetArray());
    for(Int_t i=0;i<mNMax;i++) {
      int daqId = tofres[0].daqChannel[i];
      int tdcChan = tofres[0].tdcChan[i];
      if(daqId<0) continue;
      if(tdcChan<42) {
	if(daqId>=mNTOFp) {
	  gMessMgr->Warning("More than expected TOFp channels read in","OS");
	} else {
	  mTofpRes[daqId] = tofres[0].resolution[i];
	  if(Debug()) {
	    gMessMgr->Info("","OS") << " -TOFp- daqId=" << daqId << " tdcChan=" << tdcChan << " resolution=" << mTofpRes[daqId] << endm;
	  }
	}
      } else if(tdcChan<48) {
	if(daqId>=mNPVPD) {
	  gMessMgr->Warning("More than expected PVPD channels read in","OS");
	} else {
	  mPVPDRes[daqId] = tofres[0].resolution[i];
	  if(Debug()) {
	    gMessMgr->Info("","OS") << " -PVPD- daqId=" << daqId << " tdcChan=" << tdcChan << " resolution=" << mPVPDRes[daqId] << endm;
	  }
	}
      } else {
	if(daqId>=mNTOFr) {
	  gMessMgr->Warning("More than expected TOFr channels read in","OS");
	} else {
	  mTofrRes[daqId] = tofres[0].resolution[i];
	  if(Debug()) {
	    gMessMgr->Info("","OS") << " -TOFr- daqId=" << daqId << " tdcChan=" << tdcChan << " resolution=" << mTofrRes[daqId] << endm;
	  }
	}
      }
    }
    /////////////////////////////
    /// year 5
    /////////////////////////////
  } else if(mYear5) {
    gMessMgr->Info("     loading parameters for Run V","OS");
    
    // read INL table
    St_tofr5INLtable* tofr5INLtable = static_cast<St_tofr5INLtable*>(mDbDataSet->Find("tofr5INLtable"));
    if(!tofr5INLtable) {
      gMessMgr->Error("unable to get tofr5 INL table parameters","OS");
      //    assert(tofr5INLtable);
      return kStErr;
    }
    tofr5INLtable_st* inltable = static_cast<tofr5INLtable_st*>(tofr5INLtable->GetArray());
    Int_t numRows = tofr5INLtable->GetNRows();
    
    if(Debug()) gMessMgr->Info("","OS") << " Number of rows read in: " << numRows << " for INL tables" << endm;
    
    Char_t *boardName;
    Short_t boardId;
    Short_t tdcId;
    Float_t INLcorr[1024];
    
    for (Int_t i=0;i<numRows;i++) {
      boardName = "";
      boardId = -1;
      tdcId = -1;
      for(int j=0;j<mTdcChannel;j++) {
	INLcorr[j] = 0.0;
      }
      
      boardName = (Char_t *)(inltable[i].boardID);
      boardId = inltable[i].boardNumber;
      tdcId = inltable[i].TDCID;
      if(Debug())
	gMessMgr->Info("","OS") << " name = " << boardName << " bId = " << boardId << " tdcId = " << tdcId << endm;
      for(int j=0;j<mTdcChannel;j++) {
	INLcorr[j] = inltable[i].INLcorrection[j];
	if(Debug()&&j%100==0) gMessMgr->Info("","OS") << " j=" << j << " inlcorr=" << INLcorr[j] << endm;
	mINLtable[boardId][tdcId][j] = INLcorr[j];
      }
            
    }

    // read tofTotCorr table
    St_tofTotCorr* tofTotCorr = static_cast<St_tofTotCorr*>(mDbDataSet->Find("tofTotCorr"));
    if(!tofTotCorr) {
      gMessMgr->Error("unable to get tofr5 TotCorr table parameters","OS");
      //    assert(tofTotCorr);
      return kStErr;
    }
    tofTotCorr_st* totCorr = static_cast<tofTotCorr_st*>(tofTotCorr->GetArray());
    numRows = tofTotCorr->GetNRows();

    if(Debug()) gMessMgr->Info("","OS") << " Number of rows read in: " << numRows << " for ToT correction" << endm;

    if(numRows!=mNTOFr5+mNPVPD) {
      gMessMgr->Warning("","OS") << " Mis-matched number of rows in tofTotCorr table! Return! " << endm;
      //      return kStErr;
    }

    //    for (Int_t i=0;i<numRows;i++) {
    for(Int_t i=0;i<mNTOFr5+mNPVPD;i++) {
      short trayId = totCorr[i].trayId;
      short moduleId = totCorr[i].moduleId;
      short cellId = totCorr[i].cellId;
      short tdcId = totCorr[i].tdcId;

      if(Debug()) gMessMgr->Info("","OS") << " module " << moduleId << " cell " << cellId << " tdcId " << tdcId << endm;
      for(Int_t j=0;j<mNBinMax;j++) {
	if(trayId==-1||trayId==-2) { // pVPD east west
	  mPVPDTotEdge[cellId-1][j] = totCorr[i].tot[j];
	  mPVPDTotCorr[cellId-1][j] = totCorr[i].corr[j];
	} else if(trayId==93) { // TOFr5 tray
	  mTofr5TotEdge[(moduleId-1)*6+cellId-1][j] = totCorr[i].tot[j];
	  mTofr5TotCorr[(moduleId-1)*6+cellId-1][j] = totCorr[i].corr[j];
	  if(Debug()&&j%10==0) gMessMgr->Info("","OS") << " j=" << j << " tot " << mTofr5TotEdge[(moduleId-1)*6+cellId-1][j] << " corr " << mTofr5TotCorr[(moduleId-1)*6+cellId-1][j] << endm; 
	} else { // wrong trayId
	}
      } // end j 0->mNBinMax
    } // end i 0->numRows


    // read tofZCorr table
    St_tofZCorr* tofZCorr = static_cast<St_tofZCorr*>(mDbDataSet->Find("tofZCorr"));
    if(!tofZCorr) {
      gMessMgr->Error("unable to get tofr5 ZCorr table parameters","OS");
      //    assert(tofZCorr);
      return kStErr;
    }
    tofZCorr_st* zCorr = static_cast<tofZCorr_st*>(tofZCorr->GetArray());
    numRows = tofZCorr->GetNRows();

    if(Debug()) gMessMgr->Info("","OS") << " Number of rows read in: " << numRows << " for Z correction" << endm;

    if(numRows!=mNTOFr5) {   // only for TOFr5 tray
      gMessMgr->Warning("","OS") << " Mis-matched number of rows in tofZCorr table! Return! " << endm;
      //      return kStErr;
    }

    //    for (Int_t i=0;i<numRows;i++) {
    for (Int_t i=0;i<mNTOFr5;i++) {
      short trayId = zCorr[i].trayId;
      short moduleId = zCorr[i].moduleId;
      short cellId = zCorr[i].cellId;
//      short tdcId = zCorr[i].tdcId;

      if(Debug()) gMessMgr->Info("","OS") << " module " << moduleId << " cell " << cellId << endm;
      for(Int_t j=0;j<mNBinMax;j++) {
	if(trayId==93) { // TOFr5 tray
	  mTofr5ZEdge[(moduleId-1)*6+cellId-1][j] = zCorr[i].z[j];
	  mTofr5ZCorr[(moduleId-1)*6+cellId-1][j] = zCorr[i].corr[j];
	  if(Debug()&&j%10==0) gMessMgr->Info("","OS") << " j=" << j << " z " << mTofr5ZEdge[(moduleId-1)*6+cellId-1][j] << " corr " << mTofr5ZCorr[(moduleId-1)*6+cellId-1][j] << endm; 
	} else { // wrong trayId
	}
      } // end j 0->mNBinMax
    } // end i 0->numRows

    /////////////////////////////
    /// year 8
    /////////////////////////////
  }  else if(mYear8) {
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

      int index = (trayId-mNTray-1)*mNVPD+(cellId-1);   // used for vpd index

      if(Debug()) gMessMgr->Info("","OS") << " tray " << trayId << " board " << boardId << " cell " << cellId << endm;
      for(Int_t j=0;j<mNBinMax;j++) {
	if(trayId==mWestVpdTrayId||trayId==mEastVpdTrayId) { // upVPD east west
	  mVPDTotEdge[index][j] = totCorr[i].tot[j];
	  mVPDTotCorr[index][j] = totCorr[i].corr[j];
	} else if(trayId>0&&trayId<=mNTray){ // trays
	  mTofTotEdge[trayId-1][boardId-1][j] = totCorr[i].tot[j];
	  mTofTotCorr[trayId-1][boardId-1][j] = totCorr[i].corr[j];
	  if(Debug()&&j%10==0) gMessMgr->Info("","OS") << " j=" << j << " tot " << mTofTotEdge[trayId-1][boardId-1][j] << " corr " << mTofTotCorr[trayId-1][boardId-1][j] << endm; 
	}
      } // end j 0->mNBinMax
    } // end i 0->numRows


    // read tofZCorr table
    St_tofZCorr* tofZCorr = static_cast<St_tofZCorr*>(mDbDataSet->Find("tofZCorr"));
    if(!tofZCorr) {
      gMessMgr->Error("unable to get tof ZCorr table parameters","OS");
      //    assert(tofZCorr);
      return kStErr;
    }
    tofZCorr_st* zCorr = static_cast<tofZCorr_st*>(tofZCorr->GetArray());
    numRows = tofZCorr->GetNRows();

    if(numRows!=mNTray8*mNTDIG) {
      gMessMgr->Warning("","OS") << " Mis-matched number of rows in tofZCorr table! " << endm;
      //      return kStErr;
    }
    if(Debug()) gMessMgr->Info("","OS") << " Number of rows read in: " << numRows << " for Z correction" << endm;

    for (Int_t i=0;i<mNTray8*mNTDIG;i++) {
      short trayId = totCorr[i].trayId;
      short moduleId = totCorr[i].moduleId;
      short boardId = (moduleId-1)/4+1;      // used for trays
      short cellId = totCorr[i].cellId;      // used for vpds

      if(Debug()) gMessMgr->Info("","OS") << " tray " << trayId << " board " << boardId << " cell " << cellId << endm;
      for(Int_t j=0;j<mNBinMax;j++) {
	if(trayId>0&&trayId<=120) {  // trays
	  mTofZEdge[trayId-1][boardId-1][j] = zCorr[i].z[j];
	  mTofZCorr[trayId-1][boardId-1][j] = zCorr[i].corr[j];
	  if(Debug()&&j%10==0) gMessMgr->Info("","OS") << " j=" << j << " tot " << mTofZEdge[trayId-1][boardId-1][j] << " corr " << mTofZCorr[trayId-1][boardId-1][j] << endm; 
	}
      } // end j 0->mNBinMax
    } // end i 0->numRows

    // read tofTOffset table
    St_tofTOffset* tofTOffset = static_cast<St_tofTOffset*>(mDbDataSet->Find("tofTOffset"));
    if(!tofTOffset) {
      gMessMgr->Error("unable to get tof TOffset table parameters","OS");
      //    assert(tofTOffset);
      return kStErr;
    }
    tofTOffset_st* tZero = static_cast<tofTOffset_st*>(tofTOffset->GetArray());
    numRows = tofTOffset->GetNRows();

    if(Debug()) gMessMgr->Info("","OS") << " Number of rows read in: " << numRows << " for TOffset correction" << endm;

    if(numRows!=mNTray8) {
      gMessMgr->Warning("","OS") << " Mis-matched number of rows in tofTOffset table! " << endm;
      //      return kStErr;
    }
    for (Int_t i=0;i<mNTray8;i++) {
      short trayId = tZero[i].trayId;
      if(Debug()) gMessMgr->Info("","OS") << " tray " << trayId << endm;
      
      if(trayId>0&&trayId<=mNTray) {
	for(int j=0;j<mNTOF;j++) {
	  mTofTZero[trayId-1][j/6][j%6] = tZero[i].T0[j];
	  if(Debug()&&j%10==0) gMessMgr->Info("","OS") << " j=" << j << " T0 " << mTofTZero[trayId-1][j/6][j%6] << endm; 
	}
      }
    }

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

    /*
    // test -- read in from data files
    ifstream inData;

    inData.open("/star/u/dongx/TOFr/y8update/dbase/dat/pvpdCali_9050074.dat");
    for (Int_t i=0;i<mNVPD*2;i++) {
      int tubeId, nbin;
      inData>>tubeId;
      inData>>nbin;  // nbin intervals, nbin+1 edges

      short trayId = (tubeId-1)/mNVPD + mNTray + 1;
      short cellId = (tubeId-1)%mNVPD + 1;      // used for vpds

      if(Debug()) gMessMgr->Info("","OS") << " tray " << trayId << " cell " << cellId << endm;
      int index = (trayId-mNTray-1)*mNVPD+(cellId-1);
      for(Int_t j=0;j<=nbin;j++) {
	double xedge;
	inData>>xedge;
	if(trayId==mWestVpdTrayId||trayId==mEastVpdTrayId) { // upVPD east west
	  mVPDTotEdge[index][j] = xedge;
	}
      }
      for(Int_t j=0;j<=nbin;j++) {
	double ycorr;
	inData>>ycorr;
	if(trayId==mWestVpdTrayId||trayId==mEastVpdTrayId) { // upVPD east west
	  mVPDTotCorr[index][j] = ycorr;
	}
      } // end j 0->mNBinMax
      for(Int_t j=0;j<=nbin;j++) {
	if(Debug()&&j%10==0) gMessMgr->Info("","OS") << " j=" << j << " tot " << mVPDTotEdge[index][j] << " corr " << mVPDTotCorr[index][j] << endm; 
      } // end j 0->mNBinMax
    } // end i 0->numRows
    inData.close();

    inData.open("/star/u/dongx/TOFr/y8update/dbase/dat/totCali_9050074.dat");
    for (Int_t i=0;i<mNTray8*mNTDIG;i++) {
      int trayId, boardId, nbin;
      inData >> trayId >> boardId;
      inData >> nbin;

      if(Debug()) gMessMgr->Info("","OS") << " tray " << trayId << " board " << boardId << endm;
      for(Int_t j=0;j<=nbin;j++) {
	if (trayId>0&&trayId<=mNTray){ // trays
	  double xedge;
	  inData>>xedge;
	  mTofTotEdge[trayId-1][boardId-1][j] = xedge;
	}
      }
      for(Int_t j=0;j<=nbin;j++) {
	if (trayId>0&&trayId<=mNTray){ // trays
	  double ycorr;
	  inData>>ycorr;
	  mTofTotCorr[trayId-1][boardId-1][j] = ycorr;
	}
      }
      for(Int_t j=0;j<=nbin;j++) {
	if(Debug()&&j%10==0) gMessMgr->Info("","OS") << " j=" << j << " tot " << mTofTotEdge[trayId-1][boardId-1][j] << " corr " << mTofTotCorr[trayId-1][boardId-1][j] << endm; 
      } // end j 0->mNBinMax
    } // end i 0->numRows
    inData.close();

    inData.open("/star/u/dongx/TOFr/y8update/dbase/dat/zCali_9050074.dat");
    for (Int_t i=0;i<mNTray8*mNTDIG;i++) {
      int trayId, boardId, nbin;
      inData >> trayId >> boardId;
      inData >> nbin;

      if(Debug()) gMessMgr->Info("","OS") << " tray " << trayId << " board " << boardId << endm;
      for(Int_t j=0;j<=nbin;j++) {
	if (trayId>0&&trayId<=mNTray){ // trays
	  double xedge;
	  inData>>xedge;
	  mTofZEdge[trayId-1][boardId-1][j] = xedge;
	}
      }
      for(Int_t j=0;j<=nbin;j++) {
	if (trayId>0&&trayId<=mNTray){ // trays
	  double ycorr;
	  inData>>ycorr;
	  mTofZCorr[trayId-1][boardId-1][j] = ycorr;
	}
      }
      for(Int_t j=0;j<=nbin;j++) {
	if(Debug()&&j%10==0) gMessMgr->Info("","OS") << " j=" << j << " z " << mTofZEdge[trayId-1][boardId-1][j] << " corr " << mTofZCorr[trayId-1][boardId-1][j] << endm; 
      } // end j 0->mNBinMax
    } // end i 0->numRows
    inData.close();

    inData.open("/star/u/dongx/TOFr/y8update/dbase/dat/t0_9050074.dat");
    for (Int_t i=0;i<mNTray8;i++) {
      for (Int_t j=0;j<mNModule;j++) {
	for (Int_t k=0;k<mNCell;k++) {
	  int trayId, moduleId, cellId;
	  inData>>trayId>>moduleId>>cellId;
      
	  double t0;
	  inData >> t0;
	  mTofTZero[trayId-1][moduleId-1][cellId-1] = t0;
	  if(Debug()&&k==0) gMessMgr->Info("","OS") << " trayId=" << trayId << " moduleId=" << moduleId << " cellId=" << cellId << " T0 " << mTofTZero[trayId-1][moduleId-1][cellId-1] << endm; 
	}
      }
    }
    inData.close();

    //mPhaseOffset8 = -33796.; // run 8, only e/w difference
    mPhaseOffset8 = - 27945.8; //9058055  
    */

    // ========== Set Beam Line ===================== 
    double x0 = 0.;
    double y0 = 0.;
    double dxdz = 0.;
    double dydz = 0.;
    
    // Get Current Beam Line Constraint from database
    TDataSet* dbDataSet = this->GetDataBase("Calibrations/rhic/vertexSeed");
    
    if (dbDataSet) {
      vertexSeed_st* vSeed = ((St_vertexSeed*) (dbDataSet->FindObject("vertexSeed")))->GetTable();
      
      x0 = vSeed->x0;
      y0 = vSeed->y0;
      dxdz = vSeed->dxdz;
      dydz = vSeed->dydz;
    }
    else {
      LOG_INFO << "StTofCalibMaker -- No Database for beamline" << endm;
    }

    LOG_INFO << "BeamLine Constraint: " << endm;
    LOG_INFO << "x(z) = " << x0 << " + " << dxdz << " * z" << endm;
    LOG_INFO << "y(z) = " << y0 << " + " << dydz << " * z" << endm;
    
    //**********
    //beam line not be calibrated yet
    //x0 shift by 0.5
    //x0 = 0.5;
    //**********    
    StThreeVectorD origin(x0,y0,0.0);
    double pt = 88889999;
    double nxy=::sqrt(dxdz*dxdz +  dydz*dydz);
    if(nxy<1.e-5){ // beam line _MUST_ be tilted
      LOG_WARN << "StTofCalibMaker:: Beam line must be tilted!" << endm;
      nxy=dxdz=1.e-5;
    }
    double p0=pt/nxy;
    double px   = p0*dxdz;
    double py   = p0*dydz;
    double pz   = p0; // approximation: nx,ny<<0
    StThreeVectorD MomFstPt(px*GeV, py*GeV, pz*GeV);
    if(mBeamHelix) delete mBeamHelix;
    mBeamHelix = new StPhysicalHelixD(MomFstPt,origin,0.5*tesla,1.);
    
  } // end if (mYear8)

  return kStOK;
}

//____________________________________________________________________________
Int_t StTofCalibMaker::FinishRun(int runnumber)
{
  
  if(mTofpGeom) delete mTofpGeom;
  mTofpGeom = 0;

  if(mBeamHelix) delete mBeamHelix;
  mBeamHelix = 0;
  
  return kStOK;
}

//_____________________________________________________________________________
Int_t StTofCalibMaker::Finish()
{
  clearFormulars();
  return kStOK;
}

//_____________________________________________________________________________
void StTofCalibMaker::clearFormulars()
{
  if (mTofrSlewing) delete mTofrSlewing;
  if (mTofpSlewing) delete mTofpSlewing;
  if (mTofrZCorr) delete mTofrZCorr;
  if (mTofpZCorr) delete mTofpZCorr;
  if (mPVPDSlewing) delete mPVPDSlewing;
}

//_____________________________________________________________________________
Int_t StTofCalibMaker::Make()
{
  gMessMgr->Info(" StTofCalibMaker::Maker: starting ...","OS");

  mTStart = -9999.;
  mTDiff = -9999.;
  mVPDVtxZ = -9999.;
  mProjVtxZ = -9999.;
  mVPDHitPatternEast = 0;
  mVPDHitPatternWest = 0;

  Int_t iret = kStOK;
  if(mYear2||mYear3||mYear4){
    iret = processEventYear2to4();
  } else if(mYear5) {
    iret = processEventYear5();
  } else if(mYear8) {
    iret = processEventYear8();
  }
  return kStOK;
}

//____________________________________________________________________________
Int_t StTofCalibMaker::processEventYear2to4(){
  mEvent = (StEvent *) GetInputDS("StEvent");
  
  // event selection
  if( !mEvent || !mEvent->primaryVertex() ||
      !mEvent->tofCollection() ||
      !mEvent->tofCollection()->dataPresent() ||
      (!mEvent->tofCollection()->cellsPresent() &&
       !mEvent->tofCollection()->slatsPresent()) ) {
    gMessMgr->Info("","OS") << "StTofCalibMaker -- nothing to do ... bye-bye" << endm;
    return kStOK;
  }
  
  StThreeVectorD vtx = mEvent->primaryVertex()->position();
  Double_t vz = vtx.z();

  //-------------------------------------------------
  // pVPD calibration and calculate the start timing
  //-------------------------------------------------
  StTofCollection *theTof = mEvent->tofCollection();
  StSPtrVecTofData &tofData = theTof->tofData();
  for(int i=0;i<mNPVPD;i++) {
    mPVPDAdc[i] = tofData[42+i]->adc(); 
    mPVPDTdc[i] = tofData[42+i]->tdc();
    if(mYear3||mYear4) {
      mPVPDAdc[i] = tofData[54+i]->adc(); 
    }
  }

  mValidStartTime = kTRUE;
  Double_t T0 = tstart(mPVPDAdc, mPVPDTdc, vz);
  if(T0>1000.) {
    gMessMgr->Info(" Not a good PVPD-required event!","OS");
    mValidStartTime = kFALSE;
    //    return kStOK;
  } 
  gMessMgr->Info("","OS") << " pVPD start timing T0 = " << T0 << endm;

  StTofHitCollection *tofHit = new StTofHitCollection();

  //---------------------------------------
  // Tofr calibrations cells -> hits
  //---------------------------------------
  StSPtrVecTofCell &tofCell = theTof->tofCells();
  Int_t ncells = tofCell.size();
  gMessMgr->Info("","OS") << " TOFr matched cells : " << ncells << endm;
  for(int i=0;i<ncells;i++) {
    StTofHit *aHit = new StTofHit;
    StTofCell *aCell = dynamic_cast<StTofCell*>(tofCell[i]);
    aHit->setTrayIndex(aCell->trayIndex());
    aHit->setModuleIndex(aCell->moduleIndex());
    aHit->setCellIndex(aCell->cellIndex());
    aHit->setCellCollIndex(i);

    int daqId = aCell->daqIndex();
    aHit->setDaqIndex(daqId);
    Double_t tof = (Double_t)aCell->tdc()*mTDCWidth;
    Double_t adc = (Double_t)aCell->adc();
    Double_t zhit = (Double_t)aCell->zHit();
    
    StTrack *thisTrack = aCell->associatedTrack();
    aHit->setAssociatedTrack(thisTrack);
    StTrackGeometry *theTrackGeometry = 
      (mOuterGeometry)?thisTrack->outerGeometry():thisTrack->geometry();
    Double_t L = tofPathLength(&vtx, &aCell->position(), theTrackGeometry->helix().curvature());
    aHit->setPathLength((Float_t)L);

    if(adc<mTofrADCMin[daqId]||adc>=mTofrADCMax[daqId]) {
      delete aHit;
      continue;
    }
    if(mValidCalibPar&&mValidStartTime) {
      Double_t tofcorr = tofrAllCorr(tof, T0, adc, zhit, daqId);
      aHit->setTimeOfFlight((Float_t)tofcorr);
      
      Double_t beta = L/(tofcorr*(C_C_LIGHT/1.e9));
      aHit->setBeta((Float_t)beta);
      
      const StThreeVectorF momentum = thisTrack->geometry()->momentum();
      Double_t ptot = momentum.mag();
      Double_t tofe = L/(C_C_LIGHT/1.e9)*sqrt(ptot*ptot+M_ELECTRON*M_ELECTRON)/ptot;
      Double_t tofpi = L/(C_C_LIGHT/1.e9)*sqrt(ptot*ptot+M_PION_PLUS*M_PION_PLUS)/ptot;
      Double_t tofk = L/(C_C_LIGHT/1.e9)*sqrt(ptot*ptot+M_KAON_PLUS*M_KAON_PLUS)/ptot;
      Double_t tofp = L/(C_C_LIGHT/1.e9)*sqrt(ptot*ptot+M_PROTON*M_PROTON)/ptot;
      
      aHit->setTofExpectedAsElectron((Float_t)tofe);
      aHit->setTofExpectedAsPion((Float_t)tofpi);
      aHit->setTofExpectedAsKaon((Float_t)tofk);
      aHit->setTofExpectedAsProton((Float_t)tofp);

      float sigmae = 999.;
      float sigmapi = 999.;
      float sigmak = 999.;
      float sigmap = 999.;
      float res = mTofrRes[daqId];
      if(fabs(res)>1.e-5) {
	sigmae = (Float_t)((tofcorr-tofe)/res);
	sigmapi = (Float_t)((tofcorr-tofpi)/res);
	sigmak = (Float_t)((tofcorr-tofk)/res);
	sigmap = (Float_t)((tofcorr-tofp)/res);
      }
      aHit->setSigmaElectron(sigmae);
      aHit->setSigmaPion(sigmapi);
      aHit->setSigmaKaon(sigmak);
      aHit->setSigmaProton(sigmap);

    } else {
      aHit->setTimeOfFlight(9999.);
      aHit->setBeta(9999.);
    }

    tofHit->push_back(aHit); 
  }

  //------------------------------------
  // Tofp calibrations  slats -> hits
  // -----------------------------------
  StSPtrVecTofSlat &tofSlat = theTof->tofSlats();
  Int_t nslats = tofSlat.size();
  for(int i=0;i<nslats;i++) {
    StTofHit *aHit = new StTofHit;
    StTofSlat *aSlat = tofSlat[i];
    aHit->setTrayIndex(0);
    aHit->setModuleIndex(0);
    //
    // slatIndex() fake slatId,  = daqId+1 actually -- in TofpMatchMaker
    //
    int daqId = aSlat->slatIndex()-1;
    int slatId = mTofpGeom->daqToSlatId(daqId);

    aHit->setCellIndex(slatId);
    aHit->setCellCollIndex(i);
    aHit->setDaqIndex(daqId);

    Double_t tof = (Double_t)aSlat->tdc()*mTDCWidth;
    Double_t adc = (Double_t)aSlat->adc();
    Double_t zhit = (Double_t)aSlat->zHit();

    StTrack *thisTrack = aSlat->associatedTrack();
    aHit->setAssociatedTrack(thisTrack);
    StTrackGeometry *theTrackGeometry = 
      (mOuterGeometry)?thisTrack->outerGeometry():thisTrack->geometry();
    Double_t L = tofPathLength(&vtx, &aSlat->position(), theTrackGeometry->helix().curvature());
    aHit->setPathLength((Float_t)L);
    
    //    if(adc<mTofpADCMin[daqId]||adc>=mTofpADCMax[daqId]) {
    if(adc<mTofpADCMin[daqId]) {
      delete aHit;
      continue;
    }
    if(mValidCalibPar&&mValidStartTime) {
      Double_t tofcorr = tofpAllCorr(tof, T0, adc, zhit, daqId);
      aHit->setTimeOfFlight((Float_t)tofcorr);
      
      Double_t beta = L/(tofcorr*(C_C_LIGHT/1.e9));
      aHit->setBeta((Float_t)beta);
      
      const StThreeVectorF momentum = thisTrack->geometry()->momentum();
      Double_t ptot = momentum.mag();
      Double_t tofe = L/(C_C_LIGHT/1.e9)*sqrt(ptot*ptot+M_ELECTRON*M_ELECTRON)/ptot;
      Double_t tofpi = L/(C_C_LIGHT/1.e9)*sqrt(ptot*ptot+M_PION_PLUS*M_PION_PLUS)/ptot;
      Double_t tofk = L/(C_C_LIGHT/1.e9)*sqrt(ptot*ptot+M_KAON_PLUS*M_KAON_PLUS)/ptot;
      Double_t tofp = L/(C_C_LIGHT/1.e9)*sqrt(ptot*ptot+M_PROTON*M_PROTON)/ptot;
      aHit->setTofExpectedAsElectron((Float_t)tofe);
      aHit->setTofExpectedAsPion((Float_t)tofpi);
      aHit->setTofExpectedAsKaon((Float_t)tofk);
      aHit->setTofExpectedAsProton((Float_t)tofp);

      float sigmae = 999.;
      float sigmapi = 999.;
      float sigmak = 999.;
      float sigmap = 999.;
      float res = mTofpRes[daqId];
      if(fabs(res)>1.e-5) {
	sigmae = (Float_t)((tofcorr-tofe)/res);
	sigmapi = (Float_t)((tofcorr-tofpi)/res);
	sigmak = (Float_t)((tofcorr-tofk)/res);
	sigmap = (Float_t)((tofcorr-tofp)/res);
      }
      aHit->setSigmaElectron(sigmae);
      aHit->setSigmaPion(sigmapi);
      aHit->setSigmaKaon(sigmak);
      aHit->setSigmaProton(sigmap);

    } else {
      aHit->setTimeOfFlight(9999.);
      aHit->setBeta(9999.);
    }
    tofHit->push_back(aHit); 
  }

 
  // store the hit collection in StEvent
  for (size_t j=0;j<tofHit->size();j++){
    theTof->addHit(tofHit->getHit(j)); 
    if (Debug())
      gMessMgr->Info("","OS") << "storing " << j << "  " << "  tray:"
			      << tofHit->getHit(j)->trayIndex() << "  module:"
			      << tofHit->getHit(j)->moduleIndex() << "  cell "
			      << tofHit->getHit(j)->cellIndex()  << endm;
  }

  // add the tof pidtraits
  StSPtrVecTofHit& tofHitVec = theTof->tofHits();
  StSPtrVecTrackNode& nodes = mEvent->trackNodes();
  for (unsigned int iNode=0; iNode<nodes.size(); iNode++){
    StTrack *theTrack = nodes[iNode]->track(primary);
    if(!theTrack) continue;
    Int_t trkId = theTrack->key();
    for (size_t j=0;j<tofHitVec.size();j++){
      StTrack *aTrack = tofHitVec[j]->associatedTrack();
      if(!aTrack) continue;
      if(aTrack->key()!=trkId) continue;
      StTofPidTraits* pidTof = new StTofPidTraits(tofHitVec[j]->trayIndex(), tofHitVec[j]->moduleIndex(), tofHitVec[j]->cellIndex(), tofHitVec[j]->timeOfFlight(), tofHitVec[j]->pathLength(), tofHitVec[j]->beta());
      pidTof->setSigmaElectron(tofHitVec[j]->sigmaElectron());
      pidTof->setSigmaPion(tofHitVec[j]->sigmaPion());
      pidTof->setSigmaKaon(tofHitVec[j]->sigmaKaon());
      pidTof->setSigmaProton(tofHitVec[j]->sigmaProton());
      
      theTrack->addPidTraits(pidTof);
    }
  }

  delete tofHit;
  
  return kStOK;
}

//____________________________________________________________________________
Int_t StTofCalibMaker::processEventYear5(){

  mEvent = (StEvent *) GetInputDS("StEvent");
  
  // event selection
  if( !mEvent || !mEvent->primaryVertex() ||
      !mEvent->tofCollection() ||
      (!mEvent->tofCollection()->dataPresent() &&
       !mEvent->tofCollection()->rawdataPresent()) ||
      (!mEvent->tofCollection()->cellsPresent()) ) {
    gMessMgr->Info("","OS") << "StTofCalibMaker -- nothing to do ... bye-bye" << endm;
    return kStOK;
  }
  
  StThreeVectorD vtx = mEvent->primaryVertex()->position();
  Double_t vz = vtx.z();

  //-------------------------------------------------
  // pVPD calibration and calculate the start timing
  //-------------------------------------------------
  StTofCollection *theTof = mEvent->tofCollection();
  mSortTofRawData = new StSortTofRawData(theTof);
  IntVec validchannel = mSortTofRawData->GetValidChannel();

  int used[mNPVPD]          = {0,0,0,0,0,0};
  int channum[mNPVPD]       ={-1,-1,-1,-1,-1,-1};

  for(unsigned int ich=0;ich<validchannel.size();ich++){
    int chan = validchannel[ich];
    if(chan<mNTOFr5) continue;       // need only pvpd 
    int ichan = chan - mNTOFr5;
    if(ichan<0||ichan>=mNPVPD) continue;
    if(used[ichan]>0) continue;                   // skip multi hits
    used[ichan]++;
    // leading edge
    int tmptdc = (mSortTofRawData->GetLeadingTdc(chan))[0];    
    // do inl correction
    int bin = int(tmptdc)&0x03ff;
    float pvpdletdc = tmptdc + GetINLcorr(4,chan,bin);
    mPVPDLeTime[ichan] = pvpdletdc * VHRBIN2PS/1000.;
    // Trailing edge
    tmptdc = (mSortTofRawData->GetTrailingTdc(chan))[0];    
    // do inl correction
    bin = int(tmptdc)&0x0ff;
    float pvpdtetdc = tmptdc + GetINLcorr(5,chan,bin);
    float tetime = pvpdtetdc * HRBIN2PS/1000.;
    mPVPDTot[ichan] = tetime - mPVPDLeTime[ichan];
    channum[ichan]=ichan;
  }

  // sort out the piled-up hit - remove those hits not from this event
  int nPVPDFired = 0;
  for(int i=0;i<mNPVPD;i++) {
    if(channum[i]!=i) {
      mPVPDLeTime[i] = 0.;
      mPVPDTot[i] = 0.;
    } else {
      nPVPDFired++;
    }
  }
  for(int i=0;i<mNPVPD;i++) {
    if(channum[i]!=i) continue;
    int n0 = 0;
    for(int j=0;j<mNPVPD;j++) {
      if(channum[j]!=j) continue;
      float dt = mPVPDLeTime[j] - mPVPDLeTime[i];
      if(fabs(dt)<200.) n0++;
    }

    if(n0>=nPVPDFired/2) {  // OK
    } else { // not from this event
      mPVPDLeTime[i] = 0.;
      mPVPDTot[i] = 0.;
    }
  }
  
  mValidStartTime = kTRUE;
  Double_t T0 = tstart5(mPVPDTot, mPVPDLeTime, vz);
  if(T0<0.) {
    gMessMgr->Info(" Not a good PVPD-required event!","OS");
    mValidStartTime = kFALSE;
    //    return kStOK;
  } 
  gMessMgr->Info("","OS") << " pVPD start timing T0 = " << T0 << endm;

  StTofHitCollection *tofHit = new StTofHitCollection();

  //---------------------------------------
  // Tofr calibrations cells -> hits
  //---------------------------------------
  StSPtrVecTofCell &tofCell = theTof->tofCells();
  Int_t ncells = tofCell.size();
  gMessMgr->Info("","OS") << " TOFr matched cells : " << ncells << endm;
  for(int i=0;i<ncells;i++) {
    StTofHit *aHit = new StTofHit;
    StTofCell *aCell = dynamic_cast<StTofCell*>(tofCell[i]);
    aHit->setTrayIndex(aCell->trayIndex());
    aHit->setModuleIndex(aCell->moduleIndex());
    aHit->setCellIndex(aCell->cellIndex());
    aHit->setCellCollIndex(i);

    int daqId = aCell->daqIndex();
    aHit->setDaqIndex(daqId);



    Int_t letdc = (Int_t)aCell->tdc();
    int bin = int(letdc)&0x03ff;
    double tmptdc_f = letdc + GetINLcorr(4, daqId, bin);
    double letime  = tmptdc_f * VHRBIN2PS / 1000.; // ns

    Int_t tetdc = (Int_t)aCell->adc();
    bin = int(tetdc)&0x0ff;
    tmptdc_f = tetdc + GetINLcorr(5, daqId, bin);
    double tetime = tmptdc_f * HRBIN2PS / 1000.; // ns

    double tot = tetime - letime; // ns
    double tof = letime - T0;
    Double_t zhit = (Double_t)aCell->zHit();
    
    StTrack *thisTrack = aCell->associatedTrack();
    aHit->setAssociatedTrack(thisTrack);
    StTrackGeometry *theTrackGeometry = 
      (mOuterGeometry)?thisTrack->outerGeometry():thisTrack->geometry();
    Double_t L = tofPathLength(&vtx, &aCell->position(), theTrackGeometry->helix().curvature());
    aHit->setPathLength((Float_t)L);

    if(fabs(tof)>200.) { // +/-200 ns window 
      gMessMgr->Info("","OS") << " the hit is not from this event!" << endm;
      delete aHit;
      continue;
    }
    if(mValidCalibPar&&mValidStartTime) {
      int moduleChan = (aCell->moduleIndex()-1)*6 + (aCell->cellIndex()-1);
      Double_t tofcorr = tofr5AllCorr(tof, tot, zhit, moduleChan);
      aHit->setTimeOfFlight((Float_t)tofcorr);
      
      Double_t beta = L/(tofcorr*(C_C_LIGHT/1.e9));
      aHit->setBeta((Float_t)beta);
      
      const StThreeVectorF momentum = thisTrack->geometry()->momentum();
      Double_t ptot = momentum.mag();
      Double_t tofe = L/(C_C_LIGHT/1.e9)*sqrt(ptot*ptot+M_ELECTRON*M_ELECTRON)/ptot;
      Double_t tofpi = L/(C_C_LIGHT/1.e9)*sqrt(ptot*ptot+M_PION_PLUS*M_PION_PLUS)/ptot;
      Double_t tofk = L/(C_C_LIGHT/1.e9)*sqrt(ptot*ptot+M_KAON_PLUS*M_KAON_PLUS)/ptot;
      Double_t tofp = L/(C_C_LIGHT/1.e9)*sqrt(ptot*ptot+M_PROTON*M_PROTON)/ptot;
      
      aHit->setTofExpectedAsElectron((Float_t)tofe);
      aHit->setTofExpectedAsPion((Float_t)tofpi);
      aHit->setTofExpectedAsKaon((Float_t)tofk);
      aHit->setTofExpectedAsProton((Float_t)tofp);

      float sigmae = 999.;
      float sigmapi = 999.;
      float sigmak = 999.;
      float sigmap = 999.;
      float res = 0.095;  // 95 ps by default
      if(fabs(res)>1.e-5) {
	sigmae = (Float_t)((tofcorr-tofe)/res);
	sigmapi = (Float_t)((tofcorr-tofpi)/res);
	sigmak = (Float_t)((tofcorr-tofk)/res);
	sigmap = (Float_t)((tofcorr-tofp)/res);
      }
      aHit->setSigmaElectron(sigmae);
      aHit->setSigmaPion(sigmapi);
      aHit->setSigmaKaon(sigmak);
      aHit->setSigmaProton(sigmap);

    } else {
      aHit->setTimeOfFlight(9999.);
      aHit->setBeta(9999.);
    }

    tofHit->push_back(aHit); 
  }


  // store the hit collection in StEvent
  for (size_t j=0;j<tofHit->size();j++){
    theTof->addHit(tofHit->getHit(j)); 
    if (Debug())
      gMessMgr->Info("","OS") << "storing " << j << "  " << "  tray:"
			      << tofHit->getHit(j)->trayIndex() << "  module:"
			      << tofHit->getHit(j)->moduleIndex() << "  cell:"
			      << tofHit->getHit(j)->cellIndex() << endm;
  }

  // add the tof pidtraits
  StSPtrVecTofHit& tofHitVec = theTof->tofHits();
  StSPtrVecTrackNode& nodes = mEvent->trackNodes();
  for (unsigned int iNode=0; iNode<nodes.size(); iNode++){
    StTrack *theTrack = nodes[iNode]->track(primary);
    if(!theTrack) continue;
    Int_t trkId = theTrack->key();
    for (size_t j=0;j<tofHitVec.size();j++){
      StTrack *aTrack = tofHitVec[j]->associatedTrack();
      if(!aTrack) continue;
      if(aTrack->key()!=trkId) continue;
      StTofPidTraits* pidTof = new StTofPidTraits(tofHitVec[j]->trayIndex(), tofHitVec[j]->moduleIndex(), tofHitVec[j]->cellIndex(), tofHitVec[j]->timeOfFlight(), tofHitVec[j]->pathLength(), tofHitVec[j]->beta());
      pidTof->setSigmaElectron(tofHitVec[j]->sigmaElectron());
      pidTof->setSigmaPion(tofHitVec[j]->sigmaPion());
      pidTof->setSigmaKaon(tofHitVec[j]->sigmaKaon());
      pidTof->setSigmaProton(tofHitVec[j]->sigmaProton());
      
      theTrack->addPidTraits(pidTof);
    }
  }

  delete tofHit;
  
  return kStOK;
}

//____________________________________________________________________________
Int_t StTofCalibMaker::processEventYear8(){

  mEvent = (StEvent *) GetInputDS("StEvent");
  
  // event selection  // no primary vertex in run8
  if( !mEvent ||
      !mEvent->tofCollection() ||
      (!mEvent->tofCollection()->dataPresent() &&
       !mEvent->tofCollection()->rawdataPresent()) ||
      (!mEvent->tofCollection()->cellsPresent()) ) {
    gMessMgr->Info("","OS") << "StTofCalibMaker -- nothing to do ... bye-bye" << endm;
    return kStOK;
  }

  StTofCollection *theTof = mEvent->tofCollection();
  StSPtrVecTofCell &tofCell = theTof->tofCells();
  Int_t ncells = tofCell.size();
  gMessMgr->Info("","OS") << " TOFr matched cells + upVPD fired tubes : " << ncells << endm;

  
  //added by Zebo, projVtxZ from track cloest to beam line
  float dcaRmin = 9999;
  for(int i=0;i<ncells;i++) {
    StTofHit *aHit = new StTofHit;
    StTofCell *aCell = dynamic_cast<StTofCell*>(tofCell[i]);
    int trayId = aCell->trayIndex();
    if(trayId<=0 || trayId>120) continue;
   StTrack *thisTrack = aCell->associatedTrack();
    aHit->setAssociatedTrack(thisTrack);
    StTrackGeometry *theTrackGeometry = thisTrack->geometry();

    StThreeVectorD tofPos =  theTrackGeometry->helix().at(theTrackGeometry->helix().pathLengths(*mBeamHelix).first);
    LOG_INFO<<"tofPos(x,y,z)= "<<tofPos.x()<<"  "<<tofPos.y()<<"  "<<tofPos.z()<<endm;
    StThreeVectorD dcatof = tofPos - mBeamHelix->at(theTrackGeometry->helix().pathLengths(*mBeamHelix).second);
    LOG_INFO<<"dcatof(x,y)= "<<dcatof.x()<<"  "<<dcatof.y()<<endm;

    if(dcaRmin>dcatof.perp()) {
       mProjVtxZ = tofPos.z();
       dcaRmin = dcatof.perp();
    }
    delete aHit;
  }
  //end

  //-------------------------------------------------
  // pVPD calibration and calculate the start timing
  //-------------------------------------------------
  for(int i=0;i<2*mNVPD;i++) {
    mVPDLeTime[i] = -999.;
    mVPDTot[i] = -999.;
  }
  for(int i=0;i<ncells;i++) {
    StTofCell *aCell = dynamic_cast<StTofCell*>(tofCell[i]);
    if(!aCell) continue;
    int trayId = aCell->trayIndex();

    if(trayId!=mWestVpdTrayId && trayId!=mEastVpdTrayId) continue;

    int tubeId = aCell->cellIndex();
    mVPDLeTime[(trayId-121)*mNVPD+(tubeId-1)] = aCell->leadingEdgeTime();
    mVPDTot[(trayId-121)*mNVPD+(tubeId-1)] = aCell->tot();

    // T0 is calculated one-by-one according to different vz.    
//     Double_t T0 = tstart5(mVPDTot, mVPDLeTime, vz);
//     if(T0<0.) {
//       gMessMgr->Info(" Not a good PVPD-required event!","OS");
//       mValidStartTime = kFALSE;
//       //    return kStOK;
//     } 
  }
  tsum8(mVPDTot, mVPDLeTime);
  mTStart = tstart8(mProjVtxZ);

  gMessMgr->Info("","OS") << " NWest = " << mNWest << " NEast = " << mNEast << " TdcSum West = " << mTSumWest << " East = " << mTSumEast << endm;
  if(mTStart<-1000.) {
    gMessMgr->Info("","OS") << " mTStart not available!" << endm;
    mValidStartTime = kFALSE;
  } else {
    mValidStartTime = kTRUE;
  }
  gMessMgr->Info("","OS") << " mValidCalibPar = " << mValidCalibPar << " mValidStartTime = " << mValidStartTime << endm;

  /// Fill vpd information in StTofCollection
  theTof->setVpdEast(mVPDHitPatternEast);
  theTof->setVpdWest(mVPDHitPatternWest);
  theTof->setTdiff(mTDiff);
  theTof->setVzVpd(mVPDVtxZ);
  theTof->setTstart(mTStart);

  gMessMgr->Info("","OS") << " TofCollection: NWest = " << theTof->numberOfVpdWest() << " NEast = " << theTof->numberOfVpdEast() << endm;
  gMessMgr->Info("","OS") << "Tdiff = " << mTDiff <<" vpd vz = " << mVPDVtxZ << " proj vz = " << mProjVtxZ<<endm;
 
  StTofHitCollection *tofHit = new StTofHitCollection();

  //---------------------------------------
  // Tofr calibrations cells -> hits
  //---------------------------------------

  for(int i=0;i<ncells;i++) {
    StTofHit *aHit = new StTofHit;
    StTofCell *aCell = dynamic_cast<StTofCell*>(tofCell[i]);
    int trayId = aCell->trayIndex();
    if(trayId<=0 || trayId>120) continue;

    aHit->setTrayIndex(aCell->trayIndex());
    aHit->setModuleIndex(aCell->moduleIndex());
    aHit->setCellIndex(aCell->cellIndex());
    aHit->setCellCollIndex(i);

    int daqId = aCell->daqIndex();
    aHit->setDaqIndex(daqId);

    StTrack *thisTrack = aCell->associatedTrack();
    aHit->setAssociatedTrack(thisTrack);
    StTrackGeometry *theTrackGeometry = thisTrack->geometry();

    StThreeVectorD tofPos =  theTrackGeometry->helix().at(theTrackGeometry->helix().pathLengths(*mBeamHelix).first);
    StThreeVectorD dcatof = tofPos - mBeamHelix->at(theTrackGeometry->helix().pathLengths(*mBeamHelix).second);

    Double_t L = tofPathLength(&tofPos, &aCell->position(), theTrackGeometry->helix().curvature());
    aHit->setPathLength((Float_t)L);

    //double mTStart = tstart8(dcatof.z());
    gMessMgr->Info("","OS") << " p(x,y,z) = "<<theTrackGeometry->momentum().x()<<"  "<<theTrackGeometry->momentum().y()<<"  "<<theTrackGeometry->momentum().z()<<endm;
    gMessMgr->Info("","OS") << " Project Z = " << tofPos.z() << " mTStart = " << mTStart << endm;

    double tot = aCell->tot(); // ns
    double tdc = aCell->leadingEdgeTime();
    tdc -= mPhaseOffset8;
    while(tdc>TMAX) tdc -= TMAX;
    double tof = tdc - mTStart;
    Double_t zhit = (Double_t)aCell->zHit();

    
    if(mValidCalibPar&&mValidStartTime) {
      int moduleChan = (aCell->moduleIndex()-1)*6 + (aCell->cellIndex()-1);
      Double_t tofcorr = tofr8AllCorr(tof, tot, zhit, trayId, moduleChan);
      aHit->setTimeOfFlight((Float_t)tofcorr);
      
      Double_t beta = L/(tofcorr*(C_C_LIGHT/1.e9));
      aHit->setBeta((Float_t)beta);
      
      const StThreeVectorF momentum = thisTrack->geometry()->momentum();
      Double_t ptot = momentum.mag();
      Double_t tofe = L/(C_C_LIGHT/1.e9)*sqrt(ptot*ptot+M_ELECTRON*M_ELECTRON)/ptot;
      Double_t tofpi = L/(C_C_LIGHT/1.e9)*sqrt(ptot*ptot+M_PION_PLUS*M_PION_PLUS)/ptot;
      Double_t tofk = L/(C_C_LIGHT/1.e9)*sqrt(ptot*ptot+M_KAON_PLUS*M_KAON_PLUS)/ptot;
      Double_t tofp = L/(C_C_LIGHT/1.e9)*sqrt(ptot*ptot+M_PROTON*M_PROTON)/ptot;
      
      aHit->setTofExpectedAsElectron((Float_t)tofe);
      aHit->setTofExpectedAsPion((Float_t)tofpi);
      aHit->setTofExpectedAsKaon((Float_t)tofk);
      aHit->setTofExpectedAsProton((Float_t)tofp);

      float sigmae = 999.;
      float sigmapi = 999.;
      float sigmak = 999.;
      float sigmap = 999.;
      float res = 0.085;  // 85 ps by default
      if(fabs(res)>1.e-5) {
	sigmae = (Float_t)((tofcorr-tofe)/res);
	sigmapi = (Float_t)((tofcorr-tofpi)/res);
	sigmak = (Float_t)((tofcorr-tofk)/res);
	sigmap = (Float_t)((tofcorr-tofp)/res);
      }
      aHit->setSigmaElectron(sigmae);
      aHit->setSigmaPion(sigmapi);
      aHit->setSigmaKaon(sigmak);
      aHit->setSigmaProton(sigmap);

    } else {
      aHit->setTimeOfFlight(9999.);
      aHit->setBeta(9999.);
    }

    tofHit->push_back(aHit); 
  }


  // store the hit collection in StEvent
  for (size_t j=0;j<tofHit->size();j++){
    theTof->addHit(tofHit->getHit(j)); 
    if (Debug())
      gMessMgr->Info("","OS") << "storing " << j << "  " << "  tray:"
			      << tofHit->getHit(j)->trayIndex() << "  module:"
			      << tofHit->getHit(j)->moduleIndex() << "  cell:"
			      << tofHit->getHit(j)->cellIndex() << "  TOF:"
                              << tofHit->getHit(j)->timeOfFlight() << "  beta:"
                              << tofHit->getHit(j)->beta() << endm;
  }

  // add the tof pidtraits
  StSPtrVecTofHit& tofHitVec = theTof->tofHits();
  StSPtrVecTrackNode& nodes = mEvent->trackNodes();
  for (unsigned int iNode=0; iNode<nodes.size(); iNode++){
    StTrack *theTrack = nodes[iNode]->track(primary);
    if(!theTrack) continue;
    Int_t trkId = theTrack->key();
    for (size_t j=0;j<tofHitVec.size();j++){
      StTrack *aTrack = tofHitVec[j]->associatedTrack();
      if(!aTrack) continue;
      if(aTrack->key()!=trkId) continue;
      StTofPidTraits* pidTof = new StTofPidTraits(tofHitVec[j]->trayIndex(), tofHitVec[j]->moduleIndex(), tofHitVec[j]->cellIndex(), tofHitVec[j]->timeOfFlight(), tofHitVec[j]->pathLength(), tofHitVec[j]->beta());
      pidTof->setSigmaElectron(tofHitVec[j]->sigmaElectron());
      pidTof->setSigmaPion(tofHitVec[j]->sigmaPion());
      pidTof->setSigmaKaon(tofHitVec[j]->sigmaKaon());
      pidTof->setSigmaProton(tofHitVec[j]->sigmaProton());
      
      theTrack->addPidTraits(pidTof);
    }
  }

  delete tofHit;
  
  return kStOK;
}

//_____________________________________________________________________________
Double_t StTofCalibMaker::tofrT0Corr(const Double_t tof, const Double_t Tstart, const Int_t iDaqChan)
{
  return tof - Tstart - mTofrT0[iDaqChan];
}

//_____________________________________________________________________________
Double_t StTofCalibMaker::tofrSlewingCorr(const Double_t tof, const Double_t adc, const Int_t iDaqChan)
{
  Double_t par[mNPar];
  for(int i=0;i<mNPar;i++) {
    par[i] = mTofrTAPar[iDaqChan*mNPar+i];
  }

  mTofrSlewing->SetParameters(par);
  return tof - mTofrSlewing->Eval(adc);

}

//_____________________________________________________________________________
Double_t StTofCalibMaker::tofrZCorr(const Double_t tof, const Double_t zhit)
{
  mTofrZCorr->SetParameters(mTofrZPar);
  return tof - mTofrZCorr->Eval(zhit);
}

//_____________________________________________________________________________
Double_t StTofCalibMaker::tofrAllCorr(const Double_t tof, const Double_t T0, const Double_t adc, const Double_t z, const Int_t iDaqChan)
{
  gMessMgr->Info("","OS") << "\nStTofCalibMaker::tofrAllCorr: Tofr calibrating...\n" 
			  << "\tDoing Calibration in TOFr Channel " << iDaqChan 
			  << "\n\tinput tof = " << tof << "   Tstart = " 
			  << T0 << "  ADC = " << adc << "  Zlocal = " << z 
			  << "\n" << endm;
  
  Double_t tofcorr = tofrT0Corr( tof, T0, iDaqChan );
  gMessMgr->Info("","OS") << " T0 corr: tofcorr = " << tofcorr << endm;
  Double_t tofcorr2 = tofrSlewingCorr( tofcorr, adc, iDaqChan );
  gMessMgr->Info("","OS") << " Slewing corr: tofcorr2 = " << tofcorr2 << endm;
  Double_t tofcorr3 = tofrZCorr( tofcorr2, z);
  gMessMgr->Info("","OS") << " Z position corr: tofcorr3 = " << tofcorr3 << endm;
  return tofcorr3;
}

//_____________________________________________________________________________
Double_t StTofCalibMaker::tofpT0Corr(const Double_t tof, const Double_t Tstart, const Int_t iDaqChan)
{
  return tof - Tstart - mTofpT0[iDaqChan];
}

//_____________________________________________________________________________
Double_t StTofCalibMaker::tofpSlewingCorr(const Double_t tof, const Double_t adc, const Int_t iDaqChan)
{
  Double_t par[mNPar];
  for(int i=0;i<mNPar;i++) {
    par[i] = mTofpTAPar[iDaqChan*mNPar+i];
  }

  mTofpSlewing->SetParameters(par);
  return tof - mTofpSlewing->Eval(adc);

}

//_____________________________________________________________________________
Double_t StTofCalibMaker::tofpZCorr(const Double_t tof, const Double_t zhit)
{
  mTofpZCorr->SetParameters(mTofpZPar);
  return tof - mTofpZCorr->Eval(zhit);
}

//_____________________________________________________________________________
Double_t StTofCalibMaker::tofpAllCorr(const Double_t tof, const Double_t T0, const Double_t adc, const Double_t z, const Int_t iDaqChan)
{
  gMessMgr->Info("","OS") << "\nStTofCalibMaker::tofpAllCorr: Tofp calibrating...\n" 
			  << "\tDoing Calibration in TOFp Channel " << iDaqChan 
			  << "\n\tinput tof = " << tof << "   Tstart = " 
			  << T0 << "  ADC = " << adc << "  Zlocal = " << z 
			  << "\n" << endm;
  
  Double_t tofcorr = tofpT0Corr( tof, T0, iDaqChan );
  gMessMgr->Info("","OS") << " T0 corr: tofcorr = " << tofcorr << endm;
  Double_t tofcorr2 = tofpSlewingCorr( tofcorr, adc, iDaqChan );
  gMessMgr->Info("","OS") << " Slewing corr: tofcorr2 = " << tofcorr2 << endm;
  Double_t tofcorr3 = tofpZCorr( tofcorr2, z);
  gMessMgr->Info("","OS") << " Z position corr: tofcorr3 = " << tofcorr3 << endm;
  return tofcorr3;
}

//_____________________________________________________________________________
Double_t StTofCalibMaker::tofr5AllCorr(const Double_t tof, const Double_t tot, const Double_t z, const Int_t iModuleChan)
{
  int module = iModuleChan/6 + 1;
  int cell = iModuleChan%6 + 1;
  gMessMgr->Info("","OS") << "\nStTofCalibMaker::tofr5AllCorr: Tofr5 calibrating...\n" 
			  << "\tDoing Calibration in TOFr5 Module " << module << " Cell " << cell
			  << "\n\tinput tof = " << tof
			  << "  TOT = " << tot << "  Zlocal = " << z 
			  << "\n" << endm;
  
  Double_t tofcorr = tof;

  int iTotBin = -1;
  for(int i=0;i<mNBinMax-1;i++) {
    if(tot>=mTofr5TotEdge[iModuleChan][i] && tot<mTofr5TotEdge[iModuleChan][i+1]) {
      iTotBin = i;
      break;
    }
  }
  if(iTotBin>=0&&iTotBin<mNBinMax) {
    tofcorr -= mTofr5TotCorr[iModuleChan][iTotBin];
  } else {
    tofcorr = -9999.;
  }

  int iZBin = -1;
  for(int i=0;i<mNBinMax-1;i++) {
    if(z>=mTofr5ZEdge[iModuleChan][i] && z<mTofr5ZEdge[iModuleChan][i+1]) {
      iZBin = i;
      break;
    }
  }
  if(iZBin>=0&&iZBin<mNBinMax) {
    tofcorr -= mTofr5ZCorr[iModuleChan][iZBin];
  } else {
    tofcorr = -9999.;
  }

  gMessMgr->Info("","OS") << "  Corrected tof: tofcorr = " << tofcorr << endm;
  return tofcorr;
}

//_____________________________________________________________________________
Double_t StTofCalibMaker::tofr8AllCorr(const Double_t tof, const Double_t tot, const Double_t z, const Int_t iTray, const Int_t iModuleChan)
{
  int tray = iTray;
  int module = iModuleChan/6 + 1;
  int cell = iModuleChan%6 + 1;
  int board = iModuleChan/24 + 1;
  gMessMgr->Info("","OS") << "\nStTofCalibMaker::tofr8AllCorr: Tofr8 calibrating...\n" 
			  << "\tDoing Calibration in TOFr8 Tray " << tray << " Module " << module << " Cell " << cell
			  << "\n\tinput tof = " << tof
			  << "  TOT = " << tot << "  Zlocal = " << z 
			  << "\n" << endm;
  
  Double_t tofcorr = tof;

  tofcorr -= mTofTZero[tray-1][module-1][cell-1];

  if(Debug()) gMessMgr->Info("","OS") << "T0 correction: "<<mTofTZero[tray-1][module-1][cell-1]<<endm;

  if(mSlewingCorr) {
    int iTotBin = -1;
    for(int i=0;i<mNBinMax-1;i++) {
      if(tot>=mTofTotEdge[tray-1][board-1][i] && tot<mTofTotEdge[tray-1][board-1][i+1]) {
	iTotBin = i;
	break;
      }
    }
    if(iTotBin>=0&&iTotBin<mNBinMax) {
      double x1 = mTofTotEdge[tray-1][board-1][iTotBin];
      double x2 = mTofTotEdge[tray-1][board-1][iTotBin+1];
      double y1 = mTofTotCorr[tray-1][board-1][iTotBin];
      double y2 = mTofTotCorr[tray-1][board-1][iTotBin+1];
      double dcorr = y1 + (tot-x1)*(y2-y1)/(x2-x1);
      if(Debug()) gMessMgr->Info("","OS") << "TOT correction: "<<dcorr<<endm;

      tofcorr -= dcorr;
    } else {
      gMessMgr->Info("","OS") << " TOT out of range! EXIT! " << endm;
      return -9999.;
    }

    int iZBin = -1;
    for(int i=0;i<mNBinMax-1;i++) {
      if(z>=mTofZEdge[tray-1][board-1][i] && z<mTofZEdge[tray-1][board-1][i+1]) {
	iZBin = i;
	break;
      }
    }
    if(iZBin>=0&&iZBin<mNBinMax) {
      double x1 = mTofZEdge[tray-1][board-1][iZBin];
      double x2 = mTofZEdge[tray-1][board-1][iZBin+1];
      double y1 = mTofZCorr[tray-1][board-1][iZBin];
      double y2 = mTofZCorr[tray-1][board-1][iZBin+1];
//      double dcorr = y1 + (tot-x1)*(y2-y1)/(x2-x1);
      double dcorr = y1 + (z-x1)*(y2-y1)/(x2-x1);

      tofcorr -= dcorr;
     if(Debug()) gMessMgr->Info("","OS") << "zHit correction: "<<dcorr<<endm;

    } else {
      gMessMgr->Info("","OS") << " Z our of range! EXIT! " << endm;
      return -9999.;
    }
    
  }

  gMessMgr->Info("","OS") << "  Corrected tof: tofcorr = " << tofcorr << endm;
  return tofcorr;
}

//_____________________________________________________________________________
Double_t StTofCalibMaker::tstart(const Double_t *adc, const Double_t *tdc, const Double_t vz)
{
  /// start timing calculation, default pVPD fired cut is 1,1
  /// however, the calibration parameters are from 3,3 set
  Double_t Tstart = 9999.;

  Int_t Ieast = 0, Iwest = 0;
  Double_t TdcSumEast = 0., TdcSumWest = 0.;
  Double_t ped = 0.0;
  for(int i=0;i<3;i++) {
    if( validPVPDADC(adc[i]) && validPVPDTDC(tdc[i]) ) {
      Ieast++;
      Double_t par[mNPar];
      for(int j=0;j<mNPar;j++) {
	par[j] = mPVPDTAPar[i*mNPar+j];
      }
      mPVPDSlewing->SetParameters(par);
      mPVPDTdc[i] = tdc[i] - mPVPDSlewing->Eval(adc[i]-ped);
      TdcSumEast += mPVPDTdc[i];
    }

    if( validPVPDADC(adc[i+3]) && validPVPDTDC(tdc[i+3]) ) {
      Iwest++;
      Double_t par[mNPar];
      for(int j=0;j<mNPar;j++) {
	par[j] = mPVPDTAPar[(i+3)*mNPar+j];
      }
      mPVPDSlewing->SetParameters(par);
      mPVPDTdc[i+3] = tdc[i+3] - mPVPDSlewing->Eval(adc[i+3]-ped);
      TdcSumWest += mPVPDTdc[i+3];
    }
  }

  Double_t TdcSum = TdcSumEast + TdcSumWest;
  
  //  if ( Ieast>=mPVPDEastHitsCut && Iwest>=mPVPDWestHitsCut ) {
  /// To include those runs with east PVPD dead
  if ( (Ieast>=mPVPDEastHitsCut || !mEastPVPDValid) && Iwest>=mPVPDWestHitsCut ) {
    Tstart = (TdcSum*mTDCWidth-(Ieast-Iwest)*vz/(C_C_LIGHT/1.e9))/(Ieast+Iwest);
  }

  return Tstart;
}
//_____________________________________________________________________________
Double_t StTofCalibMaker::tstart5(const Double_t *tot, const Double_t *time, const Double_t vz)
{
  /// start timing calculation, default pVPD fired cut is 1,1
  /// however, the calibration parameters are from 3,3 set
  Double_t Tstart = -999999.;

  Int_t Ieast = 0, Iwest = 0;
  Double_t TSumEast = 0., TSumWest = 0.;
  for(int i=0;i<3;i++) {
    if(Debug()) gMessMgr->Info("","OS") << " East pVPD tot = " << tot[i] << " time = " << time[i] << endm;
    if(Debug()) gMessMgr->Info("","OS") << " West pVPD tot = " << tot[i+3] << " time = " << time[i+3] << endm;

    if( time[i]>0. ) {
      int ibin = -1;
      for(int j=0;j<mNBinMax-1;j++) {
	if(tot[i]>=mPVPDTotEdge[i][j] && tot[i]<mPVPDTotEdge[i][j+1]) {
	  ibin = j;
	  break;
	}
      }
      if(ibin>=0&&ibin<mNBinMax) {
	Ieast++;
	mPVPDLeTime[i] = time[i] - mPVPDTotCorr[i][ibin];
	TSumEast += mPVPDLeTime[i];
      }
    }

    if( time[i+3]>0. ) {
      int ibin = -1;
      for(int j=0;j<mNBinMax-1;j++) {
	if(tot[i+3]>=mPVPDTotEdge[i+3][j] && tot[i+3]<mPVPDTotEdge[i+3][j+1]) {
	  ibin = j;
	  break;
	}
      }
      if(ibin>=0&&ibin<mNBinMax) {
	Iwest++;
	mPVPDLeTime[i+3] = time[i+3] - mPVPDTotCorr[i+3][ibin];
	TSumWest += mPVPDLeTime[i+3];
      }
    }
  }

  Double_t TSum = TSumEast + TSumWest;
  
  if ( Ieast>=mPVPDEastHitsCut && Iwest>=mPVPDWestHitsCut ) {
    Tstart = (TSum-(Ieast-Iwest)*vz/(C_C_LIGHT/1.e9))/(Ieast+Iwest);
  }

  return Tstart;
}

//_____________________________________________________________________________
void StTofCalibMaker::tsum8(const Double_t *tot, const Double_t *time)
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

  if ( mNEast>=mPVPDEastHitsCut && mNWest>=mPVPDWestHitsCut ) {
    mTDiff = (mTSumEast/mNEast - mTSumWest/mNWest)/2. - mProjVtxZ/(C_C_LIGHT/1.e9);
    mVPDVtxZ = (mTSumEast/mNEast - mTSumWest/mNWest)/2.*(C_C_LIGHT/1.e9);
  }
  
  return;
}
//_____________________________________________________________________________
Double_t StTofCalibMaker::tstart8(const Double_t vz)
{
  Double_t Tstart = -9999.;

  Double_t TSum = mTSumEast + mTSumWest;
  
  if ( mNEast>=mPVPDEastHitsCut && mNWest>=mPVPDWestHitsCut ) {
    Tstart = (TSum-(mNEast-mNWest)*vz/(C_C_LIGHT/1.e9))/(mNEast+mNWest);
  }

  return Tstart;
}

//_____________________________________________________________________________
Double_t StTofCalibMaker::GetINLcorr(const int edgeflag,const int tdcchan,const int bin)
{
  int iboard=tdcchan/24;
  int chan = (tdcchan%24);
  int itdc=0;
  if(edgeflag==4) itdc = chan/8;  // leading edge
  if(edgeflag==5) itdc = 3;       // trailing edge
  if(tdcchan==192&&edgeflag==4) itdc=0;
  if(tdcchan==193&&edgeflag==4) itdc=1;
  if(tdcchan==194&&edgeflag==4) itdc=1;
  if(tdcchan==195&&edgeflag==4) itdc=0;
  if(tdcchan==196&&edgeflag==4) itdc=1;
  if(tdcchan==197&&edgeflag==4) itdc=1;

  return mINLtable[iboard][itdc][bin];
}

/*****************************************************************
 *
 * $Log: StTofCalibMaker.cxx,v $
 * Revision 1.24  2018/02/26 23:26:50  smirnovd
 * StTof: Remove outdated ClassImp macro
 *
 * Revision 1.23  2018/02/26 23:13:19  smirnovd
 * Move embedded CVS log messages to the end of file
 *
 * Revision 1.22  2012/12/14 06:35:48  geurts
 * Changed global database calls to direct table access and/or removed deprecated database access code.
 *
 * Revision 1.21  2011/05/27 18:25:32  genevb
 * Propagate StTrack::key => Int_t to other codes
 *
 * Revision 1.20  2008/09/03 22:30:43  dongx
 * mTStart added, applicable for Run8
 *
 * Revision 1.19  2008/07/30 20:03:03  dongx
 * mBeamLine initialized in constructor to avoid null pointer when no calibration tables
 *
 * Revision 1.18  2008/07/22 00:16:47  dongx
 * initialization added for global variables in StTofCollection in each event
 *
 * Revision 1.17  2008/06/25 21:45:29  dongx
 * Reset time values when tot/z is out of range
 *
 * Revision 1.16  2008/06/24 21:33:37  dongx
 * Added the filling of vzvpd into StTofCollection
 *
 * Revision 1.15  2008/06/17 17:49:19  dongx
 * Update for Run 8 - first release
 *
 * Revision 1.14  2007/11/30 17:11:23  dongx
 * removed tdcId in tofZCorr for tofZCorr.tdcId is not defined in db, and removed from idl definition
 *
 * Revision 1.13  2007/03/13 15:09:10  dongx
 * Remove breaking of failure on number of return rows during db I/O for tofTotCorr and tofZCorr
 *
 * Revision 1.12  2007/03/05 18:51:02  dongx
 * updated for Run V CuCu calibration
 *  - INL correction moved in this maker
 *  - Tot Corr and Z Corr use new tables in data base
 *  - pVPD calibrated information cannot be fully stored within current infrastructure, need update on TofCollection. Configurations better than (1,1) are all selected.
 *
 * Revision 1.11  2005/04/12 17:33:47  dongx
 * update for year 5 data. not completed, leave as empty now.
 *
 * Revision 1.10  2004/09/20 16:07:35  dongx
 * correct the nsigma in StTofPidTraits
 *
 * Revision 1.9  2004/08/13 00:15:03  dongx
 * correct the nSigmaXXX calculation
 *
 * Revision 1.8  2004/08/11 19:35:40  dongx
 * loose the ADC cut of tofp
 *
 * Revision 1.7  2004/08/11 18:58:40  dongx
 * missing nSigmaXX in tofHit implemented
 *
 * Revision 1.6  2004/07/24 03:33:56  dongx
 * Tofp slewing function changed back
 *
 * Revision 1.5  2004/07/16 18:28:17  dongx
 * -Tofp Slewing function changed in AuAu200 GeV Run IV
 * -Include those runs with eastern PVPD dead
 *
 * Revision 1.4  2004/07/16 15:06:08  dongx
 * Z correction function separated for TOFp and TOFr.
 * Use a new one for RunIV AuAu 200GeV runs
 *
 * Revision 1.3  2004/07/15 18:11:22  dongx
 *  -introduce two new tables in dbase: tofAdcRange & tofResolution
 *  -continue update on writing StTofPidTraits
 *
 * Revision 1.2  2004/07/08 18:26:09  dongx
 * filling StTofPidTraits added (not completed, null nsigmaXXX now)
 *
 * Revision 1.1  2004/07/01 17:23:48  dongx
 * first release
 */
