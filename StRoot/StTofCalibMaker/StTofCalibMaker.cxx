/*******************************************************************
 *
 * $Id: StTofCalibMaker.cxx,v 1.8 2004/08/11 19:35:40 dongx Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: - Tof Calibration Maker to do the calibration for pVPD 
 *              (start timing) , TOFp and TOFr
 *              - store into StTofHit
 *              - if no valid calibration parameters, store matched hits
 *
 *****************************************************************
 *
 * $Log: StTofCalibMaker.cxx,v $
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

#ifdef __ROOT__
ClassImp(StTofCalibMaker)
#endif

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
  mTofpGeom = new StTofGeometry();
  mTofpGeom->init(this);

  Int_t val = kStOK;
  val = initParameters();
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
Int_t StTofCalibMaker::initParameters()
{
  /// initialize the calibrations parameters from dbase
  /// read in and check the size
  gMessMgr->Info("    -- retrieving run parameters from Calibrations_tof","OS");
  TDataSet *mDbDataSet = GetDataBase("Calibrations/tof");
  if (!mDbDataSet){
    gMessMgr->Error("unable to get TOF run parameters","OS");
    return kStErr;
  }

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

  return kStOK;
}

//____________________________________________________________________________
Int_t StTofCalibMaker::FinishRun(int runnumber)
{
  
  if(mTofpGeom) delete mTofpGeom;
  mTofpGeom = 0;
  
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

  mYear2 = (mEvent->runId()<4000000);
  mYear3 = (mEvent->runId()>4000000&&mEvent->runId()<5000000);
  mYear4 = (mEvent->runId()>5000000);
  
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
	sigmae = (Float_t)((tof-tofe)/res);
	sigmapi = (Float_t)((tof-tofpi)/res);
	sigmak = (Float_t)((tof-tofk)/res);
	sigmap = (Float_t)((tof-tofp)/res);
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
	sigmae = (Float_t)((tof-tofe)/res);
	sigmapi = (Float_t)((tof-tofpi)/res);
	sigmak = (Float_t)((tof-tofk)/res);
	sigmap = (Float_t)((tof-tofp)/res);
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
    unsigned short trkId = theTrack->key();
    for (size_t j=0;j<tofHitVec.size();j++){
      StTrack *aTrack = tofHitVec[j]->associatedTrack();
      if(!aTrack) continue;
      if(aTrack->key()!=trkId) continue;
      StTofPidTraits* pidTof = new StTofPidTraits(tofHitVec[j]->trayIndex(), tofHitVec[j]->moduleIndex(), tofHitVec[j]->cellIndex(), tofHitVec[j]->timeOfFlight(), tofHitVec[j]->pathLength(), tofHitVec[j]->beta());
      pidTof->setSigmaElectron(tofHitVec[j]->sigmaElectron());
      pidTof->setSigmaPion(tofHitVec[j]->sigmaElectron());
      pidTof->setSigmaKaon(tofHitVec[j]->sigmaElectron());
      pidTof->setSigmaProton(tofHitVec[j]->sigmaElectron());
      
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


