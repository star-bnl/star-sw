/*******************************************************************
 *
 * $Id: StVpdCalibMaker.cxx,v 1.2 2009/12/04 22:22:17 geurts Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: - VPD Calibration Maker to do the calibration for upVPD 
 *              - store into StBTofHeader
 *
 *****************************************************************
 *
 * $Log: StVpdCalibMaker.cxx,v $
 * Revision 1.2  2009/12/04 22:22:17  geurts
 * two updates (Xin):
 * - use the new calibration table vpdTotCorr
 * - update on the vzVpdFinder(), a cut on timing diff is used to remove outliers.
 *
 * Revision 1.1  2009/11/09 20:55:54  geurts
 * first release
 *
 *
 *******************************************************************/
#include <iostream>
#include "TFile.h"

#include "StEvent.h"
#include "StBTofCollection.h"
#include "StBTofHit.h"
#include "StBTofHeader.h"
#include "StEventTypes.h"
#include "Stypes.h"
#include "StMessMgr.h"
#include "StThreeVectorD.hh"
#include "StEventUtilities/StuRefMult.hh"
#include "PhysicalConstants.h"
#include "phys_constants.h"
#include "tables/St_vpdTotCorr_Table.h"

#include "StBTofUtil/StBTofHitCollection.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuBTofHit.h"

#include "StVpdCalibMaker.h"

#include "StMessMgr.h"
#include "StMemoryInfo.hh"
#include "StTimer.hh"

#ifdef __ROOT__
ClassImp(StVpdCalibMaker)
#endif

/// Very High resolution mode, pico-second per bin
  const Double_t StVpdCalibMaker::VHRBIN2PS =  24.4140625; // 1000*25/1024 (ps/chn)
/// High resolution mode, pico-second per bin
  const Double_t StVpdCalibMaker::HRBIN2PS = 97.65625; // 97.65625= 1000*100/1024  (ps/chn)
/// TDC limit    
  const Double_t StVpdCalibMaker::TMAX = 51200.;
/// VZDIFFCUT:  VzVpd - VzProj cut
  const Double_t StVpdCalibMaker::VZDIFFCUT=6.;
/// TDIFFCUT: fabs(time - ave(others)) > cut, remove this hit
  const Double_t StVpdCalibMaker::TDIFFCUT=0.8;


//_____________________________________________________________________________
StVpdCalibMaker::StVpdCalibMaker(const Char_t *name) : StMaker(name)
{
  /// default constructor
  /// set the default parameters.
  /// Reset the calibration parameters
  setVPDHitsCut(1,1);

  //mEvent   = 0;
  mMuDst   = 0;
  mMuDstIn = kFALSE;   //! default is to read in StEvent
  mBTofColl       = 0;
  mValidCalibPar  = kFALSE;

  setCreateHistoFlag(kFALSE);
  setHistoFileName("vpdana.root");

  // default initialization from database
  mInitFromFile = kFALSE;
  // assign default locations and names to the calibration files 
  setCalibFilePvpd("/star/institutions/rice/calib/default/pvpdCali_4DB.dat");
}

//_____________________________________________________________________________
StVpdCalibMaker::~StVpdCalibMaker()
{ /* noop */ }

//_____________________________________________________________________________
void StVpdCalibMaker::resetPars()
{
  memset(mVPDTotEdge, 0, sizeof(mVPDTotEdge));
  memset(mVPDTotCorr, 0, sizeof(mVPDTotCorr));
}

//_____________________________________________________________________________
void StVpdCalibMaker::resetVpd()
{
  memset(mVPDLeTime, 0, sizeof(mVPDLeTime));
  memset(mVPDTot, 0, sizeof(mVPDTot));

  for(int i=0;i<MaxVpdVz;i++) {
    mVPDVtxZ[i] = -9999.;
  }
  mNVzVpd = 0;
  mVPDHitPatternEast = 0;
  mVPDHitPatternWest = 0;
}

//____________________________________________________________________________
Int_t StVpdCalibMaker::Init()
{
  resetPars();
  resetVpd();

  // m_Mode can be set by SetMode() method
  if(m_Mode) {
//    setHistoFileName("vpdana.root");
  } else {
    setHistoFileName("");    
  }

  if (mHisto){
    bookHistograms();
    LOG_INFO << "Histograms are booked" << endm;
    if (mHistoFileName!="") {
      LOG_INFO << "Histograms will be stored in " << mHistoFileName.c_str() << endm;
    }
  }

  //return kStOK;
  return StMaker::Init();
}

//____________________________________________________________________________
Int_t StVpdCalibMaker::InitRun(Int_t runnumber)
{
  // tof run configurations

  Int_t val = kStOK;
  val = initParameters(runnumber);
  if(val==kStOK) {
    mValidCalibPar = kTRUE;
    //return kStOK;
    return StMaker::InitRun(runnumber);
  } else {
    mValidCalibPar = kFALSE;
    LOG_WARN << " !!! No valid calibration parameters for VPD! " << endm;
    return kStWarn;
  }


}

//_____________________________________________________________________________
Int_t StVpdCalibMaker::initParameters(Int_t runnumber)
{
  /// initialize the calibrations parameters from dbase
  /// read in and check the size
  if (mInitFromFile){
    LOG_INFO << "Retrieving calibration parameters from files" << endm;

    /// open file and read VPD calibration parameters
    LOG_INFO << " - pvpd : " << mCalibFilePvpd << endm;  
    ifstream inData;
    inData.open(mCalibFilePvpd.c_str());
    int nchl, nbin;
    for(int i=0;i<NVPD*2;i++) {
      inData>>nchl;
      inData>>nbin;
      if (nbin>NBinMax) {
	LOG_ERROR << "nummer of bins (" << nbin << ") out of range ("
		  << NBinMax << ") for vpd channel " << i << endm;
	return kStErr;
      }
      for(int j=0;j<=nbin;j++) inData>>mVPDTotEdge[i][j];
      for(int j=0;j<=nbin;j++) inData>>mVPDTotCorr[i][j];
    }
    inData.close();
  }
  else {
    /// Get all calibration parameters from the database
    LOG_INFO << " Retrieving calibration parameters from Calibrations_tof" << endm;

    /// read in and check the size
    TDataSet *dbDataSet = GetDataBase("Calibrations/tof");
    if (!dbDataSet){
      LOG_ERROR  << "unable to get VPD/TOF run parameters" << endm;
      return kStErr;
    }
  
    LOG_INFO << "     loading calibration parameters ..." << endm;
    // read vpdTotCorr table
    St_vpdTotCorr* vpdTotCorr = static_cast<St_vpdTotCorr*>(dbDataSet->Find("vpdTotCorr"));
    if(!vpdTotCorr) {
      LOG_ERROR << "unable to get vpdTotCorr table parameters" << endm;
      //    assert(vpdTotCorr);
      return kStErr;
    }
    vpdTotCorr_st* totCorr = static_cast<vpdTotCorr_st*>(vpdTotCorr->GetArray());
    Int_t numRows = vpdTotCorr->GetNRows();

    if(numRows!=NVPD*2) {
      LOG_WARN  << " Mis-matched number of rows in vpdTotCorr table: " << numRows 
		<< " (exp:" << NVPD*2 << ")" << endm;
    }

    LOG_DEBUG << " Number of rows read in: " << numRows << " for Vpd ToT correction" << endm;

    for (Int_t i=0;i<numRows;i++) {
      short tubeId = totCorr[i].tubeId;
      // check index range
      if (tubeId>2*NVPD) {
	LOG_ERROR << "tubeId (" << tubeId << ") out of range ("
		  << 2*NVPD << ")" << endm;
	return kStErr;
      }

      for(Int_t j=0;j<NBinMax;j++) {
	mVPDTotEdge[tubeId-1][j] = totCorr[i].tot[j];
	mVPDTotCorr[tubeId-1][j] = totCorr[i].corr[j];
	LOG_DEBUG << " east/west: " << (tubeId-1)/NVPD << " tubeId: " << tubeId << endm;
      } // end j 0->NBinMax
    } // end i 0->numRows
  }


  return kStOK;
}


//_____________________________________________________________________________
Int_t StVpdCalibMaker::Finish()
{
  if (mHistoFileName!="") writeHistograms();
  return StMaker::Finish();//kStOK;
}


//_____________________________________________________________________________
Int_t StVpdCalibMaker::Make()
{
  LOG_INFO << " StVpdCalibMaker::Make: starting ..." << endm;
  if(mHisto&&mhEventCounter) mhEventCounter->Fill(0);
  if(!mValidCalibPar) {
    LOG_WARN << " No valid calibration parameters. Skip ... " << endm;
    return kStWarn;
  }

  if(mHisto&&mhEventCounter) mhEventCounter->Fill(1);
  resetVpd();
  bool loadOK = loadVpdData();

  if(!loadOK) return kStWarn;
  if(mHisto&&mhEventCounter) mhEventCounter->Fill(2);

  tsum(mVPDTot, mVPDLeTime);
  vzVpdFinder();

  if(mHisto) fillHistograms();
  if(mHisto&&mhEventCounter) mhEventCounter->Fill(3);
  bool writeOK = writeVpdData();
  if (!writeOK) return kStWarn;
  if(writeOK&&mHisto) mhEventCounter->Fill(4);

  return StMaker::Make(); //kStOK;
}


//_____________________________________________________________________________
Bool_t StVpdCalibMaker::writeVpdData() const
{
  StBTofHeader *tofHeader = 0;
  if(mMuDstIn) {
    if(!mMuDst) {
      LOG_WARN << " No MuDst to write ... " << endm;
      return kFALSE;
    }
    tofHeader = (StBTofHeader *) mMuDst->btofHeader();
  } else {
    //    if(!mEvent || !mBTofColl) {
    if(!mBTofColl) {
      LOG_WARN << " No StEvent/btofCollection to write ... " << endm;
      return kFALSE;
    }
    tofHeader = (StBTofHeader *) mBTofColl->tofHeader();
  }

  if(!tofHeader) return kFALSE;
  /// Fill vpd information
  for(int i=0;i<NVPD;i++) {
    tofHeader->setVpdTime(west, i+1, mVPDLeTime[i]);
    tofHeader->setVpdTime(east, i+1, mVPDLeTime[i+NVPD]);
  }
  tofHeader->setVpdHitPattern(east, mVPDHitPatternEast);
  tofHeader->setVpdHitPattern(west, mVPDHitPatternWest);
  for(int i=0;i<mNVzVpd;i++) {
    tofHeader->setVpdVz(mVPDVtxZ[i],i);
  }

    LOG_INFO << " TofCollection: NWest = " << tofHeader->numberOfVpdHits(west) 
	     << " NEast = " << tofHeader->numberOfVpdHits(east) << endm;
    LOG_INFO <<" vpd vz = " << mVPDVtxZ[0] <<endm;
// added
    LOG_INFO << " TSum West " << mTSumWest << " East " << mTSumEast << endm;

  return kTRUE;
}


//____________________________________________________________________________
Bool_t StVpdCalibMaker::loadVpdData()
{
  if(mMuDstIn) {
    StMuDstMaker *mMuDstMaker = (StMuDstMaker *)GetMaker("MuDst");
    if(!mMuDstMaker) {
      LOG_WARN << " No MuDstMaker ... bye-bye" << endm;
      return kFALSE;
    }
    mMuDst = mMuDstMaker->muDst();
    if(!mMuDst) {
      LOG_WARN << " No MuDst ... bye-bye" << endm;
      return kFALSE;
    }

    Int_t nhits = mMuDst->numberOfBTofHit();
//    Int_t nhits = mMuDst->btofArray(muBTofHit)->GetEntries();
    for(int i=0;i<nhits;i++) {
      StMuBTofHit *aHit = (StMuBTofHit *)mMuDst->btofHit(i);
      if(!aHit) continue;
      int trayId = aHit->tray();
       if(trayId==WestVpdTrayId || trayId==EastVpdTrayId) {   // VPD this time
        int tubeId = aHit->cell();
	int indx = (trayId-NTray-1)*NVPD+(tubeId-1);
	if (indx>=2*NVPD){
	  LOG_ERROR << "vpd index (" << indx << ") out of range ("
		    << 2*NVPD << ") for trayId-tubeId " << trayId 
		    << "-" << tubeId << endm;
	  return kStErr;
	}
        mVPDLeTime[indx] = aHit->leadingEdgeTime();
        mVPDTot[indx] = aHit->tot();
      }
    }

  } else {
    StEvent *thisEvent = (StEvent *) GetInputDS("StEvent");
  
    // event selection  // no primary vertex required
    if( !thisEvent ||
        !thisEvent->btofCollection() ||
        !thisEvent->btofCollection()->hitsPresent() ) {
      LOG_WARN << " Nothing to do ... bye-bye" << endm;
      return kFALSE;
    }

    mBTofColl = thisEvent->btofCollection();
    StSPtrVecBTofHit &tofHits = mBTofColl->tofHits();
    Int_t nhits = tofHits.size();
    LOG_INFO << " Fired TOF cells + upVPD tubes : " << nhits << endm;
  
    for(int i=0;i<nhits;i++) {
      StBTofHit *aHit = dynamic_cast<StBTofHit*>(tofHits[i]);
      if(!aHit) continue;
      int trayId = aHit->tray();
      if(trayId==WestVpdTrayId || trayId==EastVpdTrayId) {   // VPD this time
        int tubeId = aHit->cell();
	int indx = (trayId-NTray-1)*NVPD+(tubeId-1);
	if (indx>=2*NVPD){
	  LOG_ERROR << "vpd index (" << indx << ") out of range ("
		    << 2*NVPD << ") for trayId-tubeId " << trayId 
		    << "-" << tubeId << endm;
	  return kStErr;
	}
        mVPDLeTime[indx] = aHit->leadingEdgeTime();
        mVPDTot[indx] = aHit->tot();
      }
    } // end loop hits
  }

  return kTRUE;
}


//_____________________________________________________________________________
void StVpdCalibMaker::tsum(const Double_t *tot, const Double_t *time)
{
  /// Sum vpd informations
  mTSumEast = 0.;
  mTSumWest = 0.;
  mNEast = 0;
  mNWest = 0;
  mVPDHitPatternWest = 0;
  mVPDHitPatternEast = 0;

  bool vpdEast=false;
  for(int i=0;i<2*NVPD;i++) {
    if (i>=NVPD) vpdEast=true;

    if( time[i]>0. && tot[i]>0. ) {

      int ibin = -1;
      for(int j=0;j<NBinMax-1;j++) {
	if(tot[i]>=mVPDTotEdge[i][j] && tot[i]<mVPDTotEdge[i][j+1]) {
	  ibin = j;
	  break;
	}
      }
      if(ibin>=0&&ibin<NBinMax) {
	Double_t x1 = mVPDTotEdge[i][ibin];
	Double_t x2 = mVPDTotEdge[i][ibin+1];
	Double_t y1 = mVPDTotCorr[i][ibin];
	Double_t y2 = mVPDTotCorr[i][ibin+1];
	Double_t dcorr = y1 + (tot[i]-x1)*(y2-y1)/(x2-x1);

	if (vpdEast){
	  mNEast++;
	  mVPDLeTime[i] = time[i] - dcorr;
	  mTSumEast += mVPDLeTime[i];
	  mVPDHitPatternEast |= 1<<(i-NVPD);
	} else {
	  mNWest++;
	  mVPDLeTime[i] = time[i] - dcorr;
	  mTSumWest += mVPDLeTime[i];
	  mVPDHitPatternWest |= 1<<i;
	}

      } else {
	if (vpdEast){
	  LOG_WARN << " Vpd East tube " << i+1-NVPD << " TOT ("<< ibin
		   << ") out of range (0-"<<NBinMax<<") !" << endm;
	} else{
	  LOG_WARN << " Vpd West tube " << i+1 << " TOT ("<< ibin
		   << ") out of range (0-"<<NBinMax<<") !" << endm;
	}
        mVPDLeTime[i] = 0.; // out of range, remove this hit
      }
    }
  }

}


//_____________________________________________________________________________
/// VzFinder from VPD, currently using a simple calculation. Can be improved in the future
void StVpdCalibMaker::vzVpdFinder()
{
  for(int i=0;i<2*NVPD;i++){
    // check
    if(mVPDLeTime[i]<1.e-4) continue;
    double vpdtime;
    if(i<NVPD&&mNWest>1) {  // west VPD
      vpdtime = (mVPDLeTime[i]*mNWest-mTSumWest)/(mNWest-1);
      if(fabs(vpdtime)>TDIFFCUT) {
        mTSumWest -= mVPDLeTime[i];
        mVPDLeTime[i] = 0.;
        mNWest--;
      }
    }
    if(i>=NVPD&&mNEast>1) {  // east VPD
      vpdtime = (mVPDLeTime[i]*mNEast-mTSumEast)/(mNEast-1);
      if(fabs(vpdtime)>TDIFFCUT) {
        mTSumEast -= mVPDLeTime[i];
        mVPDLeTime[i] = 0.;
        mNEast--; 
      }
    }
  }

  // calculate the vertex z from vpd
  if ( mNEast>=mVPDEastHitsCut && mNWest>=mVPDWestHitsCut ) {
    mVPDVtxZ[0] = (mTSumEast/mNEast - mTSumWest/mNWest)/2.*(C_C_LIGHT/1.e9);
    mNVzVpd++;
  }
}


//_____________________________________________________________________________
void StVpdCalibMaker::bookHistograms()
{
  mhEventCounter = new TH1D("eventCounter","eventCounter",20,0,20);
  mhNVpdHits = new TH2D("vpdHits"," west vs east ",20,0.,20.,20,0.,20.);
  for(int i=0;i<2*NVPD;i++) {
    //char buf[100];
    TString buf;
    if(i<NVPD) {
      //sprintf(buf,"res_W_%d",i+1);
      buf.Form("res_W_%d",i+1);
    } else {
      //sprintf(buf,"res_E_%d",i-mNVPD+1);
      buf.Form("res_E_%d",i-NVPD+1);
    }
    mhVpd[i] = new TH1D(buf, buf, 3000, -3., 3.);
  }
  mhVpdAll = new TH1D("res_All","res_All", 3000, -3., 3.);
}

//_____________________________________________________________________________
void StVpdCalibMaker::fillHistograms()
{
  if(!mHisto) return;
  if (mhNVpdHits) mhNVpdHits->Fill(mNEast, mNWest);
  if(mNWest>=2) {
    for(int i=0;i<NVPD;i++) {
      if(mVPDLeTime[i]>0.) {
        Double_t tdiff = (mNWest*mVPDLeTime[i] - mTSumWest)/(mNWest - 1);
        if (mhVpd[i]) mhVpd[i]->Fill(tdiff);
        if (mhVpdAll) mhVpdAll->Fill(tdiff);
      }
    }
  }
  if(mNEast>=2) {
    for(int j=0;j<NVPD;j++) {
      int i = j + NVPD;
      if(mVPDLeTime[i]>0.) {
        Double_t tdiff = (mNWest*mVPDLeTime[i] - mTSumEast)/(mNEast - 1);
        if (mhVpd[i]) mhVpd[i]->Fill(tdiff);
        if (mhVpdAll) mhVpdAll->Fill(tdiff);
      }
    }
  }
}


//_____________________________________________________________________________
void StVpdCalibMaker::writeHistograms() const
{
  // Output file
  TFile *theHistoFile =  new TFile(mHistoFileName.c_str(), "RECREATE");
  LOG_INFO << "StVpdCalibMaker::writeHistograms()"
       << " histogram file " <<  mHistoFileName << endm;

  if (theHistoFile&&theHistoFile->IsOpen()){
    theHistoFile->cd();

    if(mHisto) {
      mhEventCounter->Write();
      mhNVpdHits->Write();
      mhVpdAll->Write();
      for(int i=0;i<2*NVPD;i++) {
	mhVpd[i]->Write();
      }
    }

    theHistoFile->Close();
    delete theHistoFile;
  }
  else 
    LOG_ERROR << "unable to open histogram file" << endm;
    
  return;
}
