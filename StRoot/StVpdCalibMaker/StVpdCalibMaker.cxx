/*******************************************************************
 *
 * $Id: StVpdCalibMaker.cxx,v 1.14 2018/04/30 23:18:00 smirnovd Exp $
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
 * Revision 1.14  2018/04/30 23:18:00  smirnovd
 * Reduce excessive output
 *
 * Revision 1.13  2017/03/02 18:26:50  jeromel
 * Updates to StVpdCalibMaker after review - changes by jdb, nl
 *
 *
 * Revision 1.13  2016/11/14 luttrell
 * Simulated vpd hits no longer undergo electronics corrections
 *
 * Revision 1.12  2012/12/14 06:36:05  geurts
 * Changed global database calls to direct table access and/or removed deprecated database access code.
 *
 * Revision 1.11  2011/02/23 20:00:52  geurts
 * Change MaxBin for ToT arrays from 60 to 128 (in agreement with the IDL definition of vpdTotCorr)
 * Move the log message that informs the user about the start-timing mode outside the tube loop ... no need to see the same message 38 times.
 *
 * Revision 1.10  2011/01/07 21:28:51  geurts
 * Allow user to "force" VPD-start or startless mode, regardless of dbase setting
 *
 * Revision 1.9  2010/12/18 01:10:27  geurts
 * bugfix: apply VPD outlier correction in StEvent mode, too.
 *
 * Revision 1.8  2010/12/10 21:38:06  geurts
 * RT#1996: default initialization of database table in case dataset is not found
 *
 * Revision 1.7  2010/10/31 05:43:57  geurts
 * apply outlier truncation to all energies, keep at 20% (previously only enabled for low beam energies)
 *
 * Revision 1.6  2010/05/25 22:09:48  geurts
 * improved database handling and reduced log output
 *
 * Revision 1.5  2010/05/12 22:46:51  geurts
 * Startless BTOF self-calibration method (Xin)
 *
 * Revision 1.4  2010/05/06 22:37:41  geurts
 * Remove slower hits (outliers) in VPD timing calculations (Xin Dong)
 *
 * Revision 1.3  2010/01/29 19:51:08  geurts
 * Fixed a bug in vzVpdFinder outlier bookkeeping (Xin Dong)
 *
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
/// FracTruncated: LowEnergy runs, truncate VPD hit timing for mean calculation
  const Double_t StVpdCalibMaker::FracTruncated=0.2;

//_____________________________________________________________________________
StVpdCalibMaker::StVpdCalibMaker(const Char_t *name) : StMaker(name)
{
  /// default constructor
  /// set the default parameters.
  /// Reset the calibration parameters
  setVPDHitsCut(1,1);

  //mEvent   = 0;
  mTruncation = kFALSE;  //! Default setting: no truncation in mean calculation for large beam energy collisions
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
  // use vpd as start by default;
  mUseVpdStart = kTRUE;
  mForceTofStart = kFALSE; // flag indicates user-override for TOF Start time calculation
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
  memset(mFlag, 0, sizeof(mFlag));
  mVPD_qaTruth = vector<Int_t>(2*NVPD,0);

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
//    if(m_Mode) {
//    //    setHistoFileName("vpdana.root");
//    } else {
//        setHistoFileName("");
//    }

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
    LOG_INFO << "Initializing VPD calibration parameters from file"
    		 << "(" << mCalibFilePvpd << ")" << endm;  
    ifstream inData;
    inData.open(mCalibFilePvpd.c_str());
    int nchl, nbin;
    for(int i=0;i<NVPD*2;i++) {
      inData>>nchl;
      inData>>nbin;
      if (nbin>NBinMax) {
		LOG_ERROR << "number of bins (" << nbin << ") out of range ("
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
    LOG_INFO << "Initializing VPD calibration parameters from database" << endm;

    // read vpdTotCorr table
    TDataSet *dbDataSet = GetDataBase("Calibrations/tof/vpdTotCorr");
    if (dbDataSet){
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
	if (!mForceTofStart){
	  if(i==0) {  // identify once only, for the first tube
	    short flag = totCorr[i].corralgo;
	    if(flag==0) {
	      mUseVpdStart=kTRUE;
	      LOG_INFO << "Selected VPD for TOF start-timing (corralgo=1)" << endm;
	    }
	    else if(flag==1) {
	      mUseVpdStart=kFALSE;
	      LOG_INFO << "VPD NOT used for TOF start-timing (corralgo=0)" << endm;
	    }
	    else {
	      LOG_WARN << "Unknown calibration option " << flag << endm;
	    }
	  }
	  else { // verify that all other entries agree
	    if (totCorr[0].corralgo==0 && (totCorr[i].corralgo!=0))
	      {LOG_WARN << "corralgo dbase inconsistency: " << totCorr[i].corralgo << endm;}
	    if (totCorr[0].corralgo==1 && (totCorr[i].corralgo!=1))
	      {LOG_WARN << "corralgo dbase inconsistency: " << totCorr[i].corralgo << endm;}
	  }
	}

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

      // Let the user know what is used for calculating TOF Start (and how we got there)
      if (mForceTofStart) { LOG_INFO << "Detected user override in Start Timing selection ::setUseVpdStart()." << endm;}
      if (mUseVpdStart) {
	LOG_INFO << "Selected VPD for TOF start-timing" << endm;
      }
      else {
	LOG_INFO << "VPD NOT used for TOF start-timing" << endm;  
      }
    }
    else {
    // Note: this construction addresses RT#1996 and allows backward compatibility with older database
    //       timestamps at which these database structures did not exist. All values will be zero (hence the ERROR)
    //       and the VPD will be disabled for TOF start timing, i.e. the TOF could still operate in start-less mode 
      LOG_ERROR << "unable to get vpdTotCorr dataset ... reset all to zero values (NOT GOOD!) and disable use for TOF-start" << endm; 
      resetPars();
      mUseVpdStart=kFALSE;
      LOG_INFO << "VPD NOT used for TOF start-timing" << endm;  
      return kStErr;
    }
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
  LOG_DEBUG << " StVpdCalibMaker::Make: starting ..." << endm;
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

  tsum(mVPDTot, mVPDLeTime, mVPD_qaTruth);
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

    LOG_INFO << "BTofHeader: NWest = " << tofHeader->numberOfVpdHits(west) 
	     << " NEast = " << tofHeader->numberOfVpdHits(east) << endm;
    if(tofHeader->numberOfVpdHits(west)!=mNWest ||
       tofHeader->numberOfVpdHits(east)!=mNEast) {
      LOG_WARN << "BTofHeader inconsistency: Local nWest = " << mNWest << " nEast = " << mNEast << endm;
    }
    LOG_INFO <<"summary:  VPD-VtxZ = " << mVPDVtxZ[0] 
             << "; TSum West = " << mTSumWest << "  East = " << mTSumEast << endm;

  return kTRUE;
}


//____________________________________________________________________________
Bool_t StVpdCalibMaker::loadVpdData()
{
  if(mMuDstIn) {
    StMuDstMaker *mMuDstMaker = (StMuDstMaker *)GetMaker("MuDst");
    if(!mMuDstMaker) {
      LOG_WARN << "No MuDstMaker ... bye-bye" << endm;
      return kFALSE;
    }
    mMuDst = mMuDstMaker->muDst();
    if(!mMuDst) {
      LOG_WARN << "No MuDst ... bye-bye" << endm;
      return kFALSE;
    }

    // Apply truncation to all energies and collision systems
//    if(mMuDst->event() && mMuDst->event()->runInfo().centerOfMassEnergy()<40.) {
      mTruncation = kTRUE;
//    } else {
//      mTruncation = kFALSE;
//    }


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
            mVPD_qaTruth[indx] = aHit->qaTruth();
          }
    }

  }
  
  else {
    StEvent *thisEvent = (StEvent *) GetInputDS("StEvent");
  
    // event selection  // no primary vertex required
    if( !thisEvent )  {
      LOG_WARN << "No StEvent present" << endl;
      return kFALSE;
     }
    if (!thisEvent->btofCollection() ) {
       LOG_WARN << "No BTOFCollection present" << endm;
       return kFALSE;
    }
    if (!thisEvent->btofCollection()->hitsPresent() ) {
      LOG_WARN << "No hits present" << endm;
      return kFALSE;
    }

//    if(thisEvent->runInfo() && thisEvent->runInfo()->centerOfMassEnergy()<40.) { // 40 GeV or less
      mTruncation = kTRUE;
//    } else {
//      mTruncation = kFALSE;
//    }

    mBTofColl = thisEvent->btofCollection();
    StSPtrVecBTofHit &tofHits = mBTofColl->tofHits();
    Int_t nhits = tofHits.size();
    LOG_INFO << "Total number of TOF cells + VPD tubes : " << nhits << endm;
  
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
        mVPD_qaTruth[indx] = aHit->qaTruth();
      }
    } // end loop hits
  }

  return kTRUE;
}


//_____________________________________________________________________________
void StVpdCalibMaker::tsum(const Double_t *tot, const Double_t *time, std::vector<Int_t> qaTruth)
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
      
        //**************BEGIN SIMULATION CODE BLOCK***************
        if ( qaTruth[i] > 0 && time[i] != 0.) {
            LOG_DEBUG << "Simulation" << endm;
              
              if (vpdEast){
                  mNEast++;
                  mVPDLeTime[i] = time[i];
                  mTSumEast += mVPDLeTime[i];
                  mVPDHitPatternEast |= 1<<(i-NVPD);
                  mFlag[i] = 1;
              }
              else {
                  mNWest++;
                  mVPDLeTime[i] = time[i];
                  mTSumWest += mVPDLeTime[i];
                  mVPDHitPatternWest |= 1<<i;
                  mFlag[i] = 1;
              }
        }
        //**************END SIMULATION CODE BLOCK*****************
      
        else if( time[i]>0. && tot[i]>0. ) {
          
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
                          mFlag[i] = 1;
                    }
                    else {
                      mNWest++;
                      mVPDLeTime[i] = time[i] - dcorr;
                      mTSumWest += mVPDLeTime[i];
                      mVPDHitPatternWest |= 1<<i;
                          mFlag[i] = 1;
                    }
              }
              else {
                if (vpdEast){
                  LOG_WARN << " Vpd East tube " << i+1-NVPD << " TOT ("<< ibin
                       << ") out of range (0-"<<NBinMax<<") !" << endm;
                }
                else{
                  LOG_WARN << " Vpd West tube " << i+1 << " TOT ("<< ibin
                       << ") out of range (0-"<<NBinMax<<") !" << endm;
                }
                    mVPDLeTime[i] = 0.; // out of range, remove this hit
              }
        }
      
      else LOG_DEBUG << "No hit." << endm;
  }

}

//_____________________________________________________________________________
/// VzFinder from VPD, currently using a simple calculation. Can be improved in the future
void StVpdCalibMaker::vzVpdFinder()
{
  for(int i=0;i<2*NVPD;i++){
    // check
      if (mVPD_qaTruth[i] > 0 && mVPDLeTime[i]!=0 && mFlag[i]) { // Check for simulation
          mTruncation = kFALSE;
      }
    else if(mVPDLeTime[i]<1.e-4 || !mFlag[i]) continue;
      
    double vpdtime;
    if(i<NVPD&&mNWest>1) {  // west VPD
      vpdtime = (mVPDLeTime[i]*mNWest-mTSumWest)/(mNWest-1);    // Cuts on times with a significant deviation from the average.
      if(fabs(vpdtime)>TDIFFCUT) {
        mTSumWest -= mVPDLeTime[i];
        mVPDLeTime[i] = 0.;
        mNWest--;
        mVPDHitPatternWest &= ( 0x7ffff - (1<<i) );
        mFlag[i] = 0;
      }
    }
    if(i>=NVPD&&mNEast>1) {  // east VPD
      vpdtime = (mVPDLeTime[i]*mNEast-mTSumEast)/(mNEast-1);    // Cuts on times with a significant deviation from the average.
      if(fabs(vpdtime)>TDIFFCUT) {
        mTSumEast -= mVPDLeTime[i];
        mVPDLeTime[i] = 0.;
        mNEast--; 
        mVPDHitPatternEast &= ( 0x7ffff - (1<<(i-NVPD)) );
        mFlag[i] = 0;
      }
    }
  }

  // remove slower hit in low energy runs.
  if(mTruncation ) {
    LOG_DEBUG << "Uh-oh, stepped into the truncation block!" << endm;
    Int_t hitIndex[2*NVPD];
    Int_t nTube = NVPD;
    TMath::Sort(nTube, &mVPDLeTime[0], &hitIndex[0]);
    int nRejectedWest = (int)(FracTruncated*mNWest+0.5);
    LOG_DEBUG << " NWest before = " << mNWest << " rejected = " << nRejectedWest << endm;
    for(int i=0;i<nRejectedWest;i++) {
      int index = hitIndex[i];
      mTSumWest -= mVPDLeTime[index];
      mVPDLeTime[index] = 0.;
      mNWest--;
      mVPDHitPatternWest &= ( 0x7ffff - (1<<index) );
      mFlag[index] = 0;
    }

    TMath::Sort(nTube, &mVPDLeTime[NVPD], &hitIndex[NVPD]);
    int nRejectedEast = (int)(FracTruncated*mNEast+0.5);
    LOG_DEBUG << " NEast before = " << mNEast << " rejected = " << nRejectedEast << endm;
    for(int i=0;i<nRejectedEast;i++) {
      int index = hitIndex[i+NVPD] + NVPD;
      mTSumEast -= mVPDLeTime[index];
      mVPDLeTime[index] = 0.;
      mNEast--;
      mVPDHitPatternEast &= ( 0x7ffff - (1<<(index-NVPD)) );
      mFlag[index] = 0;
    }
  }

  // calculate the vertex z from vpd
  if ( mNEast>=mVPDEastHitsCut && mNWest>=mVPDWestHitsCut ) {
    mVPDVtxZ[0] = (mTSumEast/mNEast - mTSumWest/mNWest)/2.*(C_C_LIGHT/1.e9);
    LOG_DEBUG << "Vertex is at: " << mVPDVtxZ[0] << endm;
    mNVzVpd++;
  }
}


//_____________________________________________________________________________
void StVpdCalibMaker::bookHistograms()
{
  mhEventCounter = new TH1D("eventCounter","eventCounter",20,0,20);
  mhNVpdHits = new TH2D("vpdHits"," west vs east ",20,0.,20.,20,0.,20.);
  mmVpdVertexHist = new TH1D("mVpdVertexHist","Calculated Vpd Vertices; Position (cm); Counts", 300, -50, 50);
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
    if (mmVpdVertexHist) mmVpdVertexHist->Fill(mVPDVtxZ[0]);
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
        Double_t tdiff = (mNEast*mVPDLeTime[i] - mTSumEast)/(mNEast - 1);
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
        mmVpdVertexHist->Write();
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
