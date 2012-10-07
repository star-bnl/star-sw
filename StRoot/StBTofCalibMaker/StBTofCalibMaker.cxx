/*******************************************************************
 *
 * $Id: StBTofCalibMaker.cxx,v 1.3 2009/12/04 22:26:34 geurts Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: - Tof Calibration Maker to do the calibration for pVPD 
 *              (start timing) , TOF
 *              - store into StBTofPidTraits
 *
 *****************************************************************
 *
 * $Log: StBTofCalibMaker.cxx,v $
 * Revision 1.3  2009/12/04 22:26:34  geurts
 * Split original CalibMaker into dedicated StVpdCalibMaker and BTOF-specific StBTofCalibMaker (Xin):
 * - function added to directly access the MuDst
 * - clean up those VPD members and functions as they are moved to the StVpdCalibMaker
 * - add VPD related functions to load/write the calibration VPD information in the BTofHeader
 * - few small algorithm updates to be consistent with what is used in calibration procedures
 * - several minor code cleanups
 *
 * Revision 1.2  2009/11/21 00:29:52  geurts
 * Dtabase readout made more robust, static const moved to cxx.
 *
 * Revision 1.1  2009/09/23 02:28:41  geurts
 * first version: Combined BTOF & VPD CalibMaker
 *
 *
 *******************************************************************/
#include <iostream>
#include "StEvent.h"
#include "StBTofCollection.h"
#include "StBTofHit.h"
#include "StBTofHeader.h"
#include "StBTofPidTraits.h"
#include "StEventTypes.h"
#include "Stypes.h"
#include "StThreeVectorD.hh"
#include "StHelix.hh"
#include "StTrackGeometry.h"
#include "StTrackPidTraits.h"
#include "StEventUtilities/StuRefMult.hh"
#include "PhysicalConstants.h"
#include "StPhysicalHelixD.hh"
#include "tables/St_tofTOffset_Table.h"
#include "tables/St_tofTotbCorr_Table.h"
#include "tables/St_tofZbCorr_Table.h"

#include "tables/St_vertexSeed_Table.h"

#include "StBTofUtil/tofPathLength.hh"
#include "StBTofUtil/StBTofHitCollection.h"
#include "StBTofUtil/StBTofGeometry.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuBTofHit.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuBTofPidTraits.h"

#include "StBTofCalibMaker.h"

#ifdef __ROOT__
ClassImp(StBTofCalibMaker)
#endif

 /// Very High resolution mode, pico-second per bin
const Double_t StBTofCalibMaker::VHRBIN2PS =  24.4140625; // 1000*25/1024 (ps/chn)
/// High resolution mode, pico-second per bin                                                       
const Double_t StBTofCalibMaker::HRBIN2PS = 97.65625; // 97.65625= 1000*100/1024  (ps/chn)  
/// tdc limit                                          
const Double_t StBTofCalibMaker::TMAX = 51200.;           
///   VzVpd - VzProj cut
const Double_t StBTofCalibMaker::VZDIFFCUT=6.;          
///   DCAR cut
const Double_t StBTofCalibMaker::DCARCUT=1.;
const Double_t StBTofCalibMaker::mC_Light = C_C_LIGHT/1.e9;


//_____________________________________________________________________________
StBTofCalibMaker::StBTofCalibMaker(const char *name) : StMaker(name)
{
  /// default constructor
  /// set the default parameters for TDC, ADC cut etc.
  /// Reset the calibration parameters
  setVPDHitsCut(1,1);
  setOuterGeometry(true);

  mEvent = 0;
  mBTofHeader = 0;
  mMuDst = 0;

  mSlewingCorr = kTRUE;
  mUseEventVertex = kFALSE;
  mMuDstIn = kFALSE;

  setCreateHistoFlag(kFALSE);
  setHistoFileName("btofcalib.root");

  // default initialization from database
  mInitFromFile = kFALSE;

  // assign default locations and names to the calibration files 
  setCalibFileTot("/star/institutions/rice/calib/default/totCali_4DB.dat");
  setCalibFileZhit("/star/institutions/rice/calib/default/zCali_4DB.dat");
  setCalibFileT0("/star/institutions/rice/calib/default/t0_4DB.dat");  


  StThreeVectorD MomFstPt(0.,0.,9999.);
  StThreeVectorD origin(0.,0.,0.);
  mBeamHelix = new StPhysicalHelixD(MomFstPt,origin,0.5*tesla,1.);
}

//_____________________________________________________________________________
StBTofCalibMaker::~StBTofCalibMaker()
{  /* noop */ }

//_____________________________________________________________________________
void StBTofCalibMaker::resetPars()
{
  memset(mTofTotEdge, 0, sizeof(mTofTotEdge));
  memset(mTofTotCorr, 0, sizeof(mTofTotCorr));
  memset(mTofZEdge,   0, sizeof(mTofZEdge)  );
  memset(mTofZCorr,   0, sizeof(mTofZCorr)  );
  memset(mTofTZero,   0, sizeof(mTofTZero)  );
}

//_____________________________________________________________________________
void StBTofCalibMaker::resetVpd()
{
  memset(mVPDLeTime, 0, sizeof(mVPDLeTime));
  mTStart = -9999.;
  mTDiff  = -9999.;
  mVPDVtxZ = -9999.;
  mVPDHitPatternEast = 0;
  mVPDHitPatternWest = 0;
  mNEast = 0;
  mNWest = 0;
  mValidStartTime = kFALSE;
}

//____________________________________________________________________________
Int_t StBTofCalibMaker::Init()
{
  resetPars();
  resetVpd();

  // m_Mode can be set by SetMode() method
  if(m_Mode) {
//    setHistoFileName("btofcalib.root");
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

  return kStOK;
}

//____________________________________________________________________________
Int_t StBTofCalibMaker::InitRun(int runnumber)
{
  // tof run configurations

  Int_t val = initParameters(runnumber);
  if(val==kStOK) {
    mValidCalibPar = kTRUE;
  } else {
    mValidCalibPar = kFALSE;
  }

  if(mValidCalibPar) {
    LOG_INFO << " ==> Good! Valid cali parameters! " << endm;
  } else {
    LOG_WARN << " ==> No valid cali parameters! " << endm;
  }

  return kStOK;

}

//_____________________________________________________________________________
Int_t StBTofCalibMaker::initParameters(int runnumber)
{
  /// initialize the calibrations parameters from dbase
  /// read in and check the size

  if (mInitFromFile){
    LOG_INFO << "Retrieving calibration parameters from files" << endm;
    ifstream inData;

    /// open file and read Time-over-Threshold calibration parameters
    LOG_INFO << " - ToT : " << mCalibFileTot << endm;  
    inData.open(mCalibFileTot.c_str());
    int trayId, boardId;
    int nbin;
    for(int i=0;i<mNTray;i++) {
      for(int j=0;j<mNTDIG;j++) {
	inData>>trayId>>boardId;
	inData>>nbin;
	for(int k=0;k<=nbin;k++) inData>>mTofTotEdge[trayId-1][boardId-1][k];
	for(int k=0;k<=nbin;k++) {
	  inData>>mTofTotCorr[trayId-1][boardId-1][k];
	  if(k%10==0&&Debug()) {
	    LOG_INFO << " ijk= " << i << " " << j << " " << k << " tot " << mTofTotEdge[trayId-1][boardId-1][k] << " corr " << mTofTotCorr[trayId-1][boardId-1][k] << endm; 
	  }
	}
      }
    }
    inData.close();

    /// open file and read local Zhit calibration parameters
    LOG_INFO << " - Zhit : " << mCalibFileZhit << endm;  
    //inData.open("/star/institutions/lbl/dongx/tof/NewEvent/calibmaker/test_calib/CalibPar/zCali_4DB.dat");
    inData.open(mCalibFileZhit.c_str());
    for(int i=0;i<mNTray;i++) {
      for(int j=0;j<mNTDIG;j++) {
	inData>>trayId>>boardId;
	inData>>nbin;
	for(int k=0;k<=nbin;k++) inData>>mTofZEdge[trayId-1][boardId-1][k];
	for(int k=0;k<=nbin;k++) {
	  inData>>mTofZCorr[trayId-1][boardId-1][k];
	  if(k%10==0&&Debug()) {
	    LOG_DEBUG << " ijk= " << i << " " << j << " " << k << " z " << mTofZEdge[trayId-1][boardId-1][k] << " corr " << mTofZCorr[trayId-1][boardId-1][k] << endm; 
	  }
	}
      }
    }
    inData.close();

    /// open file and read T0 calibration parameters
    LOG_INFO << " - T0 : " << mCalibFileT0 << endm;  
    //inData.open("/star/institutions/lbl/dongx/tof/NewEvent/calibmaker/test_calib/CalibPar/t0_4DB.dat");
    inData.open(mCalibFileT0.c_str());
    int moduleId, cellId;
    for(int i=0;i<mNTray;i++) {
      for(int j=0;j<mNModule;j++) {
	for(int k=0;k<mNCell;k++) {
	  inData>>trayId>>moduleId>>cellId;
	  inData>>mTofTZero[trayId-1][moduleId-1][cellId-1];
	  int index = i*mNModule*mNCell+j*mNCell+k;
	  if(index%100==0&&Debug()) {
	    LOG_DEBUG << " ijk= " << i << " " << j << " " << k << " t0 " << mTofTZero[trayId-1][moduleId-1][cellId-1] << endm; 
	  }
	}
      }
    }
    inData.close();
  }
  else {

    /// Get all calibration parameters from the database
    LOG_INFO << " Retrieving calibration parameters from Calibrations_tof" << endm;

    /// read in and check the size
    TDataSet *mDbDataSet = GetDataBase("Calibrations/tof");
    if (!mDbDataSet){
      LOG_ERROR  << "unable to get TOF run parameters" << endm;
      return kStErr;
    }
  
    LOG_INFO << "     loading calibration parameters ..." << endm;
    // read tofTotbCorr table
    St_tofTotbCorr* tofTotCorr = static_cast<St_tofTotbCorr*>(mDbDataSet->Find("tofTotbCorr"));
    if(!tofTotCorr) {
      LOG_ERROR << "unable to get tof TotbCorr table parameters" << endm;
      //    assert(tofTotCorr);
      return kStErr;
    }
    tofTotbCorr_st* totCorr = static_cast<tofTotbCorr_st*>(tofTotCorr->GetArray());
    Int_t numRows = tofTotCorr->GetNRows();

    if(numRows!=mNTray*mNTDIG) {
      LOG_WARN  << " Mis-matched number of rows in tofTotbCorr table! "  << numRows 
		<< " (exp:" << mNTray*mNTDIG+mNVPD*2 << ")" << endm;
    }

    LOG_DEBUG << " Number of rows read in: " << numRows << " for ToT correction" << endm;

    for (Int_t i=0;i<numRows;i++) {
      short trayId = totCorr[i].trayId;
      short moduleId = totCorr[i].moduleId;
      short boardId = (moduleId-1)/4+1;      // used for trays

      LOG_DEBUG << " tray " << trayId << " board " << boardId << endm;
      for(Int_t j=0;j<mNBinMax;j++) {
        if(trayId>0&&trayId<=mNTray&&boardId>0&&boardId<=mNTDIG){ // trays
	  mTofTotEdge[trayId-1][boardId-1][j] = totCorr[i].tot[j];
	  mTofTotCorr[trayId-1][boardId-1][j] = totCorr[i].corr[j];
	  if(Debug()&&j%10==0) { LOG_DEBUG << " j=" << j << " tot " << mTofTotEdge[trayId-1][boardId-1][j] << " corr " << mTofTotCorr[trayId-1][boardId-1][j] << endm; }
	}
      } // end j 0->mNBinMax
    } // end i 0->numRows

    // read tofZbCorr table
    St_tofZbCorr* tofZCorr = static_cast<St_tofZbCorr*>(mDbDataSet->Find("tofZbCorr"));
    if(!tofZCorr) {
      LOG_ERROR << "unable to get tof ZbCorr table parameters" << endm;
      //    assert(tofZCorr);
      return kStErr;
    }
    tofZbCorr_st* zCorr = static_cast<tofZbCorr_st*>(tofZCorr->GetArray());
    numRows = tofZCorr->GetNRows();
    
    if(numRows!=mNTray*mNTDIG) {
      LOG_WARN << " Mis-matched number of rows in tofZbCorr table! "  << numRows 
		<< " (exp:" << mNTray*mNTDIG+mNVPD*2 << ")" << endm;
    }
    LOG_DEBUG << " Number of rows read in: " << numRows << " for Z correction" << endm;


    for (Int_t i=0;i<numRows;i++) {
      short trayId = totCorr[i].trayId;
      short moduleId = totCorr[i].moduleId;
      short boardId = (moduleId-1)/4+1;      // used for trays

      LOG_DEBUG << " tray " << trayId << " board " << boardId << endm;
      for(Int_t j=0;j<mNBinMax;j++) {
	if(trayId>0&&trayId<=mNTray&&boardId>0&&boardId<=mNTDIG) {  // trays
	  mTofZEdge[trayId-1][boardId-1][j] = zCorr[i].z[j];
	  mTofZCorr[trayId-1][boardId-1][j] = zCorr[i].corr[j];
	  if(Debug()&&j%10==0) { LOG_DEBUG << " j=" << j << " tot " << mTofZEdge[trayId-1][boardId-1][j] << " corr " << mTofZCorr[trayId-1][boardId-1][j] << endm; }
	}
      } // end j 0->mNBinMax
    } // end i 0->numRows

    // read tofTOffset table
    St_tofTOffset* tofTOffset = static_cast<St_tofTOffset*>(mDbDataSet->Find("tofTOffset"));
    if(!tofTOffset) {
      LOG_ERROR << "unable to get tof TOffset table parameters" << endm;
      //    assert(tofTOffset);
      return kStErr;
    }
    tofTOffset_st* tZero = static_cast<tofTOffset_st*>(tofTOffset->GetArray());
    numRows = tofTOffset->GetNRows();

    LOG_DEBUG << " Number of rows read in: " << numRows << " for TOffset correction" << endm;

    if(numRows!=mNTray) {
      LOG_WARN << " Mis-matched number of rows in tofTOffset table! " << numRows 
		<< " (exp:" << mNTray << ")" << endm;
      //      return kStErr;
    }
    for (Int_t i=0;i<numRows;i++) {
      short trayId = tZero[i].trayId;
      LOG_DEBUG << " tray " << trayId << endm;
      
      if(trayId>0&&trayId<=mNTray) {
	for(int j=0;j<mNTOF;j++) {
	  mTofTZero[trayId-1][j/6][j%6] = tZero[i].T0[j];
	  if(Debug()&&j%10==0) { LOG_DEBUG << " j=" << j << " T0 " << mTofTZero[trayId-1][j/6][j%6] << endm; }
	}
      }
    }
  }


  // ========== Set Beam Line =====================
    double x0 = 0.;
    double y0 = 0.;
    double dxdz = 0.;
    double dydz = 0.;

    // Get Current Beam Line Constraint from database
    TDataSet* dbDataSet = this->GetDataBase("Calibrations/rhic");

    if (dbDataSet) {
      vertexSeed_st* vSeed = ((St_vertexSeed*) (dbDataSet->FindObject("vertexSeed")))->GetTable();

      x0 = vSeed->x0;
      y0 = vSeed->y0;
      dxdz = vSeed->dxdz;
      dydz = vSeed->dydz;
    }
    else {
      LOG_WARN << "StBTofCalibMaker -- No Database for beamline" << endm;
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
      LOG_WARN << "StBTofCalibMaker:: Beam line must be tilted!" << endm;
      nxy=dxdz=1.e-5;
    }
    double p0=pt/nxy;
    double px   = p0*dxdz;
    double py   = p0*dydz;
    double pz   = p0; // approximation: nx,ny<<0
    StThreeVectorD MomFstPt(px*GeV, py*GeV, pz*GeV);
    if(mBeamHelix) delete mBeamHelix;
    mBeamHelix = new StPhysicalHelixD(MomFstPt,origin,0.5*tesla,1.);


  return kStOK;
}

//____________________________________________________________________________
Int_t StBTofCalibMaker::FinishRun(int runnumber)
{
 if(mBeamHelix) delete mBeamHelix;  
  mBeamHelix = 0;

  return kStOK;
}

//_____________________________________________________________________________
Int_t StBTofCalibMaker::Finish()
{
  if (mHistoFileName!="") writeHistograms();
  return kStOK;
}

//_____________________________________________________________________________
Int_t StBTofCalibMaker::Make()
{
  LOG_INFO << " StBTofCalibMaker::Maker: starting ..." << endm;
  if(!mValidCalibPar) {
    LOG_WARN << " No valid calibration parameters. Skip ... " << endm;
    return kStOK;
  }

  resetVpd();
  loadVpdData();

  if(!mMuDstIn) processStEvent();
  else          processMuDst();

  writeStartTime();

  return kStOK;
}

//_____________________________________________________________________________
void StBTofCalibMaker::processStEvent()
{
  // event selection  // no primary vertex required
  if( !mEvent ||
      !mEvent->btofCollection() ||
      !mEvent->btofCollection()->hitsPresent() ) {
    LOG_WARN << "StBTofCalibMaker -- nothing to do ... bye-bye" << endm;
    return;
  }

  StBTofCollection *theTof = mEvent->btofCollection();
  StSPtrVecBTofHit &tofHits = theTof->tofHits();
  Int_t nhits = tofHits.size();
  LOG_INFO << " Fired TOF cells + upVPD tubes : " << nhits << endm;

  mEvtVtxZ = -9999.;
  mProjVtxZ = -9999.;
  float dcaRmin = 9999.;   
  float rankHmax = -1.;

  if(mUseEventVertex) {
    ///
    /// select the vertex with highest positive rank within the VPDVtxZ cut range
    ///
    int nVtx = mEvent->numberOfPrimaryVertices();
    for(int i=0;i<nVtx;i++) {
      StPrimaryVertex *pVtx = mEvent->primaryVertex(i);
      if(!pVtx) continue;
      if(pVtx->ranking()<0) continue;               //! select positive ranking vertex
      if(fabs(pVtx->position().z())>200.) continue;   //! within 200 cm
      if(fabs(pVtx->position().z()-mVPDVtxZ)>VZDIFFCUT) continue;  //! VPDVtxZ cut
      if(pVtx->ranking()<rankHmax) continue;
      mEvtVtxZ = pVtx->position().z();
      rankHmax = pVtx->ranking();
    }

    if(rankHmax<0.) mEvtVtxZ = -9999.;
    tstart(mEvtVtxZ, &mTStart, &mTDiff);

  } else {
    ///
    /// select the projection position with smallest dcaR within the VPDVtxZ cut range
    ///
    for(int i=0;i<nhits;i++) {
      StBTofHit *aHit = dynamic_cast<StBTofHit*>(tofHits[i]);
      if(!aHit) continue;
      int trayId = aHit->tray();
      if(trayId>0&&trayId<=mNTray) {
        StGlobalTrack *gTrack = dynamic_cast<StGlobalTrack*>(aHit->associatedTrack());
        if(!gTrack) continue;
        StTrackGeometry *theTrackGeometry = gTrack->geometry();

        StThreeVectorD tofPos =  theTrackGeometry->helix().at(theTrackGeometry->helix().pathLengths(*mBeamHelix).first);
        StThreeVectorD beamPos = mBeamHelix->at(theTrackGeometry->helix().pathLengths(*mBeamHelix).second);
        StThreeVectorD dcatof = tofPos - beamPos;
        if(Debug()) {
          LOG_INFO<<" tofPos(x,y,z) = "<<tofPos.x()<<","<<tofPos.y()<<","<<tofPos.z()<<endm;
          LOG_INFO<<"beamPos(x,y,z) = "<<beamPos.x()<<","<<beamPos.y()<<","<<beamPos.z()<<endm;
          LOG_INFO<<"  dca  (x,y,z) = "<<dcatof.x()<<","<<dcatof.y()<<","<<dcatof.z()<<endm;
          LOG_INFO<<" 2D dca        = "<<sqrt(pow(dcatof.x(),2)+pow(dcatof.y(),2))<<endm;
          LOG_INFO<<" 2D signed dca = "<<theTrackGeometry->helix().geometricSignedDistance(beamPos.x(),beamPos.y())<<endm;
        }

        /// track projection z should be close to vzvpd
        if(fabs(tofPos.z()-mVPDVtxZ)>VZDIFFCUT) continue;

        if(dcaRmin>dcatof.perp()) {
          mProjVtxZ = tofPos.z();
          dcaRmin = dcatof.perp();
        }
      } // end if
    } // end loop tofhits

    if(dcaRmin>DCARCUT)  mProjVtxZ = -9999.;  // beam line contrain
    tstart(mProjVtxZ, &mTStart, &mTDiff);

  } // end if (mUseEventVertex)

//  if(Debug()) {
  if(1) {
    LOG_INFO << " projected Vertex Z = " << mProjVtxZ << endm;
    LOG_INFO << " Tstart = " << mTStart << " Tdiff = " << mTDiff << endm;
    LOG_INFO << " NWest = " << mNWest << " NEast = " << mNEast << " TdcSum West = " << mTSumWest << " East = " << mTSumEast << endm; 
    LOG_INFO << " mValidCalibPar = " << mValidCalibPar << " mValidStartTime = " << mValidStartTime << endm;
    LOG_INFO << " mVpdVz = " << mVPDVtxZ << endm;
  }

  if(mTStart<-1000.) {
    LOG_INFO << " No valid start time for this event. Skip ..." << endm;
    mValidStartTime = kFALSE;
    return;
  } else {
    mValidStartTime = kTRUE;
  }

  //---------------------------------------
  // BTof calibration
  //---------------------------------------

  for(int i=0;i<nhits;i++) {
    StBTofHit *aHit = dynamic_cast<StBTofHit*>(tofHits[i]);
    if(!aHit) continue;
    int trayId = aHit->tray();
    if(trayId<=0 || trayId>mNTray) continue;

    StGlobalTrack *gTrack = dynamic_cast<StGlobalTrack*>(aHit->associatedTrack());
    if(!gTrack) {
      if(Debug()) { LOG_INFO << " No associated Track with this hit." << endm;}
      continue;
    }

    const StPtrVecTrackPidTraits& theTofPidTraits = gTrack->pidTraits(kTofId);
    if(!theTofPidTraits.size()) continue;

    StTrackPidTraits *theSelectedTrait = theTofPidTraits[theTofPidTraits.size()-1];
    if(!theSelectedTrait) continue;

    StBTofPidTraits *pidTof = dynamic_cast<StBTofPidTraits *>(theSelectedTrait);
    if(!pidTof) continue;

    double tot = aHit->tot(); // ns
    double tdc = aHit->leadingEdgeTime();
    double tof = tdc - mTStart; 
    Double_t zhit = pidTof->zLocal();

    int moduleChan = (aHit->module()-1)*6 + (aHit->cell()-1);
    Double_t tofcorr = tofAllCorr(tof, tot, zhit, trayId, moduleChan);
    if(tofcorr<0.) {
      if(Debug()) { LOG_INFO << " Calibration failed! ... " << endm; }
      continue;
    }

    pidTof->setTimeOfFlight((Float_t)tofcorr);

    /// find the primary track if any
    StPrimaryTrack *pTrack = dynamic_cast<StPrimaryTrack *>(gTrack->node()->track(primary));
    StBTofPidTraits *ppidTof = 0;
    if(pTrack) {
      const StPtrVecTrackPidTraits& pTofPidTraits = pTrack->pidTraits(kTofId);
      if(!pTofPidTraits.size()) continue;

      StTrackPidTraits *pSelectedTrait = pTofPidTraits[pTofPidTraits.size()-1];
      if(!pSelectedTrait) continue;

      ppidTof = dynamic_cast<StBTofPidTraits *>(pSelectedTrait);

      if(ppidTof && mUseEventVertex) {
        ppidTof->setTimeOfFlight((Float_t)tofcorr);
      }
    }

    /// PID calculation if the track is a "primary" track.
    Double_t L = -9999.;
    Double_t ptot = -9999.;
    Bool_t doPID = kFALSE;     //! switch indicating to calculate PID or not
    if(mUseEventVertex) {
      if(!pTrack) {
        if(Debug()) { LOG_INFO << " The associated track is not a primary one. Skip PID calculation! " << endm; }
      } else {
        StTrackGeometry *theTrackGeometry = pTrack->geometry();
        const StVertex *thisVertex = pTrack->vertex();
        if(!thisVertex) {
          if(Debug()) { LOG_INFO << " The associated track is not coming from any vertex. Skip PID calculation! " << endm; }
        } else {
          StThreeVectorF primPos = thisVertex->position();
          L = tofPathLength(&primPos, &pidTof->position(), theTrackGeometry->helix().curvature());
          ptot = pTrack->geometry()->momentum().mag();
          doPID = kTRUE;
        }
      }

    } else {

      StTrackGeometry *theTrackGeometry = gTrack->geometry();
      StThreeVectorD tofPos =  theTrackGeometry->helix().at(theTrackGeometry->helix().pathLengths(*mBeamHelix).first);
      StThreeVectorD dcatof = tofPos - mBeamHelix->at(theTrackGeometry->helix().pathLengths(*mBeamHelix).second);
      if(dcatof.perp()>DCARCUT) {
        if(Debug()) { LOG_INFO << " The projected position is far from beam line. Skip PID calculation! " << endm; }
      } else if(fabs(tofPos.z()-mVPDVtxZ)>VZDIFFCUT) {
        if(Debug()) { LOG_INFO << " This track is not coming from the same VPD vertex! Skip PID calculation! " << endm; }
      } else {
        L = tofPathLength(&tofPos, &pidTof->position(), theTrackGeometry->helix().curvature());
        ptot = gTrack->geometry()->momentum().mag();
        if(gTrack->dcaGeometry()) {
          ptot = gTrack->dcaGeometry()->momentum().mag();
        }
        doPID = kTRUE;
      }

    }

    if(!doPID) continue;

    Double_t beta = L/(tofcorr*(C_C_LIGHT/1.e9));

    Double_t b_e  = ptot/sqrt(ptot*ptot+M_ELECTRON*M_ELECTRON);
    Double_t b_pi = ptot/sqrt(ptot*ptot+M_PION_PLUS*M_PION_PLUS);
    Double_t b_k  = ptot/sqrt(ptot*ptot+M_KAON_PLUS*M_KAON_PLUS);
    Double_t b_p  = ptot/sqrt(ptot*ptot+M_PROTON*M_PROTON);

    float sigmae = -9999.;
    float sigmapi = -9999.;
    float sigmak = -9999.; 
    float sigmap = -9999.; 
    float res = 0.013;  // 0.013 by default - 1/beta resolution
    if(fabs(res)>1.e-5) {
      sigmae = (Float_t)((1./beta-1./b_e)/res);
      sigmapi = (Float_t)((1./beta-1./b_pi)/res);
      sigmak = (Float_t)((1./beta-1./b_k)/res);  
      sigmap = (Float_t)((1./beta-1./b_p)/res);  
    }

    pidTof->setPathLength((Float_t)L);
    pidTof->setBeta((Float_t)beta);
    pidTof->setSigmaElectron(sigmae);
    pidTof->setSigmaPion(sigmapi);
    pidTof->setSigmaKaon(sigmak);
    pidTof->setSigmaProton(sigmap);

    if(Debug()) {
      LOG_INFO << " storing BTofPidTraits for the global track" << endm;
    }

    if(mUseEventVertex) {

      if(ppidTof) {

        ppidTof->setPathLength((Float_t)L);
        ppidTof->setBeta((Float_t)beta);
        ppidTof->setSigmaElectron(sigmae);
        ppidTof->setSigmaPion(sigmapi);
        ppidTof->setSigmaKaon(sigmak);
        ppidTof->setSigmaProton(sigmap);

        if(Debug()) { 
          LOG_INFO << " storing BTofPidTraits for the primary track" << endm;
        }
      } // end if ppidTof
    }  // end if mUseEventVertex

  }  // end tof hits

  return;
}

//_____________________________________________________________________________
void StBTofCalibMaker::processMuDst()
{
  if(!mMuDst) {
    LOG_WARN << " No MuDst ... bye-bye" << endm;
    return;
  }

  Int_t nhits = mMuDst->numberOfBTofHit();
  LOG_INFO << " Fired TOF cells + upVPD tubes : " << nhits << endm;

  mEvtVtxZ  = -9999.;
  mProjVtxZ = -9999.;
  float dcaRmin = 9999.;
  float rankHmax = -1.;

  if(mUseEventVertex) {
    ///
    /// select the vertex with highest positive rank within the VPDVtxZ cut range
    ///
    int nVtx = mMuDst->numberOfPrimaryVertices();
    for(int i=0;i<nVtx;i++) {
      StMuPrimaryVertex* pVtx = mMuDst->primaryVertex(i);
      if(!pVtx) continue;
      if(pVtx->ranking()<0) continue;               //! select positive ranking vertex
      if(fabs(pVtx->position().z())>200.) continue;   //! within 200 cm
      if(fabs(pVtx->position().z()-mVPDVtxZ)>VZDIFFCUT) continue;  //! VPDVtxZ cut
      if(pVtx->ranking()<rankHmax) continue;
      mEvtVtxZ = pVtx->position().z();
      rankHmax = pVtx->ranking();
    }

    if(rankHmax<0.) mEvtVtxZ = -9999.;
    tstart(mEvtVtxZ, &mTStart, &mTDiff);

  } else {
    ///
    /// select the projection position with smallest dcaR within the VPDVtxZ cut range
    ///
    for(int i=0;i<nhits;i++) {
      StMuBTofHit *aHit = (StMuBTofHit*)mMuDst->btofHit(i);
      if(!aHit) continue;
      int trayId = aHit->tray();
      if(trayId>0&&trayId<=mNTray) {
        StMuTrack *gTrack = aHit->globalTrack();
        if(!gTrack) continue;

        StPhysicalHelixD thisHelix = gTrack->helix();

        StThreeVectorD tofPos =  thisHelix.at(thisHelix.pathLengths(*mBeamHelix).first);
        StThreeVectorD dcatof = tofPos - mBeamHelix->at(thisHelix.pathLengths(*mBeamHelix).second);

        /// track projection z should be close to vzvpd
        if(fabs(tofPos.z()-mVPDVtxZ)>VZDIFFCUT) continue;

        if(dcaRmin>dcatof.perp()) {
          mProjVtxZ = tofPos.z();
          dcaRmin = dcatof.perp();
        }
      } // end if
    } // end loop tofhits

    if(dcaRmin>DCARCUT)  mProjVtxZ = -9999.;  // beam line contrain
    tstart(mProjVtxZ, &mTStart, &mTDiff);

  }

//  if(Debug()) {  
  if(1) {
    LOG_INFO << " projected Vertex Z = " << mProjVtxZ << endm;
    LOG_INFO << " Tstart = " << mTStart << " Tdiff = " << mTDiff << endm;
    LOG_INFO << " NWest = " << mNWest << " NEast = " << mNEast << " TdcSum West = " << mTSumWest << " East = " << mTSumEast << endm;
    LOG_INFO << " mValidCalibPar = " << mValidCalibPar << " mValidStartTime = " << mValidStartTime << endm;
    LOG_INFO << " mVpdVz = " << mVPDVtxZ << endm;
  }

  if(mTStart<-1000.) {
    LOG_INFO << " No valid start time for this event. Skip ..." << endm;
    mValidStartTime = kFALSE;
    return;
  } else {
    mValidStartTime = kTRUE;
  }

  //---------------------------------------
  // BTof calibration
  //---------------------------------------

  for(int i=0;i<nhits;i++) {
    StMuBTofHit *aHit = (StMuBTofHit*)mMuDst->btofHit(i);
    if(!aHit) continue;
    int trayId = aHit->tray();
    if(trayId<=0 || trayId>mNTray) continue;

    StMuTrack *gTrack = aHit->globalTrack();
    if(!gTrack) {
      if(Debug()) { LOG_INFO << " No associated Track with this hit." << endm; }
      continue;
    }

    StMuBTofPidTraits pidTof = gTrack->btofPidTraits();

    double tot = aHit->tot(); // ns
    double tdc = aHit->leadingEdgeTime();
    while(tdc>TMAX) tdc -= TMAX;
    double tof = tdc - mTStart;
    Double_t zhit = pidTof.zLocal();

    int moduleChan = (aHit->module()-1)*6 + (aHit->cell()-1);
    Double_t tofcorr = tofAllCorr(tof, tot, zhit, trayId, moduleChan);
    if(tofcorr<0.) {
      if(Debug()) { LOG_INFO << " Calibration failed! ... " << endm; }
      continue;
    }

    /// store the corrected tof information for all global matches
    pidTof.setTimeOfFlight((Float_t)tofcorr);

    /// find the primary track if any
    StMuTrack *pTrack = aHit->primaryTrack();
    StMuBTofPidTraits ppidTof;
    if(pTrack) {
      ppidTof = pTrack->btofPidTraits();
      if(mUseEventVertex) ppidTof.setTimeOfFlight((Float_t)tofcorr);
    }

    /// PID calculation if the track is a "primary" track.
    Double_t L = -9999.;
    Double_t ptot = -9999.;
    Bool_t doPID = kFALSE;
    if(mUseEventVertex) {
      if(!pTrack) {
        if(Debug()) { LOG_INFO << " The associated track is not a primary one. Skip PID calculation! " << endm; }
      } else {
        int iv = pTrack->vertexIndex();
        StMuPrimaryVertex *thisVertex = mMuDst->primaryVertex(iv);
        if(!thisVertex) {
          if(Debug()) { LOG_INFO << " The associated track is not coming from any vertex. Skip PID calculation! " << endm; }
        } else {
          StThreeVectorF primPos = thisVertex->position();
          StPhysicalHelixD thisHelix = pTrack->helix();
          L = tofPathLength(&primPos, &pidTof.position(), thisHelix.curvature());
          ptot = pTrack->momentum().mag();
          doPID = kTRUE;
        }
      }

    } else {

      StPhysicalHelixD gHelix = gTrack->helix();
      StThreeVectorD tofPos =  gHelix.at(gHelix.pathLengths(*mBeamHelix).first);
      StThreeVectorD dcatof = tofPos - mBeamHelix->at(gHelix.pathLengths(*mBeamHelix).second);
      if(dcatof.perp()>DCARCUT) {
        if(Debug()) { LOG_INFO << " The projected position is far from beam line. Skip PID calculation! " << endm; }
      } else if(fabs(tofPos.z()-mVPDVtxZ)>VZDIFFCUT) {
        if(Debug()) { LOG_INFO << " This track is not coming from the same VPD vertex! Skip PID calculation! " << endm; }
      } else {
        L = tofPathLength(&tofPos, &pidTof.position(), gHelix.curvature());
        ptot = gTrack->momentum().mag();
        doPID = kTRUE;
      }
    }

    if(doPID) {
      Double_t beta = L/(tofcorr*(C_C_LIGHT/1.e9));

      Double_t b_e  = ptot/sqrt(ptot*ptot+M_ELECTRON*M_ELECTRON);
      Double_t b_pi = ptot/sqrt(ptot*ptot+M_PION_PLUS*M_PION_PLUS);
      Double_t b_k  = ptot/sqrt(ptot*ptot+M_KAON_PLUS*M_KAON_PLUS);
      Double_t b_p  = ptot/sqrt(ptot*ptot+M_PROTON*M_PROTON);

      float sigmae = -9999.;
      float sigmapi = -9999.;
      float sigmak = -9999.;
      float sigmap = -9999.;
      float res = 0.013;  // 0.013 by default - 1/beta resolution
      if(fabs(res)>1.e-5) {
        sigmae = (Float_t)((1./beta-1./b_e)/res);
        sigmapi = (Float_t)((1./beta-1./b_pi)/res);
        sigmak = (Float_t)((1./beta-1./b_k)/res);
        sigmap = (Float_t)((1./beta-1./b_p)/res);
      }

      pidTof.setPathLength((Float_t)L);
      pidTof.setBeta((Float_t)beta);
      pidTof.setSigmaElectron(sigmae);
      pidTof.setSigmaPion(sigmapi);
      pidTof.setSigmaKaon(sigmak);
      pidTof.setSigmaProton(sigmap);

      if(mUseEventVertex && pTrack) {

        ppidTof.setPathLength((Float_t)L);
        ppidTof.setBeta((Float_t)beta);
        ppidTof.setSigmaElectron(sigmae);
        ppidTof.setSigmaPion(sigmapi);
        ppidTof.setSigmaKaon(sigmak);
        ppidTof.setSigmaProton(sigmap);
      }
    }

    gTrack->setBTofPidTraits(pidTof);
    if(Debug()) {
        LOG_INFO << " storing BTofPidTraits for the global track" << endm;
    }

    if(mUseEventVertex && pTrack) {
      pTrack->setBTofPidTraits(ppidTof);
      if(Debug()) {
        LOG_INFO << " storing BTofPidTraits for the primary track" << endm;
      }
    }
  }  // end tof hits

  return;
}

//_____________________________________________________________________________
void StBTofCalibMaker::loadVpdData()
{
  if(mMuDstIn) {
    StMuDstMaker *mMuDstMaker = (StMuDstMaker *)GetMaker("MuDst");
    if(!mMuDstMaker) {
      LOG_WARN << " No MuDstMaker ... bye-bye" << endm;
      return;
    }
    mMuDst = mMuDstMaker->muDst();
    if(!mMuDst) {
      LOG_WARN << " No MuDst ... bye-bye" << endm;
      return;
    }

    mBTofHeader = mMuDst->btofHeader();
  } else {
    mEvent = (StEvent *) GetInputDS("StEvent");

    if(!mEvent) return;
    StBTofCollection *btofColl = mEvent->btofCollection();
    if(!btofColl) return;
    mBTofHeader = btofColl->tofHeader();
  }

   if(!mBTofHeader) return;

   mTSumWest = 0;
   mTSumEast = 0;
   mVPDHitPatternWest = mBTofHeader->vpdHitPattern(west);
   mVPDHitPatternEast = mBTofHeader->vpdHitPattern(east);
   mNWest = mBTofHeader->numberOfVpdHits(west);
   mNEast = mBTofHeader->numberOfVpdHits(east);
   mVPDVtxZ = mBTofHeader->vpdVz();

   for(int i=0;i<mNVPD;i++) {
     mVPDLeTime[i] = mBTofHeader->vpdTime(west, i+1);
     if(mVPDLeTime[i]>0.) mTSumWest += mVPDLeTime[i];
     if(Debug()) {
       LOG_INFO << " loading VPD West tubeId = " << i+1 << " time = " << mVPDLeTime[i] << endm;
     }
   }

   for(int i=0;i<mNVPD;i++) {
     mVPDLeTime[i+mNVPD] = mBTofHeader->vpdTime(east, i+1);
     if(mVPDLeTime[i+mNVPD]>0.) mTSumEast += mVPDLeTime[i+mNVPD];
     if(Debug()) {
       LOG_INFO << " loading VPD East tubeId = " << i+1 << " time = " << mVPDLeTime[i+mNVPD] << endm;
     }
   }

   return;
}

//_____________________________________________________________________________
void StBTofCalibMaker::writeStartTime()
{
  if(mBTofHeader) {
    mBTofHeader->setTStart(mTStart);
    mBTofHeader->setTDiff(mTDiff);
  }

  return;
}

//_____________________________________________________________________________
Double_t StBTofCalibMaker::tofAllCorr(const Double_t tof, const Double_t tot, const Double_t z, const Int_t iTray, const Int_t iModuleChan)
{
  int tray = iTray;
  int module = iModuleChan/6 + 1;
  int cell = iModuleChan%6 + 1;
  int board = iModuleChan/24 + 1;
//  if(Debug()) {
  if(1) {
    LOG_INFO << "\nStBTofCalibMaker::btofAllCorr: BTof calibrating...\n" 
  	     << "\tDoing Calibration in BTOF Tray " << tray << " Module " << module << " Cell " << cell
	     << "\n\tinput tof = " << tof
	     << "  TOT = " << tot << "  Zlocal = " << z << endm;
  }
  
  Double_t tofcorr = tof;

  tofcorr -= mTofTZero[tray-1][module-1][cell-1];

  LOG_DEBUG << "T0 correction: "<<mTofTZero[tray-1][module-1][cell-1]<<endm;

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
      LOG_DEBUG << "TOT correction: "<<dcorr<<endm; 

      tofcorr -= dcorr;
    } else {
      LOG_WARN << " TOT out of range! EXIT! " << endm;
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
      double dcorr = y1 + (z-x1)*(y2-y1)/(x2-x1);

      tofcorr -= dcorr;
      LOG_DEBUG << "zHit correction: "<<dcorr<<endm;

    } else {
      LOG_WARN << " Z out of range! EXIT! " << endm;
      return -9999.;
    }
    
  }

  LOG_INFO << "  Corrected tof: tofcorr = " << tofcorr << endm;
  return tofcorr;
}

//_____________________________________________________________________________
void StBTofCalibMaker::tstart(const Double_t vz, Double_t *tstart, Double_t *tdiff)
{
  *tstart = -9999.;
  *tdiff = -9999.;

  if(fabs(vz)>200.) {LOG_INFO << "tstart: vz too big" << endm; return;}

  Double_t TSum = mTSumEast + mTSumWest;

  if(mNEast+mNWest>0) {
    *tstart = (TSum-(mNEast-mNWest)*vz/(C_C_LIGHT/1.e9))/(mNEast+mNWest);
  }
  if ( mNEast>=mVPDEastHitsCut && mNWest>=mVPDWestHitsCut ) {
    *tdiff = (mTSumEast/mNEast - mTSumWest/mNWest)/2. - vz/(C_C_LIGHT/1.e9);
  }

  return;
}

//_____________________________________________________________________________
void StBTofCalibMaker::bookHistograms()
{
  hEventCounter = new TH1D("eventCounter","eventCounter",20,0,20);
}

//_____________________________________________________________________________
void StBTofCalibMaker::writeHistograms()
{
  // Output file
  TFile *theHistoFile =  new TFile(mHistoFileName.c_str(), "RECREATE");
  LOG_INFO << "StBTofCalibMaker::writeHistograms()"
       << " histogram file " <<  mHistoFileName << endm;

  theHistoFile->cd();

  if(mHisto) {
    hEventCounter->Write();
  }
  return;
}
