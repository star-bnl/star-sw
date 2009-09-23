/*******************************************************************
 *
 * $Id: StBTofCalibMaker.cxx,v 1.1 2009/09/23 02:28:41 geurts Exp $
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
#include "tables/St_tofTzero_Table.h"
#include "tables/St_tofTotbCorr_Table.h"
#include "tables/St_tofZbCorr_Table.h"

#include "tables/St_vertexSeed_Table.h"

#include "tables/St_tofTOffset_Table.h"
#include "tables/St_tofPhaseOffset_Table.h"

#include "StBTofUtil/tofPathLength.hh"
#include "StBTofUtil/StBTofHitCollection.h"
#include "StBTofUtil/StBTofGeometry.h"
#include "StBTofCalibMaker.h"

#ifdef __ROOT__
ClassImp(StBTofCalibMaker)
#endif

//_____________________________________________________________________________
StBTofCalibMaker::StBTofCalibMaker(const char *name) : StMaker(name)
{
  /// default constructor
  /// set the default parameters for TDC, ADC cut etc.
  /// Reset the calibration parameters
  setVPDHitsCut(1,1);
  setOuterGeometry(true);

  mValidStartTime = kTRUE;
  mSlewingCorr = kTRUE;
  mUseEventVertex = kFALSE;
  // default initialization from database
  mInitFromFile = kFALSE;

  // assign default locations and names to the calibration files 
  setCalibFilePvpd("/star/institutions/rice/calib/default/pvpdCali_4DB.dat");
  setCalibFileTot("/star/institutions/rice/calib/default/totCali_4DB.dat");
  setCalibFileZhit("/star/institutions/rice/calib/default/zCali_4DB.dat");
  setCalibFileT0("/star/institutions/rice/calib/default/t0_4DB.dat");  


  StThreeVectorD MomFstPt(0.,0.,9999.);
  StThreeVectorD origin(0.,0.,0.);
  mBeamHelix = new StPhysicalHelixD(MomFstPt,origin,0.5*tesla,1.);

  resetPars();
}

//_____________________________________________________________________________
StBTofCalibMaker::~StBTofCalibMaker()
{
  resetPars();
}

//_____________________________________________________________________________
void StBTofCalibMaker::resetPars()
{
  memset(mTofTotEdge, 0, sizeof(mTofTotEdge));
  memset(mTofTotCorr, 0, sizeof(mTofTotCorr));
  memset(mTofZEdge,   0, sizeof(mTofZEdge)  );
  memset(mTofZCorr,   0, sizeof(mTofZCorr)  );
  memset(mTofTZero,   0, sizeof(mTofTZero)  );
  memset(mVPDTotEdge, 0, sizeof(mVPDTotEdge));
  memset(mVPDTotCorr, 0, sizeof(mVPDTotCorr));
  memset(mVPDTZero,   0, sizeof(mVPDTZero)  );
  memset(mVPDLeTime,  0, sizeof(mVPDLeTime) );
  memset(mVPDTot,     0, sizeof(mVPDTot)    );

  mTStart = -9999.;
  mTDiff  = -9999.;
  mVPDVtxZ = -9999.;
  mProjVtxZ = -9999.;
  mEvtVtxZ = -9999.;
  mVPDHitPatternEast = 0;
  mVPDHitPatternWest = 0;
}

//____________________________________________________________________________
Int_t StBTofCalibMaker::Init()
{
  resetPars();
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

    /// open file and read VPD calibration parameters
    LOG_INFO << " - pvpd : " << mCalibFilePvpd << endm;  
    ifstream inData;
    inData.open(mCalibFilePvpd.c_str());
    int nchl, nbin;
    for(int i=0;i<mNVPD*2;i++) {
      inData>>nchl;
      inData>>nbin;
      for(int j=0;j<=nbin;j++) inData>>mVPDTotEdge[i][j];
      for(int j=0;j<=nbin;j++) inData>>mVPDTotCorr[i][j];
    }
    inData.close();

    /// open file and read Time-over-Threshold calibration parameters
    LOG_INFO << " - ToT : " << mCalibFileTot << endm;  
    inData.open(mCalibFileTot.c_str());
    int trayId, boardId;
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

    if(numRows!=mNTray*mNTDIG+mNVPD*2) {
      LOG_WARN  << " Mis-matched number of rows in tofTotbCorr table! " << endm;
      //      return kStErr;
    }

    LOG_DEBUG << " Number of rows read in: " << numRows << " for ToT correction" << endm;

    for (Int_t i=0;i<mNTray*mNTDIG+mNVPD*2;i++) {
      short trayId = totCorr[i].trayId;
      short moduleId = totCorr[i].moduleId;
      short boardId = (moduleId-1)/4+1;      // used for trays
      short cellId = totCorr[i].cellId;      // used for vpds

      int index = (trayId-mNTray-1)*mNVPD+(cellId-1);   // used for vpd index

      LOG_DEBUG << " tray " << trayId << " board " << boardId << " cell " << cellId << endm;
      for(Int_t j=0;j<mNBinMax;j++) {
	if(trayId==mWestVpdTrayId||trayId==mEastVpdTrayId) { // upVPD east west
	  mVPDTotEdge[index][j] = totCorr[i].tot[j];
	  mVPDTotCorr[index][j] = totCorr[i].corr[j];
	} else if(trayId>0&&trayId<=mNTray){ // trays
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
      LOG_WARN << " Mis-matched number of rows in tofZbCorr table! " << endm;
      //      return kStErr;
    }
    LOG_DEBUG << " Number of rows read in: " << numRows << " for Z correction" << endm;


    for (Int_t i=0;i<mNTray*mNTDIG;i++) {
      short trayId = totCorr[i].trayId;
      short moduleId = totCorr[i].moduleId;
      short boardId = (moduleId-1)/4+1;      // used for trays
      short cellId = totCorr[i].cellId;      // used for vpds

      LOG_DEBUG << " tray " << trayId << " board " << boardId << " cell " << cellId << endm;
      for(Int_t j=0;j<mNBinMax;j++) {
	if(trayId>0&&trayId<=mNTray) {  // trays
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
      LOG_WARN << " Mis-matched number of rows in tofTOffset table! " << endm;
      //      return kStErr;
    }
    for (Int_t i=0;i<mNTray;i++) {
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

  mTStart = -9999.;
  mTDiff  = -9999.;
  mVPDVtxZ = -9999.;
  mProjVtxZ = -9999.;
  mEvtVtxZ = -9999.;
  mVPDHitPatternEast = 0;
  mVPDHitPatternWest = 0;

  mEvent = (StEvent *) GetInputDS("StEvent");
  
  // event selection  // no primary vertex required
  if( !mEvent ||
      !mEvent->btofCollection() ||
      !mEvent->btofCollection()->hitsPresent() ) {
    LOG_WARN << "StBTofCalibMaker -- nothing to do ... bye-bye" << endm;
    return kStOK;
  }

  StBTofCollection *theTof = mEvent->btofCollection();
  StSPtrVecBTofHit &tofHits = theTof->tofHits();
  Int_t nhits = tofHits.size();
  LOG_INFO << " Fired TOF cells + upVPD tubes : " << nhits << endm;

  mEvtVtxZ = -9999.;
  if(mEvent->primaryVertex()) {
    mEvtVtxZ = mEvent->primaryVertex()->position().z();
  }

  
  //-------------------------------------------------
  // VPD calibration and calculate the start timing
  //-------------------------------------------------
  for(int i=0;i<2*mNVPD;i++) {
    mVPDLeTime[i] = -9999.;
    mVPDTot[i] = -9999.;
  }
  float dcaRmin = 9999.;
  for(int i=0;i<nhits;i++) {
    StBTofHit *aHit = dynamic_cast<StBTofHit*>(tofHits[i]);
    if(!aHit) continue;
    int trayId = aHit->tray();
    if(trayId==mWestVpdTrayId || trayId==mEastVpdTrayId) {   // VPD this time
      int tubeId = aHit->cell();
      mVPDLeTime[(trayId-121)*mNVPD+(tubeId-1)] = aHit->leadingEdgeTime();
      mVPDTot[(trayId-121)*mNVPD+(tubeId-1)] = aHit->tot();
    } else if(trayId>0&&trayId<=120) {
      StTrack *thisTrack = aHit->associatedTrack();
      if(!thisTrack) continue;
      LOG_INFO << " tray/module/cell = " << trayId << "/" << aHit->module() << "/" << aHit->cell() << endm;
      StTrackGeometry *theTrackGeometry = thisTrack->geometry();

      StThreeVectorF mom = theTrackGeometry->momentum();
      LOG_INFO<<"track mom; pt="<<mom.perp()<<" eta="<<mom.pseudoRapidity()<<" phi="<<mom.phi()<<endm;

      StThreeVectorD tofPos =  theTrackGeometry->helix().at(theTrackGeometry->helix().pathLengths(*mBeamHelix).first);
      LOG_INFO<<"tofPos(x,y,z)= "<<tofPos.x()<<"  "<<tofPos.y()<<"  "<<tofPos.z()<<endm;
      StThreeVectorD dcatof = tofPos - mBeamHelix->at(theTrackGeometry->helix().pathLengths(*mBeamHelix).second);
      LOG_INFO<<"dcatof(x,y)= "<<dcatof.x()<<"  "<<dcatof.y()<<endm;
   
      if(dcaRmin>dcatof.perp()) {
         mProjVtxZ = tofPos.z();
         dcaRmin = dcatof.perp();
      }
    } // end if
  } // end loop tofhits


  tsum(mVPDTot, mVPDLeTime);
  if(mUseEventVertex) tstart(mEvtVtxZ, &mTStart, &mTDiff);
  else {
    if(dcaRmin>1.)  mProjVtxZ = -9999.;  // beam line contrain
    tstart(mProjVtxZ, &mTStart, &mTDiff);
  }

  LOG_INFO << " projected Vertex Z = " << mProjVtxZ << endm;
  LOG_INFO << " Tstart = " << mTStart << " Tdiff = " << mTDiff << endm;
  LOG_INFO << " NWest = " << mNWest << " NEast = " << mNEast << " TdcSum West = " << mTSumWest << " East = " << mTSumEast << endm;
  if(mTStart<-1000.) {
    LOG_INFO << " mTStart not available!" << endm;
    mValidStartTime = kFALSE;
  } else {
    mValidStartTime = kTRUE;
  }
  LOG_INFO << " mValidCalibPar = " << mValidCalibPar << " mValidStartTime = " << mValidStartTime << endm;

  /// Fill vpd information in StBTofCollection
  StBTofHeader *tofHeader = theTof->tofHeader();
  for(int i=0;i<mNVPD;i++) {
    tofHeader->setVpdTime(west, i+1, mVPDLeTime[i]);
    tofHeader->setVpdTime(east, i+1, mVPDLeTime[i+mNVPD]);
  }
  tofHeader->setVpdHitPattern(east, mVPDHitPatternEast);
  tofHeader->setVpdHitPattern(west, mVPDHitPatternWest);
  tofHeader->setVpdVz(mVPDVtxZ,0);
  tofHeader->setTStart(mTStart);
  tofHeader->setTDiff(mTDiff);

  LOG_INFO << " TofCollection: NWest = " << tofHeader->numberOfVpdHits(west) << " NEast = " << tofHeader->numberOfVpdHits(east) << endm;
  LOG_INFO <<" vpd vz = " << mVPDVtxZ << " event vz = " << mEvtVtxZ<<endm;

  if(!mValidStartTime) {
    LOG_INFO << " No valid start time for this event. Skip ..." << endm;
    return kStOK;
  }

  //---------------------------------------
  // Tof calibration
  //---------------------------------------

  for(int i=0;i<nhits;i++) {
    StBTofHit *aHit = dynamic_cast<StBTofHit*>(tofHits[i]);
    if(!aHit) continue;
    int trayId = aHit->tray();
    if(trayId<=0 || trayId>mNTray) continue;

//    if(Debug()) {
      LOG_INFO << " A new TOF Hit ... " << endm;
//    }

    StTrack *gTrack = aHit->associatedTrack();
    if(!gTrack) {
      LOG_INFO << " No associated Track with this hit." << endm;
      continue;
    }
    StPrimaryTrack *thisTrack = 0;

    const StPtrVecTrackPidTraits& theTofPidTraits = gTrack->pidTraits(kTofId);
    if(!theTofPidTraits.size()) continue;

    StTrackPidTraits *theSelectedTrait = theTofPidTraits[theTofPidTraits.size()-1];
    if(!theSelectedTrait) continue;

    StBTofPidTraits *pidTof = dynamic_cast<StBTofPidTraits *>(theSelectedTrait);
    if(!pidTof) continue;

    double tot = aHit->tot(); // ns
    double tdc = aHit->leadingEdgeTime();
//    tdc -= mPhaseOffset8;
    while(tdc>TMAX) tdc -= TMAX;
    double tof = tdc - mTStart; 
    Double_t zhit = pidTof->zLocal();

    int moduleChan = (aHit->module()-1)*6 + (aHit->cell()-1);
    Double_t tofcorr = tofAllCorr(tof, tot, zhit, trayId, moduleChan);

    Double_t L = -9999.;
    Double_t ptot = -9999.;   
    if(mUseEventVertex) {

      thisTrack = dynamic_cast<StPrimaryTrack *>(gTrack->node()->track(primary));
      if(!thisTrack) {
        LOG_INFO << " The associated track is not a primary one. Skip the calibration! " << endm;
        continue;
      }
      StTrackGeometry *theTrackGeometry = thisTrack->geometry();
      const StVertex *thisVertex = thisTrack->vertex();
      if(!thisVertex) {
        LOG_INFO << " The associated track is not coming from any vertex. Skip the calibration! " << endm;
        continue;
      }
      StThreeVectorF primPos = thisVertex->position();
      L = tofPathLength(&primPos, &pidTof->position(), theTrackGeometry->helix().curvature());
      ptot = thisTrack->geometry()->momentum().mag();

    } else {

      StTrackGeometry *theTrackGeometry = gTrack->geometry();
      StThreeVectorD tofPos =  theTrackGeometry->helix().at(theTrackGeometry->helix().pathLengths(*mBeamHelix).first);
      StThreeVectorD dcatof = tofPos - mBeamHelix->at(theTrackGeometry->helix().pathLengths(*mBeamHelix).second);
      if(dcatof.perp()>1.) {
        LOG_INFO << " The projected position is far from beam line. Skip the calibration! " << endm;
        continue;
      }
      if(fabs(tofPos.z()-mVPDVtxZ)>VZDIFFCUT) {
        LOG_INFO << " This track is not coming from the same VPD vertex! Skip ... " << endm;
        continue;
      }
      L = tofPathLength(&tofPos, &pidTof->position(), theTrackGeometry->helix().curvature());
      ptot = gTrack->geometry()->momentum().mag();

    }

    Double_t beta = L/(tofcorr*(C_C_LIGHT/1.e9));

    Double_t tofe = L/(C_C_LIGHT/1.e9)*sqrt(ptot*ptot+M_ELECTRON*M_ELECTRON)/ptot;   
    Double_t tofpi = L/(C_C_LIGHT/1.e9)*sqrt(ptot*ptot+M_PION_PLUS*M_PION_PLUS)/ptot;
    Double_t tofk = L/(C_C_LIGHT/1.e9)*sqrt(ptot*ptot+M_KAON_PLUS*M_KAON_PLUS)/ptot;
    Double_t tofp = L/(C_C_LIGHT/1.e9)*sqrt(ptot*ptot+M_PROTON*M_PROTON)/ptot;

    float sigmae = -9999.;
    float sigmapi = -9999.;
    float sigmak = -9999.; 
    float sigmap = -9999.; 
    float res = 0.100;  // 100 ps by default
    if(fabs(res)>1.e-5) {
      sigmae = (Float_t)((tofcorr-tofe)/res);
      sigmapi = (Float_t)((tofcorr-tofpi)/res);
      sigmak = (Float_t)((tofcorr-tofk)/res);  
      sigmap = (Float_t)((tofcorr-tofp)/res);  
    }

    if(mUseEventVertex) {

      StBTofPidTraits *ppidTof = new StBTofPidTraits();
      ppidTof->setTofHit(pidTof->tofHit());
      ppidTof->setMatchFlag(pidTof->matchFlag());
      ppidTof->setYLocal(pidTof->yLocal());
      ppidTof->setZLocal(pidTof->zLocal());
      ppidTof->setPosition(pidTof->position());

      ppidTof->setPathLength((Float_t)L);
      ppidTof->setTimeOfFlight((Float_t)tofcorr);
      ppidTof->setBeta((Float_t)beta);
      ppidTof->setSigmaElectron(sigmae);
      ppidTof->setSigmaPion(sigmapi);
      ppidTof->setSigmaKaon(sigmak);
      ppidTof->setSigmaProton(sigmap);

      thisTrack->addPidTraits(ppidTof);
//      if(Debug()) { 
        LOG_INFO << " storing BTofPidTraits for the primary track" << endm;
//      }
    } else {
      pidTof->setPathLength((Float_t)L);
      pidTof->setTimeOfFlight((Float_t)tofcorr);
      pidTof->setBeta((Float_t)beta);
      pidTof->setSigmaElectron(sigmae);
      pidTof->setSigmaPion(sigmapi);
      pidTof->setSigmaKaon(sigmak);
      pidTof->setSigmaProton(sigmap);

//      if(Debug()) { 
        LOG_INFO << " storing BTofPidTraits for the global track" << endm;
//      }
    }  // end if valid calib par
  }  // end tof hits

  return kStOK;
}

//_____________________________________________________________________________
Double_t StBTofCalibMaker::tofAllCorr(const Double_t tof, const Double_t tot, const Double_t z, const Int_t iTray, const Int_t iModuleChan)
{
  int tray = iTray;
  int module = iModuleChan/6 + 1;
  int cell = iModuleChan%6 + 1;
  int board = iModuleChan/24 + 1;
  LOG_INFO << "\nStBTofCalibMaker::btofAllCorr: BTof calibrating...\n" 
	   << "\tDoing Calibration in BTOF Tray " << tray << " Module " << module << " Cell " << cell
	   << "\n\tinput tof = " << tof
	   << "  TOT = " << tot << "  Zlocal = " << z << endm;
  
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
      LOG_WARN << " Z our of range! EXIT! " << endm;
      return -9999.;
    }
    
  }

  LOG_INFO << "  Corrected tof: tofcorr = " << tofcorr << endm;
  return tofcorr;
}

//_____________________________________________________________________________
void StBTofCalibMaker::tsum(const Double_t *tot, const Double_t *time)
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
        LOG_WARN << " Vpd West tube " << i+1 << " TOT out of range!" << endm;
        // out of range, remove this hit
      }
    }
      
    // East
    if( time[i+mNVPD]>0. && tot[i+mNVPD]>0. ) {
      double tmp = time[i+mNVPD];
//      double tmp = time[i+mNVPD] - mPhaseOffset8;
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
        LOG_WARN << " Vpd East tube " << i+1 << " TOT out of range!" << endm;
        // out of range, remove this hit
      }
    }
  }

  if ( mNEast>=mVPDEastHitsCut && mNWest>=mVPDWestHitsCut ) {
//    mTDiff = (mTSumEast/mNEast - mTSumWest/mNWest)/2. - mEvtVtxZ/(C_C_LIGHT/1.e9);
    mVPDVtxZ = (mTSumEast/mNEast - mTSumWest/mNWest)/2.*(C_C_LIGHT/1.e9);
  }
  
  return;
}
//_____________________________________________________________________________
void StBTofCalibMaker::tstart(const Double_t vz, Double_t *tstart, Double_t *tdiff)
{
  *tstart = -9999.;
  *tdiff = -9999.;

  if(fabs(vz)>200.) {LOG_INFO << "tstart: vz too big" << endm; return;}

  Double_t TSum = mTSumEast + mTSumWest;

  if ( mNEast>=mVPDEastHitsCut && mNWest>=mVPDWestHitsCut ) {
    *tstart = (TSum-(mNEast-mNWest)*vz/(C_C_LIGHT/1.e9))/(mNEast+mNWest);
    *tdiff = (mTSumEast/mNEast - mTSumWest/mNWest)/2. - vz/(C_C_LIGHT/1.e9);
  }

  return;
}
