/*******************************************************************
 *
 * $Id: StBTofCalibMaker.cxx,v 1.24 2021/05/29 23:57:08 geurts Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: - Tof Calibration Maker to do the calibration for pVPD
 *              (start timing) , TOF
 *              - store into StBTofPidTraits
 *
 *****************************************************************
 *Revision 1.23 2020/10/09 11pm, Zaochen
 *add (if (IAttr("btofFXT")) mFXTMode = kTRUE;) in the Init(),
 *it could allow the chain option "btofFXT" to turn on the FXTMode easily

 *Revision 1.22 2020/04/09 4pm, Zaochen
 *implement Xin's updates to allow more pions and protons for the T0s in FXT mode
 *add a flag mFXTMode: 0 for Collider mode, 1 for FXT mode
 *
 * $Log: StBTofCalibMaker.cxx,v $
 * Revision 1.24  2021/05/29 23:57:08  geurts
 * Updates to improve pp and pA handling - by Bassam Aboona (TAMU)
 *
 * Revision 1.23  2021/01/27 04:06:25  geurts
 * Introducing meaningful nTofSigma calculations in VPDstartless mode.
 *
 * Revision 1.22  2020/10/10 04:36:00  zye20
 * new added chain option btofFXT which could turn on FXTMode of StBTofCalibMaker
 *
 * Revision 1.21  2020/10/10 04:31:03  zye20
 * new added chain option btofFXT which could turn on FXTMode of StBTofCalibMaker
 *
 * Revision 1.20  2020/07/03 21:51:24  geurts
 * Removed unnecessary warning messages and replaced them with counters.
 *
 * Revision 1.19  2020/04/10 20:41:38  zye20
 * Xin add more pions and add protons for T0s in the FXT mode
 *
 * Revision 1.18  2019/04/23 05:49:57  jdb
 * Added function to allow forcing 0 starttime for totally startless BTOF usage in UPC
 *
 * Revision 1.17  2017/10/20 17:50:32  smirnovd
 * Squashed commit of the following:
 *
 *     StBTof: Remove outdated ClassImp macro
 *
 *     Prefer explicit namespace for std:: names in header files
 *
 *     Removed unnecessary specification of default std::allocator
 *
 * Frank signed-off
 *
 * Revision 1.16  2017/03/02 18:30:44  jeromel
 * Changes by jdb, nl - inData.open() of files on live disk TBF later
 *
 * Revision 1.10 2016/11/14 11:32:15  nluttrel
 * Simulated hits no longer undergo electronics corrections
 * If StVpdSimMaker used in chain, defaults to use Vpd start time
 *
 * Revision 1.15  2016/06/30 17:09:59  jdb
 * Fixed Several errors identified by Coverity
 *
 * Revision 1.14  2011/07/27 15:44:32  geurts
 * minor bug update: mProjVtxZ does not get initialized when mUseEventVertex is false, but is printed regardless.
 *
 * Revision 1.13  2011/05/09 14:32:10  geurts
 * use appropriate log level for debug messages
 *
 * Revision 1.12  2010/10/31 05:52:11  geurts
 * fixed module index range for read in loop for BOARD (TDIG) based calibration
 *
 * Revision 1.11  2010/10/30 05:20:50  geurts
 * Calibration Maker reads (file/dbase) in and applies cell-based, module-based, or board-based (TDIG) calibration parameters
 *
 * Revision 1.10  2010/05/27 21:41:14  geurts
 * Pick the default primary vertex (for mUseEventVertex). Additional cuts in selecting the vertex for tstart() have been removed.
 *
 * Revision 1.9  2010/05/25 22:09:18  geurts
 * improved database handling and reduced log output
 *
 * Revision 1.8  2010/05/12 22:46:21  geurts
 * Startless BTOF self-calibration method (Xin)
 *
 * Revision 1.7  2010/04/29 03:42:37  dongx
 * Remove ranking>0 cut in event vertex selection for start time calculation
 *
 * Revision 1.6  2010/04/09 21:26:51  geurts
 * Introduced "UseProjectedVertex" maker attribute to allow selection of the
 * standard event vertex or one determined by track extrapolation
 * (typically used in pp collisions).
 *
 * Revision 1.5  2010/04/03 15:43:58  dongx
 * Change the default to use event vertex for start position for Run10 AuAu
 *
 * Revision 1.4  2010/03/04 23:10:20  dongx
 * Added cleanup for PID variables in MuBTofPidTraits when processMuDst()
 *
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
#include "StEvent/StEvent.h"
#include "StEvent/StBTofCollection.h"
#include "StEvent/StBTofHit.h"
#include "StEvent/StBTofHeader.h"
#include "StEvent/StBTofPidTraits.h"
#include "StEvent/StEventTypes.h"
#include "Stypes.h"
#include "StThreeVectorD.hh"
#include "StHelix.hh"
#include "StEvent/StTrackGeometry.h"
#include "StEvent/StTrackPidTraits.h"
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
#include "StVpdCalibMaker/StVpdCalibMaker.h"
#include "StBTofUtil/StBTofSimResParams.h"
#include "StBTofUtil/StVpdSimConfig.h"

#include "TProfile.h"

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

const Float_t StBTofCalibMaker::BTHBLCHCNST = 8.; // Bethe-Bloch constant used for dE/dx 
                                                  // correction in start-time calculations 
                                                  // in pppAMode
const Float_t StBTofCalibMaker::DEDXTCORR[2] = {0.033, 0.013}; // correction to the vpd hit times 
							       // to match the corrected BTOF start time
							       // as a function of run year	

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
    mZCalibType = NOTSET;
    mTotCalibType = NOTSET;

    mSlewingCorr = kTRUE;
    mMuDstIn = kFALSE;
    mUseVpdStart = kTRUE;
    mForceTStartZero = false;
    isMcFlag = kFALSE;
    mFXTMode = kFALSE;

    mPPPAMode = kFALSE;
    mPPPAPionSel = kFALSE;
    mPPPAOutlierRej = kFALSE;
    mNSigmaTofMode = kFALSE;
    mRun15Slew = kFALSE;
    mPPPAModeHist = kFALSE;

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
    mProjVtxZ = -9999.;
    mVPDVtxZ = -9999.;
    mVPDHitPatternEast = 0;
    mVPDHitPatternWest = 0;
    mNEast = 0;
    mNWest = 0;
    mValidStartTime = kFALSE;
    mNTzero = 0;

    mNTzeroCan = 0;
    mTCanFirst = 99999.;
    mTCanLast = -99999.;

    mVpdEHits = 0;
    mVpdWHits = 0;
    mVpdEGoodHits = 0;
    mVpdWGoodHits = 0;
    mEarliestVpdEHit = 99999.;
    mEarliestVpdWHit = 99999.;
    mClosestVpdEHit = 99999.;
    mClosestVpdWHit = 99999.;
    mLatestVpdEHit = -99999.;
    mLatestVpdWHit = -99999.;
}

//____________________________________________________________________________
Int_t StBTofCalibMaker::Init()
{
    resetPars();
    resetVpd();

	if (IAttr("btofFXT")) mFXTMode = kTRUE; //True for FXT mode calib, default as false for collider mode calib

  if (IAttr("pppAMode")) {
      mPPPAMode = kTRUE;
      mRun15Slew = kTRUE;
      LOG_INFO << "pppAMode is on." << endm;
  }

  if (IAttr("setPPPAOutlierRej")) mPPPAOutlierRej = kTRUE;

  mUseEventVertex = ! IAttr("UseProjectedVertex");
  if (mUseEventVertex) {
      LOG_INFO << "Use event vertex position." << endm;
  } else {
      LOG_INFO << "Use projected vertex position." << endm;
  }

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

  if (mPPPAModeHist){
     bookPPPAHistograms();
     LOG_INFO << "pppAMode Histograms are booked!" << endm;
  }

  return kStOK;
}

//____________________________________________________________________________
Int_t StBTofCalibMaker::InitRun(int runnumber)
{
    // tof run configurations

    /// retrieve the BTOF calibration parameters (from database or file)
    Int_t val = initParameters(runnumber);
    if(val==kStOK) {
        mValidCalibPar = kTRUE;
        LOG_DEBUG << "Initialized valid calibration parameters." << endm;
    } else {
        mValidCalibPar = kFALSE;
        LOG_WARN << "No valid calibration parameters! " << endm;
    }

    /// Look for StVpdCalibMaker and decide on its setting (based on its dbase entry) to use VPD for TOF start-timing
    StVpdCalibMaker *vpdCalib = (StVpdCalibMaker *)GetMaker("vpdCalib");

    /// Get VPD and BTOF resolutions from database
    mVpdResConfig = new StVpdSimConfig;
    mVpdResConfig->loadVpdSimParams(); // do i really need this?
    mVpdRes = mVpdResConfig->getParams();
    mBTofRes = new StBTofSimResParams;
    mBTofRes->loadParams();


    if(vpdCalib) {
        mUseVpdStart = vpdCalib->useVpdStart();

        if (mUseVpdStart) {LOG_INFO << "Found VPD Calibration Maker: use vpd for start timing" << endm;}
        else         {LOG_INFO << "Found VPD Calibration Maker: vpd **NOT** used for start timing" << endm;}
    } else {
        mUseVpdStart = kFALSE;
        LOG_INFO << "NO VPD Calibration Maker found:  vpd **NOT** used for start timing" << endm;
    }

    // Apply fudge factor (FF) for dE/dx corrections (Runs 14 - 18)
    if(runnumber>=14350003 && runnumber<=17315001) {
        iYr = 0;
    }
    else {
        iYr = 1;
    }

    /// If no VPD is used then one should have selected to use the EventVertex from the TPC, and warn if not.
    if(!mUseVpdStart && !mUseEventVertex) {
        LOG_WARN << " Try to run calibration without VPD as the start time and DON'T use the event vertex! Wrong command! Exit!" << endm;
        return kStOK;
    }

    return kStOK;

}



//_____________________________________________________________________________
Int_t StBTofCalibMaker::initParameters(int runnumber)
{
    /// initialize the calibrations parameters from dbase
    /// read in and check the size

    if (mInitFromFile){
        LOG_INFO << "Initializing calibration parameters from files" << endm;
        ifstream inData;

        /// open file and read Time-over-Threshold calibration parameters
        LOG_INFO << " - ToT : " << mCalibFileTot << endm;
        inData.open(mCalibFileTot.c_str());
        unsigned int trayId, moduleId, cellId, boardId;	// Coverity - jdb
        int nbin;
        int iCalibType; //9=cell, 8=module, 7=board -- Inset the number at the top of the .dat
        inData >> iCalibType;

        // move enumeration to include file
        //enum calibtype {board=960, module=3840, cell=23040} calibType;
        mTotCalibType = calibtype(iCalibType);

        //    switch(CalibType){ //selecting the calibration parameter format
        switch(mTotCalibType) {
                /********************************************************/
                //cell based format
            case CELLCALIB:
                for(int i=0;i<mNTray;i++) {
                    for(int j=0;j<mNModule;j++) {
                        for(int l=0;l<mNCell;l++){
                            inData>>trayId>>moduleId>>cellId;
                            inData>>nbin;

                            // coverity - jdb
                            // protect agains overflow
                            if ( trayId - 1 >= mNTray || moduleId - 1 >= mNModule || cellId - 1 >= mNCell ){
                                LOG_ERROR << "OUT OF BOUNDS, trayId = " << trayId << ", moduleId = " << moduleId << ", cellId = " << cellId << endl;
                                continue;
                            }
                            if ( nbin >= mNBinMax || nbin < 0){
                                LOG_ERROR << "OUT OF BOUNDS, # of TOT bins = " << nbin << endl;
                                continue;
                            }

                            for( int k=0; k <= nbin; k++ ) {
                                inData >> mTofTotEdge[trayId-1][moduleId-1][cellId-1][k];
                            }

                            for(int k=0;k <= nbin;k++) {
                                inData >> mTofTotCorr[trayId-1][moduleId-1][cellId-1][k];

                                if(k%10==0&&Debug()) {
                                    LOG_DEBUG << " ijlk= " << i << " " << j << " " << l << " " << k << " tot " << mTofTotEdge[trayId-1][moduleId-1][cellId-1][k] << " corr " << mTofTotCorr[trayId-1][moduleId-1][cellId-1][k] << endm;
                                }
                            }
                        }//cell
                    }//module
                }//tray
                break;
                /********************************************************/
                // Module based format
            case MODULECALIB: //module based
                for(int i=0;i<mNTray;i++) {
                    for(int j=0;j<mNModule;j++) {
                        inData>>trayId>>moduleId;
                        cellId = 1;

                        inData>>nbin;

                        // coverity - jdb
                        // protect agains overflow
                        if ( trayId - 1 >= mNTray || moduleId - 1 >= mNModule || cellId - 1 >= mNCell ){
                            LOG_ERROR << "OUT OF BOUNDS, trayId = " << trayId << ", moduleId = " << moduleId << ", cellId = " << cellId << endl;
                            continue;
                        }
                        if ( nbin >= mNBinMax || nbin < 0){
                            LOG_ERROR << "OUT OF BOUNDS, # of TOT bins = " << nbin << endl;
                            continue;
                        }

                        for(int k=0;k<=nbin;k++){
                            inData>>mTofTotEdge[trayId-1][moduleId-1][cellId-1][k];

                            // fill all cells
                            for(int l=0; l < mNCell; l++){
                                mTofTotEdge[trayId-1][moduleId-1][l][k] = mTofTotEdge[trayId-1][moduleId-1][cellId-1][k];
                            }
                        }
                        for(int k=0;k<=nbin;k++) {
                            inData>>mTofTotCorr[trayId-1][moduleId-1][cellId-1][k];
                            // fill all cells
                            for(int l=0;l<mNCell;l++){
                                mTofTotCorr[trayId-1][moduleId-1][l][k] = mTofTotCorr[trayId-1][moduleId-1][cellId-1][k];
                            }

                            if(k%10==0&&Debug()) {
                                LOG_DEBUG << " ijlk= " << i << " " << j << " " << 0 << " " << k << " tot " << mTofTotEdge[trayId-1][moduleId-1][0][k] << " corr " << mTofTotCorr[trayId-1][moduleId-1][0][k] << endm;
                            }
                        }

                    }	//module
                }	//tray
                break;

                /********************************************************/
                // Board based format
            case BOARDCALIB: //tdig based
                for(int i=0;i<mNTray;i++) {
                    for(int j=0;j<mNTDIG;j++) {
                        inData>>trayId>>boardId;
                        // first in each board
                        moduleId = (boardId-1)*4;
                        cellId = 1;

                        inData>>nbin;

                        // coverity - jdb
                        // protect agains overflow
                        if ( trayId - 1 >= mNTray || moduleId >= mNModule || cellId - 1 >= mNCell || moduleId + 3 >= mNModule ){
                            LOG_ERROR << "OUT OF BOUNDS, trayId = " << trayId << ", moduleId = " << moduleId << ", cellId = " << cellId << endl;
                            continue;
                        }
                        if ( nbin >= mNBinMax || nbin < 0){
                            LOG_ERROR << "OUT OF BOUNDS, # of TOT bins = " << nbin << endl;
                            continue;
                        }

                        for(int k=0;k<=nbin;k++){
                            inData>>mTofTotEdge[trayId-1][moduleId][cellId-1][k];

                            for(int m=0;m<4;m++){
                                for(int l=0;l<mNCell;l++){
                                    mTofTotEdge[trayId-1][moduleId+m][l][k] = mTofTotEdge[trayId-1][moduleId][cellId-1][k];
                                }//cell
                            }//modules per board
                        }

                        for(int k=0;k<=nbin;k++) {

                            inData>>mTofTotCorr[trayId-1][moduleId][cellId-1][k];

                            for(int m=0;m<4;m++){
                                for(int l=0;l<mNCell;l++){
                                    mTofTotCorr[trayId-1][moduleId+m][l][k] = mTofTotCorr[trayId-1][moduleId][cellId-1][k];
                                }//cell
                            }//modules per board

                            if(k%10==0&&Debug()) {
                                LOG_DEBUG << " ijlk= " << i << " " << j*4 << " " << 0 << " " << k << " tot " << mTofTotEdge[trayId-1][moduleId][cellId][k] << " corr " << mTofTotCorr[trayId-1][moduleId][cellId][k] << endm;
                            }
                        } // k
                    }//board
                }//tray
                break;

            default:
                LOG_WARN << "Please check the top of your TOT.dat file for the Calibration format.  9=cell,8=module,7=tdig. Your's is : " << mTotCalibType << endl;

        }//switch

        inData.close();


        /******************************************************************************************************************************************************/
        /// open file and read local Zhit calibration parameters
        LOG_INFO << " - Zhit : " << mCalibFileZhit << endm;
        inData.open(mCalibFileZhit.c_str());

        inData>>iCalibType;
        mZCalibType = calibtype(iCalibType);

        switch(mZCalibType){ //selecting the calibration parameter format
            case CELLCALIB: //cell based format
                for(int i=0;i<mNTray;i++) {
                    for(int j=0;j<mNModule;j++) {
                        for(int l=0;l<mNCell;l++){
                            inData>>trayId>>moduleId>>cellId;
                            inData>>nbin;

                            // coverity - jdb
                            // protect agains overflow
                            if ( trayId - 1 >= mNTray || moduleId - 1 >= mNModule || cellId - 1 >= mNCell ){
                                LOG_ERROR << "OUT OF BOUNDS, trayId = " << trayId << ", moduleId = " << moduleId << ", cellId = " << cellId << endl;
                                continue;
                            }
                            if ( nbin >= mNBinMax || nbin < 0){
                                LOG_ERROR << "OUT OF BOUNDS, # of TOT bins = " << nbin << endl;
                                continue;
                            }

                            for(int k=0;k<=nbin;k++) inData>>mTofZEdge[trayId-1][moduleId-1][cellId-1][k];

                            for(int k=0;k<=nbin;k++) {
                                inData>>mTofZCorr[trayId-1][moduleId-1][cellId-1][k];
                                if(k%10==0&&Debug()) {
                                    LOG_DEBUG << " ijlk= " << i << " " << j << " " << l << " " << k << " zEdge " << mTofZEdge[trayId-1][moduleId-1][cellId-1][k] << " corr " << mTofZCorr[trayId-1][moduleId-1][cellId-1][k] << endm;
                                }
                            }

                        }//cell
                    }//module
                }//tray
                break;

            case MODULECALIB: //module based
                for(int i=0;i<mNTray;i++) {
                    for(int j=0;j<mNModule;j++) {
                        inData>>trayId>>moduleId;
                        cellId = 1;
                        inData>>nbin;

                        // coverity - jdb
                        // protect agains overflow
                        if ( trayId - 1 >= mNTray || moduleId - 1 >= mNModule || cellId - 1 >= mNCell ){
                            LOG_ERROR << "OUT OF BOUNDS, trayId = " << trayId << ", moduleId = " << moduleId << ", cellId = " << cellId << endl;
                            continue;
                        }
                        if ( nbin >= mNBinMax || nbin < 0){
                            LOG_ERROR << "OUT OF BOUNDS, # of TOT bins = " << nbin << endl;
                            continue;
                        }

                        for(int k=0;k<=nbin;k++){
                            inData>>mTofZEdge[trayId-1][moduleId-1][cellId-1][k];
                            for(int l=0;l<mNCell;l++){
                                mTofZEdge[trayId-1][moduleId-1][l][k]=mTofZEdge[trayId-1][moduleId-1][cellId-1][k];
                            }
                        }
                        for(int k=0;k<=nbin;k++) {
                            inData>>mTofZCorr[trayId-1][moduleId-1][cellId-1][k];
                            for(int l=0;l<mNCell;l++){
                                mTofZCorr[trayId-1][moduleId-1][l][k]=mTofZCorr[trayId-1][moduleId-1][cellId-1][k];
                            }
                            if(k%10==0&&Debug()) {
                                LOG_DEBUG << " ijlk= " << i << " " << j << " " << cellId-1 << " " << k << " zEdge " << mTofZEdge[trayId-1][moduleId-1][cellId][k] << " corr " << mTofZCorr[trayId-1][moduleId-1][cellId][k] << endm;
                            }
                        }
                    }//module
                }//tray
                break;

            case BOARDCALIB: //tdig based
                for(int i=0;i<mNTray;i++) {
                    for(int j=0;j<mNTDIG;j++) {
                        inData>>trayId>>boardId;
                        // first in each board
                        moduleId = (boardId-1)*4;
                        cellId = 1;

                        inData>>nbin;

                        // coverity - jdb
                        // protect agains overflow
                        if ( trayId - 1 >= mNTray || moduleId >= mNModule || cellId - 1 >= mNCell || moduleId + 3 >= mNModule ){
                            LOG_ERROR << "OUT OF BOUNDS, trayId = " << trayId << ", moduleId = " << moduleId << ", cellId = " << cellId << endl;
                            continue;
                        }
                        if ( nbin >= mNBinMax || nbin < 0){
                            LOG_ERROR << "OUT OF BOUNDS, # of TOT bins = " << nbin << endl;
                            continue;
                        }

                        for(int k=0;k<=nbin;k++){
                            inData>>mTofZEdge[trayId-1][moduleId][cellId-1][k];
                            for(int m=0;m<4;m++){

                                for(int l=0;l<mNCell;l++){
                                    mTofZEdge[trayId-1][moduleId+m][l][k] = mTofZEdge[trayId-1][moduleId][cellId-1][k];
                                }//cell
                            }//modules per board
                        } // k

                        for(int k=0;k<=nbin;k++) {
                            inData>>mTofZCorr[trayId-1][moduleId][cellId-1][k];
                            for(int m=0;m<4;m++){
                                for(int l=0;l<mNCell;l++){
                                    mTofZCorr[trayId-1][moduleId+m][l][k] = mTofZCorr[trayId-1][moduleId][cellId-1][k];
                                }//cell
                            }//modules per board

                            if(k%10==0&&Debug()) {
                                LOG_DEBUG << " ijlk= " << i << " " << moduleId << " " << cellId-1 << " " << k << " tot " << mTofZEdge[trayId-1][moduleId][cellId][k] << " corr " << mTofZCorr[trayId-1][moduleId][cellId][k] << endm;
                            }
                        }// k bin
                    }//board
                }//tray
                break;


            default:
                LOG_WARN << "Please check the top of your Zhit.dat file for the Calibration format.  9=cell,8=module,7=tdig. Your's is : " << mZCalibType << endl;
        }//switch
        inData.close();

        /******************************************************************************************************************************************************/
        /// open file and read T0 calibration parameters
        LOG_INFO << " - T0 : " << mCalibFileT0 << endm;
        inData.open(mCalibFileT0.c_str());
        //int moduleId, cellId;
        for(int i=0;i<mNTray;i++) {
            for(int j=0;j<mNModule;j++) {
                for(int k=0;k<mNCell;k++) {
                    inData>>trayId>>moduleId>>cellId;

                    // coverity - jdb
                    // protect agains overflow
                    if ( trayId - 1 >= mNTray || moduleId - 1 >= mNModule || cellId - 1 >= mNCell ){
                        LOG_ERROR << "OUT OF BOUNDS, trayId = " << trayId << ", moduleId = " << moduleId << ", cellId = " << cellId << endl;
                        continue;
                    }

                    inData>>mTofTZero[trayId-1][moduleId-1][cellId-1];
                    int index = i*mNModule*mNCell+j*mNCell+k;

                    if(index%100==0&&Debug()) {
                        LOG_DEBUG << " ijk= " << i << " " << j << " " << k << " t0 " << mTofTZero[trayId-1][moduleId-1][cellId-1] << endm;
                    }
                }
            }
        }
        inData.close();

        // end load from file
    } else {

        /// Get all calibration parameters from the database
        LOG_INFO << "Initializing calibration parameters from database" << endm;

        // read tofTotbCorr table
        TDataSet *mDbDataSet = GetDataBase("Calibrations/tof/tofTotbCorr");
        St_tofTotbCorr* tofTotCorr = static_cast<St_tofTotbCorr*>(mDbDataSet->Find("tofTotbCorr"));
        if(!tofTotCorr) {
            LOG_ERROR << "unable to get tof TotbCorr table parameters" << endm;
            //    assert(tofTotCorr);
            return kStErr;
        }
        tofTotbCorr_st* totCorr = static_cast<tofTotbCorr_st*>(tofTotCorr->GetArray());
        Int_t numRows = tofTotCorr->GetNRows();

        if(numRows!=mNTray*mNTDIG && numRows!=mNTray*mNModule*mNCell && numRows!=mNTray*mNModule) {
            LOG_WARN  << " Mis-matched number of rows in tofTotbCorr table! "  << numRows
            << " (exp:" << mNTray*mNTDIG << " or " << mNTray*mNModule*mNCell << " or "<< mNTray*mNModule << ")" << endm;
        }

        LOG_INFO << " Number of rows read in: " << numRows << " for ToT correction" << endm;

        // convert to calibtype
        mTotCalibType = calibtype(numRows);

        switch(mTotCalibType){
            case CELLCALIB://cell
                for (Int_t i=0;i<numRows;i++) {
                    short trayId = totCorr[i].trayId;
                    short moduleId = totCorr[i].moduleId;
                    short cellId = totCorr[i].cellId;
                    //short boardId = (moduleId-1)/4+1;      // used for trays

                    LOG_DEBUG << " tray " << trayId << " module " << moduleId << " cell " << cellId << endm;
                    for(Int_t j=0;j<mNBinMax;j++) {
                        if(trayId>0&&trayId<=mNTray&&moduleId>0&&moduleId<=mNModule&&cellId>0&&cellId<=mNCell){ // trays
                            mTofTotEdge[trayId-1][moduleId-1][cellId-1][j] = totCorr[i].tot[j];
                            mTofTotCorr[trayId-1][moduleId-1][cellId-1][j] = totCorr[i].corr[j];
                            if(Debug()&&j%10==0) { LOG_DEBUG << " j=" << j << " tot " << mTofTotEdge[trayId-1][moduleId-1][cellId-1][j] << " corr " << mTofTotCorr[trayId-1][moduleId-1][cellId-1][j] << endm; }
                        }
                    } // end j 0->mNBinMax
                } // end i 0->numRows
                break;

            case MODULECALIB://module
                for (Int_t i=0;i<numRows;i++) {
                    short trayId = totCorr[i].trayId;
                    short moduleId = totCorr[i].moduleId;
                    short cellId = totCorr[i].cellId;
                    //short boardId = (moduleId-1)/4+1;      // used for trays

                    LOG_DEBUG << " tray " << trayId << " module " << moduleId << " cell " << cellId << endm;
                    for(Int_t j=0;j<mNBinMax;j++) {
                        if(trayId>0&&trayId<=mNTray&&moduleId>0&&moduleId<=mNModule){ // trays
                            for(Int_t k=0;k<mNCell;k++){
                                mTofTotEdge[trayId-1][moduleId-1][cellId-1+k][j] = totCorr[i].tot[j];
                                mTofTotCorr[trayId-1][moduleId-1][cellId-1+k][j] = totCorr[i].corr[j];
                                if(Debug()&&j%10==0) { LOG_DEBUG << " j=" << j << " tot " << mTofTotEdge[trayId-1][moduleId-1][cellId-1+k][j] << " corr " << mTofTotCorr[trayId-1][moduleId-1][cellId-1+k][j] << endm; }
                            }//duplicating entries into each cell
                        }
                    } // end j 0->mNBinMax
                } // end i 0->numRows
                break;

            case BOARDCALIB://board
                for (Int_t i=0;i<numRows;i++) {
                    short trayId = totCorr[i].trayId;
                    short moduleId = totCorr[i].moduleId;
                    short cellId = totCorr[i].cellId;
                    short boardId = (moduleId-1)/4+1;      // used for trays

                    LOG_DEBUG << " tray " << trayId << " module " << moduleId << " cell " << cellId << endm;
                    for(Int_t j=0;j<mNBinMax;j++) {
                        if(trayId>0&&trayId<=mNTray&&boardId>0&&boardId<=mNTDIG){ // trays
                            for(Int_t k=0; k<4;k++){
                                for(Int_t l=0;l<mNCell;l++){
                                    mTofTotEdge[trayId-1][moduleId-1+k][cellId-1+l][j] = totCorr[i].tot[j];
                                    mTofTotCorr[trayId-1][moduleId-1+k][cellId-1+l][j] = totCorr[i].corr[j];
                                    if(Debug()&&j%10==0) { LOG_DEBUG << " j=" << j << " tot " << mTofTotEdge[trayId-1][moduleId-1+k][cellId-1+l][j] << " corr " << mTofTotCorr[trayId-1][moduleId-1+k][cellId-1+l][j] << endm; }
                                }//duplicating into cells
                            }//duplication into modules
                        }
                    } // end j 0->mNBinMax
                } // end i 0->numRows
                break;

            default:
                LOG_WARN << "Number of rows in tofTotbCorr table mis-matched. "<<endl;
        }//end of switch


        // read tofZbCorr table
        mDbDataSet = GetDataBase("Calibrations/tof/tofZbCorr");
        St_tofZbCorr* tofZCorr = static_cast<St_tofZbCorr*>(mDbDataSet->Find("tofZbCorr"));
        if(!tofZCorr) {
            LOG_ERROR << "unable to get tof ZbCorr table parameters" << endm;
            //    assert(tofZCorr);
            return kStErr;
        }
        tofZbCorr_st* zCorr = static_cast<tofZbCorr_st*>(tofZCorr->GetArray());
        numRows = tofZCorr->GetNRows();

        if(numRows!=mNTray*mNTDIG && numRows!=mNTray*mNModule*mNCell && numRows !=mNTray*mNModule) {
            LOG_WARN << " Mis-matched number of rows in tofZbCorr table! "  << numRows
            << " (exp:" << mNTray*mNTDIG << " or " << mNTray*mNModule*mNCell << " or "<< mNTray*mNModule << ")" << endm;
        }
        LOG_INFO << " Number of rows read in: " << numRows << " for Z correction" << endm;

        // convert to calibtype
        mZCalibType = calibtype(numRows);

        switch(mZCalibType){
            case CELLCALIB://cell
                for (Int_t i=0;i<numRows;i++) {
                    short trayId = totCorr[i].trayId;
                    short moduleId = totCorr[i].moduleId;
                    short cellId = totCorr[i].cellId;
                    //short boardId = (moduleId-1)/4+1;      // used for trays

                    LOG_DEBUG << " tray " << trayId << " module " << moduleId << " cell " << cellId << endm;
                    for(Int_t j=0;j<mNBinMax;j++) {
                        if(trayId>0&&trayId<=mNTray&&moduleId>0&&moduleId<=mNModule&&cellId>0&&cellId<=mNCell) {  // trays
                            mTofZEdge[trayId-1][moduleId-1][cellId-1][j] = zCorr[i].z[j];
                            mTofZCorr[trayId-1][moduleId-1][cellId-1][j] = zCorr[i].corr[j];
                            if(Debug()&&j%10==0) { LOG_DEBUG << " j=" << j << " tot " << mTofZEdge[trayId-1][moduleId-1][cellId-1][j] << " corr " << mTofZCorr[trayId-1][moduleId-1][cellId-1][j] << endm; }
                        }
                    } // end j 0->mNBinMax
                } // end i 0->numRows
                break;

            case MODULECALIB://module
                for (Int_t i=0;i<numRows;i++) {
                    short trayId = totCorr[i].trayId;
                    short moduleId = totCorr[i].moduleId;
                    short cellId = totCorr[i].cellId;
                    short boardId = (moduleId-1)/4+1;      // used for trays

                    LOG_DEBUG << " tray " << trayId << " module " << moduleId << " cell " << cellId << endm;
                    for(Int_t j=0;j<mNBinMax;j++) {
                        if(trayId>0&&trayId<=mNTray&&boardId>0&&moduleId<=mNModule) {  // trays
                            for(Int_t k=0;k<mNCell;k++){
                                mTofZEdge[trayId-1][moduleId-1][cellId-1+k][j] = zCorr[i].z[j];
                                mTofZCorr[trayId-1][moduleId-1][cellId-1+k][j] = zCorr[i].corr[j];
                                if(Debug()&&j%10==0) { LOG_DEBUG << " j=" << j << " tot " << mTofZEdge[trayId-1][moduleId-1][cellId-1+k][j] << " corr " << mTofZCorr[trayId-1][moduleId-1][cellId-1+k][j] << endm; }
                            }//duplicating info to all cells
                        }
                    } // end j 0->mNBinMax
                } // end i 0->numRows
                break;

            case BOARDCALIB://board
                for (Int_t i=0;i<numRows;i++) {
                    short trayId = totCorr[i].trayId;
                    short moduleId = totCorr[i].moduleId;
                    short cellId = totCorr[i].cellId;
                    short boardId = (moduleId-1)/4+1;      // used for trays

                    LOG_DEBUG << " tray " << trayId << " module " << moduleId << " cell " << cellId << endm;
                    for(Int_t j=0;j<mNBinMax;j++) {
                        if(trayId>0&&trayId<=mNTray&&boardId>0&&boardId<=mNTDIG) {  // trays
                            for(Int_t k=0;k<4;k++){
                                for(Int_t l=0;l<mNCell;l++){
                                    mTofZEdge[trayId-1][moduleId-1+k][cellId-1+l][j] = zCorr[i].z[j];
                                    mTofZCorr[trayId-1][moduleId-1+k][cellId-1+l][j] = zCorr[i].corr[j];
                                    if(Debug()&&j%10==0) { LOG_DEBUG << " j=" << j << " tot " << mTofZEdge[trayId-1][moduleId-1+k][cellId-1+l][j] << " corr " << mTofZCorr[trayId-1][moduleId-1+k][cellId-1+l][j] << endm; }
                                }//duplicating info to cell lvl
                            }//duplicating info to module lvl
                        }
                    } // end j 0->mNBinMax
                } // end i 0->numRows
                break;

            default:
                LOG_WARN << "Number of rows in tofZbCorr table mis-matched. "<<endl;
        }//switch
         // read tofTOffset table
        mDbDataSet = GetDataBase("Calibrations/tof/tofTOffset");
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
    TDataSet* dbDataSet = this->GetDataBase("Calibrations/rhic/vertexSeed");

    if (dbDataSet) {
        vertexSeed_st* vSeed = ((St_vertexSeed*) (dbDataSet->FindObject("vertexSeed")))->GetTable();

        x0 = vSeed->x0;
        y0 = vSeed->y0;
        dxdz = vSeed->dxdz;
        dydz = vSeed->dydz;
    }
    else {
        LOG_WARN << "No database for beamline (Calibrations/rhic/vertexSeed)" << endm;
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
        LOG_WARN << "Beam line must be tilted!" << endm;
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

    if (mBTofRes){delete mBTofRes; mBTofRes = 0;}
    if (mVpdResConfig) {delete mVpdResConfig;  mVpdResConfig = 0;}

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
    LOG_DEBUG << "StBTofCalibMaker::Maker: starting ..." << endm;
    if(!mValidCalibPar) {
        LOG_WARN << "No valid calibration parameters. Skip ... " << endm;
        return kStOK;
    }

    initEvent();
    resetVpd();
    if(mUseVpdStart) {
        loadVpdData();
    }

    if(!mMuDstIn) processStEvent();
    else          processMuDst();

    archiveVpdHitInfo();
    writeStartTime();

    return kStOK;
}

//_____________________________________________________________________________
void StBTofCalibMaker::processStEvent()
{

    // event selection  // no primary vertex required
    if( !mEvent ) {LOG_WARN << "No StEvent" << endm; return;}
    if (!mEvent->btofCollection()) {LOG_WARN << "No BTOFCollection" << endm; return;}
    if (!mEvent->btofCollection()->hitsPresent()) {LOG_WARN << "No hits present" << endm; return;}

    StBTofCollection *theTof = mEvent->btofCollection();
    StSPtrVecBTofHit &tofHits = theTof->tofHits();
    Int_t nhits = tofHits.size();
    LOG_INFO << " Fired TOF cells + upVPD tubes : " << nhits << endm;

    if(mUseVpdStart) {

        mEvtVtxZ = -9999.;
        mProjVtxZ = -9999.;
        float dcaRmin = 9999.;
        //    float rankHmax = -1.e9;

        if(mUseEventVertex) {
            //      ///
            //      /// select the vertex with highest positive rank within the VPDVtxZ cut range
            //      ///
            //       int nVtx = mEvent->numberOfPrimaryVertices();
            //       for(int i=0;i<nVtx;i++) {
            //         StPrimaryVertex *pVtx = mEvent->primaryVertex(i);
            //         if(!pVtx) continue;
            // //        if(pVtx->ranking()<0) continue;               //! select positive ranking vertex
            //         if(fabs(pVtx->position().z())>200.) continue;   //! within 200 cm
            //         if(fabs(pVtx->position().z()-mVPDVtxZ)>VZDIFFCUT) continue;  //! VPDVtxZ cut
            //         if(pVtx->ranking()<rankHmax) continue;
            //         mEvtVtxZ = pVtx->position().z();
            //         rankHmax = pVtx->ranking();
            //       }
            /// Select default primary vertex.
            //  (Future version should allow for non-default vertex selections)
            StPrimaryVertex *pVtx = mEvent->primaryVertex();

            if (pVtx){
                mEvtVtxZ = pVtx->position().z();
            }
            else {
                LOG_WARN << "No (default) primary vertex information for this (st-) event"  << endm;
            };

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

                    LOG_DEBUG<<" tofPos(x,y,z) = "<<tofPos.x()<<","<<tofPos.y()<<","<<tofPos.z()<<endm;
                    LOG_DEBUG<<"beamPos(x,y,z) = "<<beamPos.x()<<","<<beamPos.y()<<","<<beamPos.z()<<endm;
                    LOG_DEBUG<<"  dca  (x,y,z) = "<<dcatof.x()<<","<<dcatof.y()<<","<<dcatof.z()<<endm;
                    LOG_DEBUG<<" 2D dca        = "<<sqrt(pow(dcatof.x(),2)+pow(dcatof.y(),2))<<endm;
                    LOG_DEBUG<<" 2D signed dca = "<<theTrackGeometry->helix().geometricSignedDistance(beamPos.x(),beamPos.y())<<endm;

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

    } else {   // Don't use VPD as the start time

        StPrimaryVertex *pVtx = mEvent->primaryVertex();
        if(!pVtx) {
            LOG_WARN << " No primary vertex ... bye-bye" << endm;
            return;
        }
        mEvtVtxZ = pVtx->position().z();

        tstart_NoVpd(theTof, pVtx, &mTStart);
        
    }  // end if(mUseVpdStart)

    LOG_INFO << "primVz = " << mEvtVtxZ << " projVz = " << mProjVtxZ << "  vpdVz = " << mVPDVtxZ << endm;
    LOG_INFO << "Tstart = " << mTStart << " Tdiff = " << mTDiff  << "  NTzero = " << mNTzero << endm;
    LOG_INFO << "NWest = " << mNWest << " NEast = " << mNEast << " TdcSum West = " << mTSumWest << " East = " << mTSumEast << endm;


    if(mTStart<-1000.) {
        LOG_INFO << "No valid start time for this event. Skip ..." << endm;
        mValidStartTime = kFALSE;
        return;
    } else {
        mValidStartTime = kTRUE;
    }

    //---------------------------------------
    // BTof calibration
    //---------------------------------------

    // keep a few counters:
    int nohitfound(0), ismchit(0),trayoutofbounds(0),notrack(0),
        gnopidtraits(0),gtraitisfalse(0), pidtoffalse(0),calibfailed(0),
        pgnopidtraits(0), ptraitisfalse(0),tofisset(0), doPID(0);

    for(int i=0;i<nhits;i++) {

        StBTofHit *aHit = dynamic_cast<StBTofHit*>(tofHits[i]);
        if(!aHit){
            LOG_DEBUG << "No hit found in the BTof Calibration." << endm;
            nohitfound++; continue;
        }

        isMcFlag = kFALSE;       // Check to see if the hit is simulated.
        if ( aHit->qaTruth() == 1  ) {
            isMcFlag = kTRUE; ismchit++;
            LOG_DEBUG << "Simulated hit." << endm;
        }

        int trayId = aHit->tray();
        if(trayId<=0 || trayId>mNTray) {
            LOG_DEBUG << "Tray out of bounds." << endm;
            trayoutofbounds++; continue;
        }

        StGlobalTrack *gTrack = dynamic_cast<StGlobalTrack*>(aHit->associatedTrack());
        if(!gTrack) {
            LOG_DEBUG << " No associated Track with this hit." << endm;
            notrack++; continue;
        }
        else LOG_DEBUG << "Track found" << endm;

        const StPtrVecTrackPidTraits& theTofPidTraits = gTrack->pidTraits(kTofId);
        if(!theTofPidTraits.size()) {
            LOG_DEBUG << "Tof Pid Traits not populated." << endm;
            gnopidtraits++; continue;
        }

        StTrackPidTraits *theSelectedTrait = theTofPidTraits[theTofPidTraits.size()-1];
        if(!theSelectedTrait) {
            LOG_DEBUG << "The Selected Trait is false." << endm;
            gtraitisfalse++; continue;
        }

        StBTofPidTraits *pidTof = dynamic_cast<StBTofPidTraits *>(theSelectedTrait);
        if(!pidTof) {
            LOG_DEBUG << "PID Tof reports false" << endm;
            pidtoffalse++; continue;
        }

        double tot = aHit->tot(); // ns
        double tdc = aHit->leadingEdgeTime();
        double tof = tdc - mTStart;
        Double_t zhit = pidTof->zLocal();

        int moduleChan = (aHit->module()-1)*6 + (aHit->cell()-1);
        Double_t tofcorr = tof;
        if (!isMcFlag) {
            tofcorr = tofAllCorr(tof, tot, zhit, trayId, moduleChan);
            if(tofcorr<0.) {
                //LOG_WARN << " Calibration failed! ... " << endm;
                calibfailed++; continue;
            }
        }

        pidTof->setTimeOfFlight((Float_t)tofcorr);

        /// find the primary track if any
        StPrimaryTrack *pTrack = dynamic_cast<StPrimaryTrack *>(gTrack->node()->track(primary));
        StBTofPidTraits *ppidTof = 0;
        if(pTrack) {
            const StPtrVecTrackPidTraits& pTofPidTraits = pTrack->pidTraits(kTofId);
            if(!pTofPidTraits.size()) {
                LOG_DEBUG << "pTofPidTraits is false." << endm;
                pgnopidtraits++; continue;
            }

            StTrackPidTraits *pSelectedTrait = pTofPidTraits[pTofPidTraits.size()-1];
            if(!pSelectedTrait) {
                LOG_DEBUG << "pSelectedTrait is false." << endm;
                ptraitisfalse++; continue;
            }

            ppidTof = dynamic_cast<StBTofPidTraits *>(pSelectedTrait);

            if(ppidTof && mUseEventVertex) {
                ppidTof->setTimeOfFlight((Float_t)tofcorr);
                tofisset++; LOG_DEBUG << "Time of Flight set." << endm;
            }
        }

        /// PID calculation if the track is a "primary" track.
        Double_t L = -9999.;
        Double_t ptot = -9999.;
        Bool_t doPID = kFALSE;     //! switch indicating to calculate PID or not
        if(mUseEventVertex) {
            if(!pTrack) {
                LOG_DEBUG << " The associated track is not a primary one. Skip PID calculation! " << endm;
            } else {
                StTrackGeometry *theTrackGeometry = pTrack->geometry();
                const StVertex *thisVertex = pTrack->vertex();
                if(!thisVertex) {
                    LOG_DEBUG << " The associated track is not coming from any vertex. Skip PID calculation! " << endm;
                } else {
                    StThreeVectorF primPos = thisVertex->position();
                    L = tofPathLength(&primPos, &pidTof->position(), theTrackGeometry->helix().curvature());
                    ptot = pTrack->geometry()->momentum().mag();
                    doPID = kTRUE;
                    LOG_DEBUG << "Pathlength and ptot set." << endm;
                }
            }

        } else {

            StTrackGeometry *theTrackGeometry = gTrack->geometry();
            StThreeVectorD tofPos =  theTrackGeometry->helix().at(theTrackGeometry->helix().pathLengths(*mBeamHelix).first);
            StThreeVectorD dcatof = tofPos - mBeamHelix->at(theTrackGeometry->helix().pathLengths(*mBeamHelix).second);
            if(dcatof.perp()>DCARCUT) {
                LOG_DEBUG << " The projected position is far from beam line. Skip PID calculation! " << endm;
            } else if(fabs(tofPos.z()-mVPDVtxZ)>VZDIFFCUT) {
                LOG_DEBUG << " This track is not coming from the same VPD vertex! Skip PID calculation! " << endm;
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
        doPID++;

        Double_t beta = L/(tofcorr*(C_C_LIGHT/1.e9));

        Double_t b_e  = ptot/sqrt(ptot*ptot+M_ELECTRON*M_ELECTRON);
        Double_t b_pi = ptot/sqrt(ptot*ptot+M_PION_PLUS*M_PION_PLUS);
        Double_t b_k  = ptot/sqrt(ptot*ptot+M_KAON_PLUS*M_KAON_PLUS);
        Double_t b_p  = ptot/sqrt(ptot*ptot+M_PROTON*M_PROTON);

        float sigmae = -9999.;
        float sigmapi = -9999.;
        float sigmak = -9999.;
        float sigmap = -9999.;
//        float res = 0.013;  // 0.013 by default - 1/beta resolution
        float res = tofCellResolution(trayId, moduleChan);
	float res_c = res * (C_C_LIGHT/1.e9);

        if(fabs(res)>1.e-5) {
            sigmae = (Float_t)(L*(1./beta-1./b_e)/res_c);
            sigmapi = (Float_t)(L*(1./beta-1./b_pi)/res_c);
            sigmak = (Float_t)(L*(1./beta-1./b_k)/res_c);
            sigmap = (Float_t)(L*(1./beta-1./b_p)/res_c);
        }

        pidTof->setPathLength((Float_t)L);
        pidTof->setBeta((Float_t)beta);
        pidTof->setSigmaElectron(sigmae);
        pidTof->setSigmaPion(sigmapi);
        pidTof->setSigmaKaon(sigmak);
        pidTof->setSigmaProton(sigmap);

        LOG_DEBUG << " storing BTofPidTraits for the global track" << endm;

        if(mUseEventVertex) {

            if(ppidTof) {

                ppidTof->setPathLength((Float_t)L);
                ppidTof->setBeta((Float_t)beta);
                ppidTof->setSigmaElectron(sigmae);
                ppidTof->setSigmaPion(sigmapi);
                ppidTof->setSigmaKaon(sigmak);
                ppidTof->setSigmaProton(sigmap);

                LOG_DEBUG << " storing BTofPidTraits for the primary track" << endm;
            } // end if ppidTof
        }  // end if mUseEventVertex

    }  // end tof hits

    LOG_INFO << "nohitfound:"<< nohitfound << " ismchit:" <<ismchit << " trayoutofbounds:" << trayoutofbounds << " notrack:" << notrack << endm;
    LOG_INFO << " gnopidtraits:" <<  gnopidtraits <<" gtraitisfalse:" <<  gtraitisfalse << " pidtoffalse:"<< pidtoffalse <<" calibfailed:"<<  calibfailed << endm;
    LOG_INFO << " pgnopidtraits:"<< pgnopidtraits << " ptraitisfalse:" <<  ptraitisfalse << " tofisset:"<<  tofisset << " doPID:" <<  doPID << endm;

    return;
}

//_____________________________________________________________________________
void StBTofCalibMaker::processMuDst()
{
    if(!mMuDst) {
        LOG_WARN << " No MuDst ... bye-bye" << endm;
        return;
    }

    cleanCalibMuDst();

    Int_t nhits = mMuDst->numberOfBTofHit();
    LOG_INFO << " Fired TOF cells + upVPD tubes : " << nhits << endm;

    if(mUseVpdStart) {

        mEvtVtxZ  = -9999.;
        mProjVtxZ = -9999.;
        float dcaRmin = 9999.;
        //    float rankHmax = -1.e9;

        if(mUseEventVertex) {
            //      ///
            //      /// select the vertex with highest positive rank within the VPDVtxZ cut range
            //      ///
            //       int nVtx = mMuDst->numberOfPrimaryVertices();
            //       for(int i=0;i<nVtx;i++) {
            //         StMuPrimaryVertex* pVtx = mMuDst->primaryVertex(i);
            //         if(!pVtx) continue;
            // //        if(pVtx->ranking()<0) continue;               //! select positive ranking vertex
            //         if(fabs(pVtx->position().z())>200.) continue;   //! within 200 cm
            //         if(fabs(pVtx->position().z()-mVPDVtxZ)>VZDIFFCUT) continue;  //! VPDVtxZ cut
            //         if(pVtx->ranking()<rankHmax) continue;
            //         mEvtVtxZ = pVtx->position().z();
            //         rankHmax = pVtx->ranking();
            //       }
            /// Select default primary vertex.
            //  (Future version should allow for non-default vertex selections)
            StMuPrimaryVertex* pVtx = mMuDst->primaryVertex();
            if (pVtx){
                mEvtVtxZ = pVtx->position().z();
            }
            else {
                LOG_WARN << "No (default) primary vertex information for this (mudst) event"  << endm;
            }

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

        } // end if(mUseEventVertex)
    } else { // don't use vpd as the start time

        StMuPrimaryVertex *pVtx = mMuDst->primaryVertex();
        if(!pVtx) {
            LOG_WARN << " No primary vertex ... bye-bye" << endm;
            return;
        }
        mEvtVtxZ = pVtx->position().z();

        tstart_NoVpd(mMuDst, pVtx, &mTStart);
    }

    LOG_INFO << "primVz = " << mEvtVtxZ << " projVz = " << mProjVtxZ << "  vpdVz = " << mVPDVtxZ  << endm;
    LOG_INFO << "Tstart = " << mTStart << " Tdiff = " << mTDiff << "  NTzero = " << mNTzero << endm;
    LOG_INFO << "NWest = " << mNWest << " NEast = " << mNEast << " TdcSum West = " << mTSumWest << " East = " << mTSumEast << endm;


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

    // keep a few counters:
    int nohitfound(0), ismchit(0),trayoutofbounds(0),notrack(0),
        gnopidtraits(0),gtraitisfalse(0), pidtoffalse(0),calibfailed(0),
        pgnopidtraits(0), ptraitisfalse(0),tofisset(0), doPID(0);

    for(int i=0;i<nhits;i++) {
        StMuBTofHit *aHit = (StMuBTofHit*)mMuDst->btofHit(i);
        if(!aHit) {nohitfound++; continue;}
        int trayId = aHit->tray();
        if(trayId<=0 || trayId>mNTray) {trayoutofbounds++;continue;}

        StMuTrack *gTrack = aHit->globalTrack();
        if(!gTrack) {
            LOG_DEBUG << " No associated Track with this hit." << endm;
            notrack++; continue;
        }

        isMcFlag = kFALSE;       // Check to see if the hit is simulated.
        if ( aHit->qaTruth() == 1  ) {
            isMcFlag = kTRUE; ismchit++;
            LOG_DEBUG << "Simulated hit." << endm;
        }

        StMuBTofPidTraits pidTof = gTrack->btofPidTraits();

        double tot = aHit->tot(); // ns
        double tdc = aHit->leadingEdgeTime();
        while(tdc>TMAX) tdc -= TMAX;
        double tof = tdc - mTStart;
        Double_t zhit = pidTof.zLocal();

        int moduleChan = (aHit->module()-1)*6 + (aHit->cell()-1);

        Double_t tofcorr = tof;
        if (!isMcFlag) {
            tofcorr = tofAllCorr(tof, tot, zhit, trayId, moduleChan);
            if(tofcorr<0.) {
                //LOG_WARN << " Calibration failed! ... " << endm;
                calibfailed++; continue;
            }
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
                LOG_DEBUG << " The associated track is not a primary one. Skip PID calculation! " << endm;
            } else {
                int iv = pTrack->vertexIndex();
                StMuPrimaryVertex *thisVertex = mMuDst->primaryVertex(iv);
                if(!thisVertex) {
                    LOG_DEBUG << " The associated track is not coming from any vertex. Skip PID calculation! " << endm;
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
                LOG_DEBUG << " The projected position is far from beam line. Skip PID calculation! " << endm;
            } else if(fabs(tofPos.z()-mVPDVtxZ)>VZDIFFCUT) {
                LOG_DEBUG << " This track is not coming from the same VPD vertex! Skip PID calculation! " << endm;
            } else {
                L = tofPathLength(&tofPos, &pidTof.position(), gHelix.curvature());
                ptot = gTrack->momentum().mag();
                doPID = kTRUE;
            }
        }

        if(doPID) {
            doPID++;
            Double_t beta = L/(tofcorr*(C_C_LIGHT/1.e9));

            Double_t b_e  = ptot/sqrt(ptot*ptot+M_ELECTRON*M_ELECTRON);
            Double_t b_pi = ptot/sqrt(ptot*ptot+M_PION_PLUS*M_PION_PLUS);
            Double_t b_k  = ptot/sqrt(ptot*ptot+M_KAON_PLUS*M_KAON_PLUS);
            Double_t b_p  = ptot/sqrt(ptot*ptot+M_PROTON*M_PROTON);

            float sigmae = -9999.;
            float sigmapi = -9999.;
            float sigmak = -9999.;
            float sigmap = -9999.;
//            float res = 0.013;  // 0.013 by default - 1/beta resolution
            float res = tofCellResolution(trayId, moduleChan);
	    float res_c = res * (C_C_LIGHT/1.e9);

           if(fabs(res)>1.e-5) {
                sigmae = (Float_t)(L*(1./beta-1./b_e)/res_c);
                sigmapi = (Float_t)(L*(1./beta-1./b_pi)/res_c);
                sigmak = (Float_t)(L*(1./beta-1./b_k)/res_c);
                sigmap = (Float_t)(L*(1./beta-1./b_p)/res_c);
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
        LOG_DEBUG << " storing BTofPidTraits for the global track" << endm;

        if(mUseEventVertex && pTrack) {
            pTrack->setBTofPidTraits(ppidTof);
            LOG_DEBUG << " storing BTofPidTraits for the primary track" << endm;
        }
    }  // end tof hits

    LOG_INFO << "nohitfound:"<< nohitfound << " ismchit:" <<ismchit << " trayoutofbounds:" << trayoutofbounds << " notrack:" << notrack << endm;
    LOG_INFO << " gnopidtraits:" <<  gnopidtraits <<" gtraitisfalse:" <<  gtraitisfalse << " pidtoffalse:"<< pidtoffalse <<" calibfailed:"<<  calibfailed << endm;
    LOG_INFO << " pgnopidtraits:"<< pgnopidtraits << " ptraitisfalse:" <<  ptraitisfalse << " tofisset:"<<  tofisset << " doPID:" <<  doPID << endm;

    return;
}


//_____________________________________________________________________________
void StBTofCalibMaker::cleanCalibMuDst()
{
    if(!mMuDst) return;

    Int_t nPrimary = mMuDst->numberOfPrimaryTracks();
    Int_t nGlobal = mMuDst->numberOfGlobalTracks();
    for(int i=0;i<nPrimary;i++) {
        StMuTrack *pTrack = (StMuTrack *)mMuDst->primaryTracks(i);
        if(!pTrack) continue;
        StMuBTofPidTraits pid = pTrack->btofPidTraits();
        cleanCalib(pid);
        pTrack->setBTofPidTraits(pid);
    }
    for(int i=0;i<nGlobal;i++) {
        StMuTrack *gTrack = (StMuTrack *)mMuDst->globalTracks(i);
        if(!gTrack) continue;
        StMuBTofPidTraits pid = gTrack->btofPidTraits();
        cleanCalib(pid);
        gTrack->setBTofPidTraits(pid);
    }
    return;
}

//_____________________________________________________________________________
void StBTofCalibMaker::cleanCalib(StMuBTofPidTraits& pid)
{
    pid.setTimeOfFlight(-999.);
    pid.setPathLength(-999.);
    pid.setBeta(-999.);
    pid.setSigmaElectron(-999.);
    pid.setSigmaPion(-999.);
    pid.setSigmaKaon(-999.);
    pid.setSigmaProton(-999.);
    pid.setProbElectron(-999.);
    pid.setProbPion(-999.);
    pid.setProbKaon(-999.);
    pid.setProbProton(-999.);
    return;
}

//_____________________________________________________________________________
void StBTofCalibMaker::initEvent()
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

    return;
}

//_____________________________________________________________________________
void StBTofCalibMaker::loadVpdData()
{
    if(!mBTofHeader) return;

    mTSumWest = 0;
    mTSumEast = 0;
    mTSumWestSigma = 0;
    mTSumEastSigma = 0;
    mVPDHitPatternWest = mBTofHeader->vpdHitPattern(west);
    mVPDHitPatternEast = mBTofHeader->vpdHitPattern(east);
    mNWest = mBTofHeader->numberOfVpdHits(west);
    mNEast = mBTofHeader->numberOfVpdHits(east);
    mVPDVtxZ = mBTofHeader->vpdVz();

    for(int i=0;i<mNVPD;i++) {
        mVPDLeTime[i] = mBTofHeader->vpdTime(west, i+1);
        if(mVPDLeTime[i]>0.) {
          mTSumWest += mVPDLeTime[i];
 //fg add here the mTSumEastSigma based on what tubes were used.
        }
        if(Debug()) {
            LOG_DEBUG << " loading VPD West tubeId = " << i+1 << " time = " << mVPDLeTime[i] << endm;
        }
    }

    for(int i=0;i<mNVPD;i++) {
        mVPDLeTime[i+mNVPD] = mBTofHeader->vpdTime(east, i+1);
        if(mVPDLeTime[i+mNVPD]>0.) mTSumEast += mVPDLeTime[i+mNVPD];
        if(Debug()) {
            LOG_DEBUG << " loading VPD East tubeId = " << i+1 << " time = " << mVPDLeTime[i+mNVPD] << endm;
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
        mBTofHeader->setNTzero(mNTzero);

        mBTofHeader->setNTzeroCan(mNTzeroCan);
        mBTofHeader->setTCanFirst(mTCanFirst);
        mBTofHeader->setTCanLast(mTCanLast);

        mBTofHeader->setVpdEHits(mVpdEHits);
        mBTofHeader->setVpdWHits(mVpdWHits);
        mBTofHeader->setVpdEGoodHits(mVpdEGoodHits);
        mBTofHeader->setVpdWGoodHits(mVpdWGoodHits);
        mBTofHeader->setEarliestVpdEHit(mEarliestVpdEHit);
        mBTofHeader->setEarliestVpdWHit(mEarliestVpdWHit);
        mBTofHeader->setClosestVpdEHit(mClosestVpdEHit);
        mBTofHeader->setClosestVpdWHit(mClosestVpdWHit);
        mBTofHeader->setLatestVpdEHit(mLatestVpdEHit);
        mBTofHeader->setLatestVpdWHit(mLatestVpdWHit);
    }

    return;
}

//_____________________________________________________________________________
Double_t StBTofCalibMaker::tofAllCorr(const Double_t tof, const Double_t tot, const Double_t z, const Int_t iTray, const Int_t iModuleChan)
{
    int tray = iTray;
    int module = iModuleChan/6 + 1;
    int cell = iModuleChan%6 + 1;
    //int board = iModuleChan/24 + 1;
    LOG_DEBUG << "\nStBTofCalibMaker::btofAllCorr: BTof calibrating...\n"
    << "\tDoing Calibration in BTOF Tray " << tray << " Module " << module << " Cell " << cell
			 << "\n\tinput tof = " << tof
			 << "  TOT = " << tot << "  Zlocal = " << z << endm;

    Double_t tofcorr = tof;

    tofcorr -= mTofTZero[tray-1][module-1][cell-1];

    LOG_DEBUG << "T0 correction: "<<mTofTZero[tray-1][module-1][cell-1]<<endm;

    if(mSlewingCorr) {
        int iTotBin = -1;
        for(int i=0;i<mNBinMax-1;i++) {
            if(tot>=mTofTotEdge[tray-1][module-1][cell-1][i] && tot<mTofTotEdge[tray-1][module-1][cell-1][i+1]) {
                iTotBin = i;
                break;
            }
        }
        if(iTotBin>=0&&iTotBin<mNBinMax) {
            double x1 = mTofTotEdge[tray-1][module-1][cell-1][iTotBin];
            double x2 = mTofTotEdge[tray-1][module-1][cell-1][iTotBin+1];
            double y1 = mTofTotCorr[tray-1][module-1][cell-1][iTotBin];
            double y2 = mTofTotCorr[tray-1][module-1][cell-1][iTotBin+1];
            double dcorr = y1 + (tot-x1)*(y2-y1)/(x2-x1);
            LOG_DEBUG << "TOT correction: "<<dcorr<<endm;

            tofcorr -= dcorr;
        } else {
            LOG_DEBUG << " TOT out of range! EXIT! " << endm;
            return -9999.;
        }

        int iZBin = -1;
        for(int i=0;i<mNBinMax-1;i++) {
            if(z>=mTofZEdge[tray-1][module-1][cell-1][i] && z<mTofZEdge[tray-1][module-1][cell-1][i+1]) {
                iZBin = i;
                break;
            }
        }
        if(iZBin>=0&&iZBin<mNBinMax) {
            double x1 = mTofZEdge[tray-1][module-1][cell-1][iZBin];
            double x2 = mTofZEdge[tray-1][module-1][cell-1][iZBin+1];
            double y1 = mTofZCorr[tray-1][module-1][cell-1][iZBin];
            double y2 = mTofZCorr[tray-1][module-1][cell-1][iZBin+1];
            double dcorr = y1 + (z-x1)*(y2-y1)/(x2-x1);

            tofcorr -= dcorr;
            LOG_DEBUG << "zHit correction: "<<dcorr<<endm;

        } else {
            LOG_DEBUG << " Z out of range! EXIT! " << endm;
            return -9999.;
        }

    }

    LOG_DEBUG << "  Corrected tof: tofcorr = " << tofcorr << endm;

// Special handling of Run15 slewing correction (Bassam)
    const double SLEWCORR = 0.19; // slewing correction constant for Run 15 in ns
    if(mRun15Slew && tot<15.) {
        if(tot<13.) {
            tofcorr -= SLEWCORR;
        }
        else {
            tofcorr -= (SLEWCORR/4.)*(15.-tot)*(15.-tot);
        }
    }

    if(mRun15Slew && tot>19. && tot<26.) {
        double delta = (22.5-tot)/3.5;
        tofcorr += .110*(1.0-delta*delta);
    }

    if (mRun15Slew) LOG_DEBUG << "  Corrected tof (Run15Slew): tofcorr = " << tofcorr << endm;


    return tofcorr;
}

//_____________________________________________________________________________
void StBTofCalibMaker::tstart(const Double_t vz, Double_t *tstart, Double_t *tdiff)
{
    if ( mForceTStartZero ){
        *tstart = 0;
        return;
    }
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
void StBTofCalibMaker::tstart_NoVpd(const StBTofCollection *btofColl, const StPrimaryVertex *pVtx, Double_t *tstart)
{
    if ( mForceTStartZero ){
        *tstart = 0;
        return;
    }

    *tstart = -9999.;
    if(!btofColl) return;

    const StSPtrVecBTofHit &tofHits = btofColl->tofHits();
    Int_t nCan = 0;
    Double_t tSum = 0.;
    Double_t t0[5000];
    Double_t tCanFirst = 99999.;
    Double_t tCanLast = -99999.;

    memset(t0, 0., sizeof(t0));
    for(size_t i=0;i<tofHits.size();i++) {
        StBTofHit *aHit = dynamic_cast<StBTofHit*>(tofHits[i]);
        if(!aHit) continue;

        isMcFlag = kFALSE;       // Check to see if the hit is simulated.
        if ( aHit->qaTruth() == 1  ) {
            isMcFlag = kTRUE;
        }

        int trayId = aHit->tray();
        if(trayId>0&&trayId<=mNTray) {
            StGlobalTrack *gTrack = dynamic_cast<StGlobalTrack*>(aHit->associatedTrack());
            if(!gTrack) continue;
            StPrimaryTrack *pTrack = dynamic_cast<StPrimaryTrack*>(gTrack->node()->track(primary));
            if(!pTrack) continue;
            if(pTrack->vertex() != pVtx) continue;
            StThreeVectorF mom = pTrack->geometry()->momentum();
            double ptot = mom.mag();
            int q = pTrack->geometry()->charge();

            static StTpcDedxPidAlgorithm PidAlgorithm;
            static StPionPlus* Pion = StPionPlus::instance();
            static StProton* Proton = StProton::instance();
            static StKaonPlus* Kaon = StKaonPlus::instance();
            static StElectron* Electron = StElectron::instance();

            const StParticleDefinition* pd = pTrack->pidTraits(PidAlgorithm);
            double nSigPi = -999.;
            double nSigP  = -999.;
            double nSigKaon = -999.;
            double nSigElectron = -999.;
            float nHitsDedx = 0.;

            if(pd) {
                nSigPi = PidAlgorithm.numberOfSigma(Pion);
                nSigP = PidAlgorithm.numberOfSigma(Proton);
                nSigKaon = PidAlgorithm.numberOfSigma(Kaon);
                nSigElectron = PidAlgorithm.numberOfSigma(Electron);

                nHitsDedx = PidAlgorithm.traits()->numberOfPoints();
            }

            float nHitsFit = pTrack->fitTraits().numberOfFitPoints();
            float nHitsPoss = pTrack->numberOfPossiblePoints();

            double pT = mom.perp();
            double eta = mom.pseudoRapidity();
	          double zVtx = pVtx->position().z();

            const StPtrVecTrackPidTraits& theTofPidTraits = pTrack->pidTraits(kTofId);
            if(!theTofPidTraits.size()) continue;

            StTrackPidTraits *theSelectedTrait = theTofPidTraits[theTofPidTraits.size()-1];
            if(!theSelectedTrait) continue;

            StBTofPidTraits *pidTof = dynamic_cast<StBTofPidTraits *>(theSelectedTrait);
            if(!pidTof) continue;

            double tot = aHit->tot(); // ns
            double tof = aHit->leadingEdgeTime();
            double zhit = pidTof->zLocal();

            int moduleChan = (aHit->module()-1)*6 + (aHit->cell()-1);
            Double_t tofcorr = tof;
            if (!isMcFlag) {
              tofcorr = tofAllCorr(tof, tot, zhit, trayId, moduleChan);
            }
            if(tofcorr<0.) continue;

            StThreeVectorF primPos = pVtx->position();
            StPhysicalHelixD helix = pTrack->geometry()->helix();
            double L = tofPathLength(&primPos, &pidTof->position(), helix.curvature());
            double tofPi = L*sqrt(M_PION_PLUS*M_PION_PLUS+ptot*ptot)/(ptot*(C_C_LIGHT/1.e9));
            double tofP  = L*sqrt(M_PROTON*M_PROTON+ptot*ptot)/(ptot*(C_C_LIGHT/1.e9));

            // use lose cut for low energies to improve the efficiency
			if(mFXTMode) //use both Pion and Proton for FXT mode
			{
				if(( (q<0 && ptot>0.2) || (q>0 && ptot>0.2 && ptot<1.0)) && fabs(nSigPi)<2.0)//pi selection
				{
					tSum     += tofcorr - tofPi;
					t0[nCan]  = tofcorr - tofPi;
					nCan++;
				}
				else if(q>0 && fabs(nSigP)<2.0)//proton selection
				{
					tSum      += tofcorr - tofP;
					t0[nCan]   = tofcorr - tofP;
					nCan++;
				}
			}
      else if(mPPPAMode || mPPPAPionSel)
      {
          // Obtaining gDCA
          const StDcaGeometry *dcaGeometry = gTrack->dcaGeometry();
          if(!dcaGeometry) continue;
          Double_t vtx[3] = {primPos[0], primPos[1], primPos[2]};
          THelixTrack thelix = dcaGeometry->thelix();
          thelix.Move(thelix.Path(vtx)); // move along the helix to the point that is closest to the vertex
          const Double_t *pos = thelix.Pos();
          StThreeVectorF gDca = StThreeVectorF(pos[0],pos[1],pos[2]) - pVtx->position();
          double gDcaMag = gDca.mag();
          if(tot > 25.) {
              continue;
          }
          if(ptot>0.2 && ptot<0.7 && fabs(nSigPi)< 2.0
                      && nHitsFit>20
                      && nHitsFit>0.51*nHitsPoss
                      && nHitsDedx>0.5*nHitsFit
                      && fabs(gDcaMag)<2.0
                      && nSigKaon<-3.0
                      && nSigElectron<-3.0
                      && pT>0.18
                    )
          {
          double FDGCNST = fudgeFactor(eta, zVtx);
          double nvrsBeta = sqrt(1.+(M_PION_PLUS/ptot)*(M_PION_PLUS/ptot));
          double bthBlchFrml = nvrsBeta*nvrsBeta*(BTHBLCHCNST+2.*log(ptot/M_PION_PLUS));
          double dTofPi = FDGCNST*L*L*((M_PION_PLUS/ptot)*(M_PION_PLUS/ptot)*(1./ptot)*bthBlchFrml);
          Double_t tofCorrPi = tofcorr - tofPi - dTofPi;
          if(tofCorrPi < tCanFirst) {
            tCanFirst = tofCorrPi;
          }
          if(tofCorrPi > tCanLast) {
            tCanLast = tofCorrPi;
          }
          tSum     += tofCorrPi;
          t0[nCan]  = tofCorrPi;
          nCan++;
        }
      }
			else//If not FXT, Only use Pion
			{
				if(ptot>0.2 && ptot<0.6 && fabs(nSigPi)< 2.0)
				{
					tSum     += tofcorr - tofPi;
					t0[nCan]  = tofcorr - tofPi;
					nCan++;
				}
			}

        }
    }

    mNTzeroCan = nCan;
    mTCanFirst = tCanFirst;
    mTCanLast  = tCanLast;

    if(nCan<=0) {
        *tstart = -9999.;
        return;
    }

    Int_t nTzero = nCan;

if(mPPPAMode || mPPPAOutlierRej)
{
    const float outlierCut = 2.5 * 0.086; // numberOfSigmas * time resolution (sigma) of a BTof pad
    if(nCan>1) {
        while(nTzero>2) {
            int iMax = 0;
            double DtiMax = 0.0;
            for(int i=0;i<nTzero;i++) {
                double tdiff = fabs(t0[i] - (tSum-t0[i])/(nTzero-1));
                if(tdiff>DtiMax) {
                    iMax = i;
                    DtiMax = tdiff;
                } // end of if(tdiff>DtiMax)
            } // end of for(int i=0;i<nTzero;i++)
            if(DtiMax>sqrt(1.0+1.0/(nTzero-1))*outlierCut) {
                tSum -= t0[iMax];
                t0[iMax] = t0[nTzero-1];
                nTzero--;
            } // end of if(DtiMax>1.0)
            else {
                break;
            }
        } // end of while(nTzero>2)
        if(nTzero==2){
            if(fabs(t0[0]-t0[1])>sqrt(2.0)*outlierCut) {
                float vpdTDiffCut = 0.25; // Acceptance window between an individual t0 vlaue
                                          // and VPD start time based on the VPD resolution
                double vpdStartTime = 0.;
                Double_t vz = pVtx->position().z();
                vpdTStartForBTofComparison(vz, &vpdStartTime);
                if(fabs(t0[0]-vpdStartTime)<vpdTDiffCut && fabs(t0[1]-vpdStartTime)<vpdTDiffCut) {
                    if(fabs(t0[0]-vpdStartTime) < fabs(t0[1]-vpdStartTime)) {
                        tSum -= t0[1];
                        nTzero--;
                    }
                    else {
                        tSum -= t0[0];
                        t0[0] = t0[1];
                        nTzero--;
                    }
                } // end of if(fabs(t0[0]-vpdTStartTime)<0.25 && fabs(t0[1]-vpdTStartTime)<0.25)
                else if(fabs(t0[0]-vpdStartTime)<vpdTDiffCut) {
                    tSum -= t0[1];
                    nTzero--;
                }
                else if(fabs(fabs(t0[1]-vpdStartTime)<vpdTDiffCut)) {
                    tSum -= t0[0];
                    t0[0] = t0[1];
                    nTzero--;
                }
                else {
                    nTzero = 0;
                }
            }
        } // end of if(nTzero==2)
    } // end of if(nCan>1)
}
else
{
    if(nCan>1) { // remove hits too far from others
        for(int i=0;i<nCan;i++) {
            double tdiff = t0[i] - (tSum-t0[i])/(nTzero-1);
            if(fabs(tdiff)>5.0) {
                tSum -= t0[i];
                nTzero--;
            }
        }
    }
  }

    mNTzero = nTzero;

    *tstart = nTzero>0 ? tSum / nTzero : -9999.;

    return;
}

//_____________________________________________________________________________
void StBTofCalibMaker::tstart_NoVpd(const StMuDst *muDst, const StMuPrimaryVertex *pVtx, Double_t *tstart)
{
    if ( mForceTStartZero ){
        *tstart = 0;
        return;
    }
    *tstart = -9999.;
    if(!muDst) return;

    Int_t nBTofHits = muDst->numberOfBTofHit();
    Int_t nCan = 0;
    Double_t tSum = 0.;
    Double_t t0[5000];

    Double_t tCanFirst = 99999.;
    Double_t tCanLast = -99999.;
    Double_t gDCA[5000];
    Double_t ptotSave[5000];
    Double_t etaSave[5000];

    memset(t0, 0., sizeof(t0));
    int calibfailed(0); // keep counter
    for(int i=0;i<nBTofHits;i++) {
        StMuBTofHit *aHit = (StMuBTofHit*)muDst->btofHit(i);
        if(!aHit) {
            continue;
        }

        isMcFlag = kFALSE;       // Check to see if the hit is simulated.
        if ( aHit->qaTruth() == 1  ) {
            isMcFlag = kTRUE;
        }

        int trayId = aHit->tray();
        if(trayId>0&&trayId<=mNTray) {
            StMuTrack *gTrack = aHit->globalTrack();
            if(!gTrack) continue;
            StMuTrack *pTrack = aHit->primaryTrack();
            if(!pTrack) continue;
            StMuPrimaryVertex *aVtx = muDst->primaryVertex(pTrack->vertexIndex());
            if(aVtx != pVtx) continue;
            StThreeVectorF mom = pTrack->momentum();
            double ptot = mom.mag();
            int q = pTrack->charge();

            double nSigPi = pTrack->nSigmaPion();
            double nSigP = pTrack->nSigmaProton();

            float  nHitsFit = pTrack->nHitsFit();
            float  nHitsPoss = pTrack->nHitsPoss();
            float  nHitsDedx = pTrack->nHitsDedx();

            double pT = pTrack->pt();
            double nSigKaon = pTrack->nSigmaKaon();
            double nSigElectron = pTrack->nSigmaElectron();
            double gDcaMag = pTrack->dcaGlobal().mag();
	          double eta = pTrack->eta();
            double zVtx = pVtx->position().z();

            StMuBTofPidTraits pidTof = pTrack->btofPidTraits();

            double tot = aHit->tot(); // ns
            double tof = aHit->leadingEdgeTime();
            double zhit = pidTof.zLocal();

            int moduleChan = (aHit->module()-1)*6 + (aHit->cell()-1);

            Double_t tofcorr = tof;
            if (!isMcFlag) {
                tofcorr = tofAllCorr(tof, tot, zhit, trayId, moduleChan);
                if(tofcorr<0.) {
//                    LOG_WARN << " Calibration failed! ... " << endm;
                    calibfailed++; continue;
                }
            }
            if(tofcorr<0.) continue;

            StThreeVectorF primPos = pVtx->position();
            StPhysicalHelixD helix = pTrack->helix();
            double L = tofPathLength(&primPos, &pidTof.position(), helix.curvature());
            double tofPi = L*sqrt(M_PION_PLUS*M_PION_PLUS+ptot*ptot)/(ptot*(C_C_LIGHT/1.e9));
            double tofP  = L*sqrt(M_PROTON*M_PROTON+ptot*ptot)/(ptot*(C_C_LIGHT/1.e9));

            // For low energies, lose cut to improve the efficiency in peripheral collisions - resolution should be not a big issue
			if(mFXTMode) //use both Pion and Proton for FXT mode
			{
				if(( (q<0 && ptot>0.2) || (q>0 && ptot>0.2 && ptot<1.0)) && fabs(nSigPi)<2.0)//pi selection
				{
					tSum     += tofcorr - tofPi;
					t0[nCan]  = tofcorr - tofPi;
					nCan++;
				}
				else if(q>0 && fabs(nSigP)<2.0)//proton selection
				{
					tSum      += tofcorr - tofP;
					t0[nCan]   = tofcorr - tofP;
					nCan++;
				}
			}
      // calculating the total time, individual time,
      // and multiplicity of candidate particles for the pppA mode
      else if(mPPPAMode || mPPPAPionSel)
      {
        if(tot>25.) {
          continue;
        }
        if(ptot>0.2 && ptot<0.7 && fabs(nSigPi)< 2.0
                    && nHitsFit>20
                    && nHitsFit>0.51*nHitsPoss
                    && nHitsDedx>0.5*nHitsFit
                    && fabs(gDcaMag)<2.0
                    && nSigKaon<-3.0
                    && nSigElectron<-3.0
                    && pT>0.18)
        {
          double FDGCNST = fudgeFactor(eta, zVtx);
          double nvrsBeta = sqrt(1.+(M_PION_PLUS/ptot)*(M_PION_PLUS/ptot));
          double bthBlchFrml = nvrsBeta*nvrsBeta*(BTHBLCHCNST+2.*log(ptot/M_PION_PLUS));
          double dTofPi = FDGCNST*L*L*((M_PION_PLUS/ptot)*(M_PION_PLUS/ptot)*(1./ptot)*bthBlchFrml);
          Double_t tofCorrPi = tofcorr - tofPi - dTofPi;
          if(tofCorrPi < tCanFirst) {
            tCanFirst = tofCorrPi;
          }
          if(tofCorrPi > tCanLast) {
            tCanLast = tofCorrPi;
          }
          tSum     += tofCorrPi;
          t0[nCan]  = tofCorrPi;
          gDCA[nCan] = gDcaMag;
          ptotSave[nCan] = ptot;
          etaSave[nCan] = eta;
          nCan++;
        }
      }
			else//If not FXT, Only use Pion
			{
				if(ptot>0.2 && ptot<0.6 && fabs(nSigPi)< 2.0)
				{
					tSum     += tofcorr - tofPi;
					t0[nCan]  = tofcorr - tofPi;
					nCan++;
				}
			}

		}//tray
	}//tof hit

  if (calibfailed) LOG_WARN << "tstart_NoVpd calibrations failures: " << calibfailed << " (out of "<< nBTofHits << "BTOF hits)"<< endm;

  mNTzeroCan = nCan;
  mTCanFirst = tCanFirst;
  mTCanLast  = tCanLast;


	if(nCan<=0) {
        *tstart = -9999.;
        return;
      }

    Int_t nTzero = nCan;

    // Adjusting the default outlier rejection to accomodate for PPPAMode
    if(mPPPAMode || mPPPAOutlierRej)
    {
        if(mPPPAModeHist) {
            if(nCan==1) { // filling global DCA histograms before outlier rejection
                hGDcaArray[0]->Fill(fabs(gDCA[0]));
            }
            else if(nCan==2) {
                for(int i=0;i<nCan;i++) {
                    hGDcaArray[1]->Fill(fabs(gDCA[i]));
                }
            }
            else if(nCan>2) {
                for(int i=0;i<nCan;i++) {
                    hGDcaArray[2]->Fill(fabs(gDCA[i]));
                }
            }
        }
        const float outlierCut = 2.5 * 0.086; // numberOfSigmas * time resolution (sigma) of a BTof pad
        if(nCan>1) {
            while(nTzero>2) {
                int iMax = 0;
                double DtiMax = 0.0;
                for(int i=0;i<nTzero;i++) {
                    double tdiff = fabs(t0[i] - (tSum-t0[i])/(nTzero-1));
                    if(tdiff>DtiMax) {
                        iMax = i;
                        DtiMax = tdiff;
                    } // end of if(tdiff>DtiMax)
                } // end of for(int i=0;i<nTzero;i++)
                if(DtiMax>sqrt(1.0+1.0/(nTzero-1))*outlierCut) {
                    tSum -= t0[iMax];
                    t0[iMax] = t0[nTzero-1];
                    gDCA[iMax] = gDCA[nTzero-1];
        ptotSave[iMax] = ptotSave[nTzero-1];
        etaSave[iMax] = etaSave[nTzero-1];
                    nTzero--;
                } // end of if(DtiMax>1.0)
                else {
                    break;
                }
            } // end of while(nTzero>2)
            if(nTzero==2){
                if(fabs(t0[0]-t0[1])>sqrt(2.0)*outlierCut) {
                    float vpdTDiffCut = 0.25; // Acceptance window between an individual t0 vlaue
                                              //and VPD start time based on the VPD resolution
                    double vpdStartTime = 0.;
                    Double_t vz = pVtx->position().z();
                    vpdTStartForBTofComparison(vz, &vpdStartTime);
                    if(fabs(t0[0]-vpdStartTime)<vpdTDiffCut && fabs(t0[1]-vpdStartTime)<vpdTDiffCut) {
                        if(fabs(t0[0]-vpdStartTime) < fabs(t0[1]-vpdStartTime)) {
                            tSum -= t0[1];
                            nTzero--;
                        }
                        else {
                            tSum -= t0[0];
                            t0[0] = t0[1];
                            gDCA[0] = gDCA[1];
          ptotSave[0] = ptotSave[1];
          etaSave[0] = etaSave[1];
                            nTzero--;
                        }
                    } // end of if(fabs(t0[0]-vpdTStartTime)<0.25 && fabs(t0[1]-vpdTStartTime)<0.25)
                    else if(fabs(t0[0]-vpdStartTime)<vpdTDiffCut) {
                        tSum -= t0[1];
                        nTzero--;
                    }
                    else if(fabs(fabs(t0[1]-vpdStartTime)<vpdTDiffCut)) {
                        tSum -= t0[0];
                        t0[0] = t0[1];
                        gDCA[0] = gDCA[1];
            ptotSave[0] = ptotSave[1];
            etaSave[0] = etaSave[1];
                        nTzero--;
                    }
                    else {
                        nTzero = 0;
                    }
                }
            } // end of if(nTzero==2)
        } // end of if(nCan>1)
    }
    else
    {
      if(nCan>1) { // remove hits too far from others
        for(int i=0;i<nCan;i++) {
            double tdiff = t0[i] - (tSum-t0[i])/(nTzero-1);
            if(fabs(tdiff)>5.0) {
                tSum -= t0[i];
                nTzero--;
            }
        }
    }
  }

    mNTzero = nTzero;

    *tstart = nTzero>0 ? tSum / nTzero : -9999.;

    if(mPPPAModeHist) {
       if(nTzero>1) {
           for(int i=0;i<nTzero; i++) {
               double Dti = t0[i] - (tSum-t0[i])/(nTzero-1);
               if(nTzero>10) {
                   hDtiArray[9]->Fill(Dti);
               }
               else {
                   hDtiArray[nTzero-2]->Fill(Dti);
               }
               //if(nTzero>4 && fabs(pVtx->position().perp())<1. && fabs(pVtx->position().z())<75.) {
               //    hDtiVsPtot[0]->Fill(ptotSave[i], Dti);
               //}
	       //if(nTzero>4 && fabs(pVtx->position().perp())<1. && fabs(pVtx->position().z())>75.) {
               //    hDtiVsPtot[1]->Fill(ptotSave[i], Dti);
               //}
	       double rVtx = pVtx->position().perp();
               double zVtx = pVtx->position().z();
               if(nTzero>4 && (fabs(rVtx)<1. || (fabs(zVtx-200.)<3. && fabs(rVtx)<3.))) {
	           fillDtiVsPtotProfiles(etaSave[i], zVtx, ptotSave[i], Dti);
               }
           } // end of for(int i=0;i<mNTzero; i++)
       } // end of if(mNTzero>1)
       if(nCan==2) { // filling global DCA histograms after outlier rejection
           for(int i=0;i<nTzero;i++) {
               hGDcaArray[3]->Fill(fabs(gDCA[i]));
           }
       }
       else if(nCan>2) {
           for(int i=0;i<nTzero;i++) {
               hGDcaArray[4]->Fill(fabs(gDCA[i]));
           }
       }
    } // end of if(mPPPAModeHist)


    return;
}

//_____________________________________________________________________________
void StBTofCalibMaker::vpdTStartForBTofComparison(Double_t vz, Double_t *vpdStartTime)
{
    if(!mBTofHeader){
        *vpdStartTime = -9999.0;
        return;
    }
    double vzNs = vz/mC_Light;
    //int nVpdWCandHits = 0;
    //int nVpdECandHits = 0;
    Double_t vpdHitTStart[38]; // Array will contain the start time of West + East VPD hit start time
    //Bool_t isEastHit[38]; // true if the hit came from VPDE false if hit came from VPDW

    int nVpdTotCandHits = 0; // Total number of West + East VPD candidate hits in a given event
    double tSum = 0.;

    for(int i=0; i<19; i++) {
        double vpdTime = mBTofHeader->vpdTime(west,i+1); // Here, the index has to be between 1-19
							 //since the btofHeader function defines tubeId as tubeId-1.
        if(vpdTime<1.e-4) {
           continue;
        }
        vpdHitTStart[nVpdTotCandHits] = vpdTime + vzNs - DEDXTCORR[iYr];
        //isEastHit[nVpdTotCandHits] = kFALSE;
        tSum += vpdHitTStart[nVpdTotCandHits];
        //nVpdWCandHits++;
        nVpdTotCandHits++;
    }

    for(int i=0; i<19; i++) {
        double vpdTime = mBTofHeader->vpdTime(east,i+1); // Here, the index has to be between 1-19
							 //since the btofHeader function defines tubeId as tubeId-1.
        if(vpdTime<1.e-4) {
           continue;
        }
        vpdHitTStart[nVpdTotCandHits] = vpdTime - vzNs - DEDXTCORR[iYr];
        //isEastHit[nVpdTotCandHits] = kTRUE;
        tSum += vpdHitTStart[nVpdTotCandHits];
        //nVpdECandHits++;
        nVpdTotCandHits++;
    }

    if(nVpdTotCandHits<1) {
        //*mNVpdWHits = 0;
        //*mNVpdEHits = 0;
        //*mNVpdTzero = 0;
        *vpdStartTime = -9999.0;
        return;
    }

    Int_t nVpdTzero = nVpdTotCandHits;

    const float vpdTStartOulierCut = 2.5*0.130; // numberOfSigmas * time resolution of a vpd phototube

    if(nVpdTotCandHits>1) {
        while(nVpdTzero>2) {
            int iVpdMax = 0;
            double vpdDtiMax = 0.;
            for(int i=0; i<nVpdTzero; i++) {
                double vpdTdiff = fabs(vpdHitTStart[i] - (tSum-vpdHitTStart[i])/(nVpdTzero-1));
                if(vpdTdiff>vpdDtiMax) {
                    iVpdMax = i;
                    vpdDtiMax = vpdTdiff;
                } // end of if(vpdTdiff>vpdDtiMax)
            } // end of for(int i=0; i<nVpdTzero; i++)
            if(vpdDtiMax > sqrt(1.0+1.0/(nVpdTzero-1))*vpdTStartOulierCut) {
                tSum -= vpdHitTStart[iVpdMax];
                vpdHitTStart[iVpdMax] = vpdHitTStart[nVpdTzero-1];
                //isEastHit[iVpdMax] = isEastHit[nVpdTzero-1];
                nVpdTzero--;
            } // end of if(vpdDitMax>sqrt(1.0+1.0/(nVpdTzero-1))*vpdTStartOulierCut)
            else{
                break;
            }
        } // end of while(nVpdTzero)
        if(nVpdTzero==2) {
            if(fabs(vpdHitTStart[0]-vpdHitTStart[1]) > sqrt(2.0)*vpdTStartOulierCut) {
                nVpdTzero = 0;
            }
        } // end of if(nTzero==2)
    } // end of nVpdTzero

    *vpdStartTime = nVpdTzero>0 ? tSum/nVpdTzero : -9999.;
}

//_____________________________________________________________________________
double StBTofCalibMaker::fudgeFactor(double eta, double zVtx)
{
    double ffArr[2][22] = {
			    {6.1e-8, 6.95e-8, 7.9e-8, 11.2e-8, 14.7e-8, 13.2e-8, 10.7e-8, 8.8e-8, 8.21e-8, 8.31e-8, 9.01e-8, 8.31e-8, 7.2e-8, 9.e-8, 8.8e-8, 12.e-8, 8.9e-8, 6.e-8, 5.1e-8, 4.7e-8, 5.9e-8, 3.3e-8},
                            {4.0e-8, 3.25e-8, 4.45e-8, 4.75e-8, 4.1e-8, 3.6e-8, 3.8e-8, 3.7e-8, 3.6e-8, 3.5e-8, 3.8e-8, 3.7e-8, 3.5e-8, 3.8e-8, 3.3e-8, 4.0e-8, 4.4e-8, 4.5e-8, 2.7e-8, 3.5e-8, 4.83e-8, 3.3e-8}
                          };
    double FF = 0.;
    if(eta<=0.) {
        if(zVtx<-75.) {
    	FF = ffArr[iYr][0];
        }
        else if(zVtx<-50.) {
            FF = ffArr[iYr][2];
        }
        else if(zVtx<-30.) {
            FF = ffArr[iYr][4];
        }
        else if(zVtx<-10.) {
            FF = ffArr[iYr][6];
        }
        else if(zVtx<0.) {
            FF = ffArr[iYr][8];
        }
        else if(zVtx<10.) {
            FF = ffArr[iYr][10];
        }
        else if(zVtx<30.) {
            FF = ffArr[iYr][12];
        }
        else if(zVtx<50.) {
            FF = ffArr[iYr][14];
        }
        else if(zVtx<75.) {
            FF = ffArr[iYr][16];
        }
        else if(zVtx<196.){
            FF = ffArr[iYr][18];
        }
        else {
            if (eta < -1.) {
                FF = ffArr[iYr][20];
            }
            else {
                FF = ffArr[iYr][21];
            }
        }
    } // end of if(eta<=0.)
    else {
        if(zVtx<-75.) {
    	FF = ffArr[iYr][1];
        }
        else if(zVtx<-50.) {
            FF = ffArr[iYr][3];
        }
        else if(zVtx<-30.) {
            FF = ffArr[iYr][5];
        }
        else if(zVtx<-10.) {
            FF = ffArr[iYr][7];
        }
        else if(zVtx<0.) {
            FF = ffArr[iYr][9];
        }
        else if(zVtx<10.) {
            FF = ffArr[iYr][11];
        }
        else if(zVtx<30.) {
            FF = ffArr[iYr][13];
        }
        else if(zVtx<50.) {
            FF = ffArr[iYr][15];
        }
        else if(zVtx<75.) {
            FF = ffArr[iYr][17];
        }
        else if(zVtx<196.){
            FF = ffArr[iYr][19];
        }
        else {
            FF = ffArr[iYr][21];
        }
    }
    return FF;
}

//_____________________________________________________________________________
void StBTofCalibMaker::fillDtiVsPtotProfiles(double eta, double zVtx, double ptot, double Dti)
{
    int etaIdx;
    int zIdx;

    if(zVtx<-75.) {
	zIdx = 0;
    }
    else if(zVtx<-50.) {
        zIdx = 1;
    }
    else if(zVtx<-30.) {
        zIdx = 2;
    }
    else if(zVtx<-10.) {
        zIdx = 3;
    }
    else if(zVtx<0.) {
        zIdx = 4;
    }
    else if(zVtx<10.) {
        zIdx = 5;
    }
    else if(zVtx<30.) {
        zIdx = 6;
    }
    else if(zVtx<50.) {
        zIdx = 7;
    }
    else if(zVtx<75.) {
        zIdx = 8;
    }
    else if(fabs(zVtx-200.)<3.){
        zIdx = 10;
    }
    else {
        zIdx = 9;
    }

    if(eta<=0.) {
        if(zIdx==10) {
           if(eta<-1.) {
               etaIdx = 0;
           }
           else {
               etaIdx = 1;
           }
        }
	else {
           etaIdx = 0;
        }
    }
    else {
        etaIdx = 1;
    }

    hDtiVsPtot[zIdx][etaIdx]->Fill(ptot, Dti);
}

//_____________________________________________________________________________
void StBTofCalibMaker::archiveVpdHitInfo()
{
    if(!mBTofHeader) return;
    if(mEvtVtxZ<-999.) return;
 
    const double SIGMA = 0.13; // ns
    float dist = 2.5; 

    double vzNs = mEvtVtxZ/mC_Light; // mC_Light is c in cm/ns

    int wHitCan = 0;
    int eHitCan = 0;
    int goodWHits = 0;
    int goodEHits = 0;

    double wEarlHit = 99999.;
    double eEarlHit = 99999.;

    double wClosestHit = 99999.;
    double wClosestDiff = 99999.;
    double eClosestHit = 99999.;
    double eClosestDiff = 99999.;

    double wLateHit = -99999.;
    double eLateHit = -99999.;

    for(int i=0; i<mNVPD; i++) {
        double wHitTime = mBTofHeader->vpdTime(west,i+1);
        if(wHitTime<1.e-4) {
           continue;
        }
        wHitCan++;
        double wHitTimeCorr = wHitTime + vzNs - DEDXTCORR[iYr];
        if(wHitTimeCorr<wEarlHit) {
            wEarlHit = wHitTimeCorr;
        }
        if(wHitTimeCorr>wLateHit) {
            wLateHit = wHitTimeCorr;
        }
        if(mTStart>-1000.) {
            double wHitTDiff = wHitTimeCorr - mTStart;
            if(fabs(wHitTDiff)<dist*SIGMA) {
                goodWHits++;
            }
            if(fabs(wHitTDiff)<fabs(wClosestDiff)) {
                wClosestDiff = wHitTDiff;
                wClosestHit = wHitTimeCorr;
            }
        } // End of if(mTStart>-1000.)
    } // end of for(int i=0; i<MAXVPD; i++)

    for(int i=0; i<mNVPD; i++) {
        double eHitTime = mBTofHeader->vpdTime(east,i+1);
        if(eHitTime<1.e-4) {
           continue;
        }
        eHitCan++;
        double eHitTimeCorr = eHitTime - vzNs - DEDXTCORR[iYr];
        if(eHitTimeCorr<eEarlHit) {
            eEarlHit = eHitTimeCorr;
        }
        if(eHitTimeCorr>eLateHit) {
            eLateHit = eHitTimeCorr;
        }
        if(mTStart>-1000.) {
            double eHitTDiff = eHitTimeCorr - mTStart;
            if(fabs(eHitTDiff)<dist*SIGMA) {
                goodEHits++;
            }
            if(fabs(eHitTDiff)<fabs(eClosestDiff)) {
                eClosestDiff = eHitTDiff;
                eClosestHit = eHitTimeCorr;
            }
        } // End of if(mTStart>-1000.)
    } // End of for(int i=0; i<MAXVPD; i++)

    mEarliestVpdEHit = eEarlHit;
    mClosestVpdEHit = eClosestHit;
    mLatestVpdEHit = eLateHit;

    mEarliestVpdWHit = wEarlHit;
    mClosestVpdWHit = wClosestHit;
    mLatestVpdWHit = wLateHit;

    mVpdEGoodHits = goodEHits;
    mVpdWGoodHits = goodWHits;
    mVpdEHits = eHitCan;
    mVpdWHits = wHitCan;

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

//_____________________________________________________________________________
void StBTofCalibMaker::bookPPPAHistograms()
{
    for(int i=0;i<10;i++) {
            ostringstream histogramName;
            ostringstream histogramTitle;

            if(i == 9)
            {
                histogramName << "DtiDistFornTzeroGreater10";
                histogramTitle << "Dti Dist. For nTzero > 10";
            }
            else
            {
                histogramName << "DtiDistFornTzero" << i+2;
                histogramTitle << "Dti Dist. for nTzero " << i+2;
            }
            hDtiArray[i] = new TH1D(histogramName.str().c_str(), histogramTitle.str().c_str(), 200, -1.0, 1.0);
    } // end of for(int i=0;i<5;i++)

    for(int i=0;i<5;i++) {
        ostringstream histogramName;
        ostringstream histogramTitle;

        if(i<2) {
            histogramName << "gDCABeforeOutlierRejectionForNCan" << i+1;
            histogramTitle << "|gDCA| Before Outlier Rejection For nCan = " << i+1;
        }
        else if(i==2) {
            histogramName << "gDCABeforeOutlierRejectionForNCanGreater2";
            histogramTitle << "|gDCA| Before Outlier Rejection For nCan > 2";
        }
        else if(i==3) {
            histogramName << "gDCAAfterOutlierRejectionForNCan2";
            histogramTitle << "|gDCA| After Outlier Rejection For nCan = 2";
        }
        else if(i==4) {
            histogramName << "gDCAAfterOutlierRejectionForNCanGreater2";
            histogramTitle << "|gDCA| After Outlier Rejection Ror nCan > 2";
        }
        hGDcaArray[i] = new TH1D(histogramName.str().c_str(), histogramTitle.str().c_str(), 200, 0., 2.5);
    }

    string zVertLocation[11] = {"z_{vtx}#LT-75", "-75#leqz_{vtx}#LT-50", "-50#leqz_{vtx}#LT-30", "-30#leqz_{vtx}#LT-10", "-10#leqz_{vtx}#LT0", "0#leqz_{vtx}#LT10", "10#leqz_{vtx}#LT30", "30#leqz_{vtx}#LT50", "50#leqz_{vtx}#LT75", "z_{vtx}#geq75 (excluding |z_{vtx}-200|<3)", "|z_{vtx}-200|<3"};
    string trkEta[4] = {"#eta#leq0","#eta>0", "#eta#LT-1", "#eta#geq-1"};
    for(int i=0; i<11; i++) {
        for(int j=0; j<2; j++) {
	    ostringstream histogramName;
	    ostringstream histogramTitle;

            if(i==10 && j==0) {
                histogramName << "hDtiVsPtot_" << i << j;
                histogramTitle << "Dti vs. pTot  " << zVertLocation[i] << "  " << trkEta[2];
            }
            else if(i==10 && j==1) {
                histogramName << "hDtiVsPtot_" << i << j;
                histogramTitle << "Dti vs. pTot  " << zVertLocation[i] << "  " << trkEta[3];
            }
            else {
                histogramName << "hDtiVsPtot_" << i << j;
                histogramTitle << "Dti vs. pTot  " << zVertLocation[i] << "  " << trkEta[j];
            }

            hDtiVsPtot[i][j] = new TProfile(histogramName.str().c_str(), histogramTitle.str().c_str(), 50, 0.2, 0.7, -1.0, 1.0);
	}
    }
}

//_____________________________________________________________________________
void StBTofCalibMaker::writePPPAHistograms()
{
    // Output file
    TFile *thePPPAHistoFile = new TFile(mPPPAModeHistoFileName.c_str(), "RECREATE");

    thePPPAHistoFile->cd();

    if(mPPPAModeHist) {
        for(int i=0;i<10;i++) {
            hDtiArray[i]->Write();
        }
	for(int i=0;i<5;i++) {
            hGDcaArray[i]->Write();
        }
	for(int i=0; i<11; i++) {
	    for(int j=0; j<2; j++) {
	        hDtiVsPtot[i][j]->Write();
	    }
	}
    }

    return;
}

//_____________________________________________________________________________
float StBTofCalibMaker::tofCellResolution(const Int_t itray, const Int_t iModuleChan)
{

 float resolution(0.013); // 0.013 by default - 1/beta resolution
 if (itray<0){return resolution;}

 int module = iModuleChan/6 + 1;
 int cell   = iModuleChan%6 + 1;
 // mBTofRes::timeres_tof() reports in picoseconds
 float stop_resolution  = mBTofRes->timeres_tof(itray, module, cell)/1000.;

float start_resolution(0);
 if (mUseVpdStart){

   // For VPD timing determine the VPD starttime by combing the resolutions of
   //   tray == 122 (east)
   //   mSimParams[singleHit.tubeId-1+19].singleTubeRes
   //   tray 121 (west)
   //   mSimParams[singleHit.tubeId-1].singleTubeRes
   //
   // needs to be implemented

 }
 else {
   // combine an average BTOF resolution based on NT0
   // more sophisticated: figure out what BTOF cells actually went into the NT0 count.

   // mBTofRes::timeres_tof() reports in picoseconds
   start_resolution = mBTofRes->average_timeres_tof()/sqrt(mNTzero)/1000.;
 }

 resolution = sqrt(stop_resolution*stop_resolution + start_resolution*start_resolution);

return resolution;
}
