/*******************************************************************
 *
 * $Id: StBTofCalibMaker.h,v 1.15 2021/05/29 23:57:08 geurts Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: Tof Calibration Maker to do the calibration for VPD
 *              (start timing) , TOF tray
 *              store into StBTofPidTraits
 *
 *****************************************************************
 *
 * $Log: StBTofCalibMaker.h,v $
 * Revision 1.15  2021/05/29 23:57:08  geurts
 * Updates to improve pp and pA handling - by Bassam Aboona (TAMU)
 *
 * Revision 1.14  2021/01/27 04:06:25  geurts
 * Introducing meaningful nTofSigma calculations in VPDstartless mode.
 *
 * Revision 1.13  2020/04/10 20:41:38  zye20
 * Xin add more pions and add protons for T0s in the FXT mode
 *
 * Revision 1.12  2019/04/23 05:49:57  jdb
 * Added function to allow forcing 0 starttime for totally startless BTOF usage in UPC
 *
 * Revision 1.11  2017/10/20 17:50:32  smirnovd
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
 * Revision 1.10  2017/03/02 18:30:44  jeromel
 * Changes by jdb, nl - inData.open() of files on live disk TBF later
 *
 * Revision 1.10 2016/11/14 11:32:15  nluttrel
 * Simulated hits no longer undergo electronics corrections
 * If StVpdSimMaker used in chain, defaults to use Vpd start time
 *
 * Revision 1.9  2016/06/30 17:09:56  jdb
 * Fixed Several errors identified by Coverity
 *
 * Revision 1.8  2014/08/06 11:42:53  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.7  2010/10/31 05:51:06  geurts
 * fixed array dimensions to accomodate cell-based calibrations
 *
 * Revision 1.6  2010/10/30 05:20:52  geurts
 * Calibration Maker reads (file/dbase) in and applies cell-based, module-based, or board-based (TDIG) calibration parameters
 *
 * Revision 1.5  2010/05/12 22:46:21  geurts
 * Startless BTOF self-calibration method (Xin)
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
#ifndef STBTOFCALIMAKER_H
#define STBTOFCALIMAKER_H

#include "phys_constants.h"
#include "TMath.h"
#include "StMaker.h"

//#include "TProfile.h"

#include <string>
#include <vector>

class StEvent;
class StPrimaryVertex;
class StBTofGeometry;
class StBTofCollection;
class StBTofHeader;
class StBTofHitCollection;
class StBTofPidTraits;
class StMuDst;
class StMuPrimaryVertex;
class StMuBTofPidTraits;
class StVpdSimConfig;
class StBTofSimResParams;

class TProfile;

#include "StPhysicalHelixD.hh"
#include "StBTofUtil/StVpdSimConfig.h"

typedef std::vector<Int_t>  IntVec;
typedef std::vector<Double_t>  DoubleVec;

class StBTofCalibMaker : public StMaker{
public:

  /// Default constructor
  StBTofCalibMaker(const char* name="btofCalib");
  /// Destructor
  virtual ~StBTofCalibMaker();

  virtual Int_t Init();
  virtual Int_t InitRun(int);
  virtual Int_t FinishRun(int);
  virtual Int_t Make();
  virtual Int_t Finish();

  /// switch to select the Helix Geometry
  void setOuterGeometry(const bool val=kTRUE);
  /// switch to turn on slewing correction or not - maybe only T0 in the first step.
  void setSlewingCorr(const bool val=kTRUE);
  /// switch to turn on the use of event vertex
  void setUseEventVertex(const bool);
  /// switch to set the VPD # of hits cut
  void setVPDHitsCut(const Int_t, const Int_t);
  /// switch to read in StEvent/MuDst
  void setMuDstIn(const bool val=kTRUE);
  /// switch to FXT mode to include protons in T0 calculation
  void setFXTMode(const bool val=kTRUE);

  ///  switch to pp/pA mode
  void setPPPAMode(const Bool_t val=kFALSE);
  /// switch to pp/pA pion selection
  void setPPPAPionSel(const Bool_t val=kFALSE);
  /// switch outlier rejection in pp/pA
  void setPPPAOutlierRej(const Bool_t val=kFALSE);
  /// enable nSigmTof mode
  void setNSigmaTofMode(const Bool_t val=kFALSE);
  /// explicitly set Run15 slewing correction
  void setRun15Slew(const Bool_t val=kFALSE);
  /// enable pp/pA QA histograms
  void setPPPAModeHist(const Bool_t val=kFALSE);
  /// enable JP 2015 fudge factor
  void setJP2015FF(const Bool_t val=kFALSE);
  /// enable MB 2015 fudge factor
  void setMB2015FF(const Bool_t val=kFALSE);
  /// enable JP 2017 fudge factor
  void setJP2017FF(const Bool_t val=kFALSE);

  /// enable QA histogram filling
  void setCreateHistoFlag(Bool_t histos=kTRUE);
  /// set histogram output file name
  void setHistoFileName(const Char_t*);

  /// set file name for pp/pA QA histograms
  void setPPPAModeHistoFileName(const Char_t* filename);

  /// read calibration parameters from file
  void setInitFromFile(const Bool_t val = kTRUE);
  void setCalibFilePvpd(const Char_t*);
  void setCalibFileTot(const Char_t*);
  void setCalibFileZhit(const Char_t*);
  void setCalibFileT0(const Char_t*);

  enum calibtype {NOTSET=0, BOARDCALIB=960, MODULECALIB=3840, CELLCALIB=23040};
  Int_t getZCalibType();
  Int_t getTotCalibType();

  void forceTStartZero();

private:

  /// Calibration type
  calibtype mZCalibType, mTotCalibType;
  /// Reset the calibration parameters
  void  resetPars();
  /// initialize StEvent/MuDst pointer
  void initEvent();
  /// Reset the VPD parameters
  void resetVpd();
  /// Load Vpd data
  void loadVpdData();
  /// write the start time to btofHeader
  void writeStartTime();
  /// Initialize the calibration parameters from dbase
  Int_t initParameters(Int_t runnumber);

  ///
  void processStEvent();
  ///
  void processMuDst();

  void cleanCalibMuDst();
  void cleanCalib(StMuBTofPidTraits&);  //! functions to clean up calib done before in MuDst

  /// calculate tstart from Vpd
  void tstart(const Double_t Vz, Double_t *tstart, Double_t *tdiff);  //! tstart calculation splitted into 2 steps
  /// full calibration function for tray hits
  Double_t tofAllCorr(const Double_t tof, const Double_t tot, const Double_t zlocal, const Int_t iTray, const Int_t iModuleChan);

  ///
  void tstart_NoVpd(const StBTofCollection *btofCollection, const StPrimaryVertex *pVtx, Double_t *tstart);
  void tstart_NoVpd(const StMuDst *muDst, const StMuPrimaryVertex *pVtx, Double_t *tstart);

  /// calculate start time using the VPD to use it in the BTof outlier rejection in pp/pA mode
  void   vpdTStartForBTofComparison(Double_t vz, Double_t *vpdStartTime);
  double fudgeFactor(double eta, double zVtx);
  void   fillDtiVsPtotProfiles(double eta, double zVtx, double ptot, double Dti);
  
  void   archiveVpdHitInfo();


  ///
  float tofCellResolution(const Int_t iTray, const Int_t iModuleChan);

  /// book histograms
  void bookHistograms();
  /// write histograms
  void writeHistograms();

  /// book pp/pA histograms
  void bookPPPAHistograms();
  /// write pp/pA histograms
  void writePPPAHistograms();


private:
  enum{
    mNTOF = 192,        // 192 for tof in Run 8++
    mNTDIG = 8,         // 8 per tray in Run 8++
    mNModule = 32,      // 32 for tofr5++
    mNVPD = 19,         // 19 tubes at each side
    mNCell = 6,         // 6 cells per module
    mNBinMax = 60,      // 60 bins for T-Tot, T-Z correction

    mNTray = 120,        // 120 trays in full
    mWestVpdTrayId = 121,
    mEastVpdTrayId = 122
  };

  static const Double_t VHRBIN2PS; // Very High resolution mode, pico-second per bin
                                   // 1000*25/1024 (ps/chn)
  static const Double_t HRBIN2PS;  // High resolution mode, pico-second per bin
                                   // 97.65625= 1000*100/1024  (ps/chn)
  static const Double_t TMAX;      // tdc limit
  static const Double_t VZDIFFCUT; // VzVpd - VzProj cut
  static const Double_t DCARCUT;   // dcaR cut

  static const Double_t mC_Light;  // = C_C_LIGHT/1.e9;

  static const Float_t BTHBLCHCNST; // Bethe-Bloch constant used for dE/dx 
                                    // correction in start-time calculations 
                                    // in pppAMode
  static const Float_t DEDXTCORR[2];

    Bool_t     mValidCalibPar = kFALSE;
    Bool_t     mValidStartTime = kFALSE;

    Int_t      mVPDEastHitsCut = 0;
    Int_t      mVPDWestHitsCut = 0;

    Float_t   mTofTotEdge[mNTray][mNModule][mNCell][mNBinMax];//!From Double_t to Float_t
    Float_t   mTofTotCorr[mNTray][mNModule][mNCell][mNBinMax];//! from board-by-board to cell-by-cell
    Float_t   mTofZEdge[mNTray][mNModule][mNCell][mNBinMax];//! boards now filled 24 times
    Float_t   mTofZCorr[mNTray][mNModule][mNCell][mNBinMax];
    Double_t  mTofTZero[mNTray][mNModule][mNCell];  //! cell-by-cell T0

    Double_t   mVPDLeTime[2*mNVPD];

    Double_t   mTSumEast = 0.0;
    Double_t   mTSumWest = 0.0;
    Double_t   mTSumEastSigma = 0.0;
    Double_t   mTSumWestSigma = 0.0;
    UInt_t     mVPDHitPatternEast = 0;
    UInt_t     mVPDHitPatternWest = 0;
    Int_t      mNEast = 0;
    Int_t      mNWest = 0;            //! for Run8 to save time, these stored first
    Double_t   mVPDVtxZ = 0.0;          //! vertex z from VPD
    Double_t   mProjVtxZ = 0.0;         //! vertex z from track projection, track closest to beam line
    Double_t   mEvtVtxZ = 0.0;          //! vertex z from event vertex (mostly TPC vertex)
    Double_t   mTDiff = 0.0;            //! time difference between east and west
    Double_t   mTStart = 0.0;           //! start time
    Int_t      mNTzero = 0;           //! number of hits used in T0 (non-vpd-start)

    Int_t    mNTzeroCan = 0;
    Double_t mTCanFirst = 0.0;
    Double_t mTCanLast = 0.0;

    Double_t     mVpdEHits = 0;
    Double_t     mVpdWHits = 0;
    Double_t     mVpdEGoodHits = 0;
    Double_t     mVpdWGoodHits = 0;
    Double_t     mEarliestVpdEHit = 0.0;
    Double_t     mEarliestVpdWHit = 0.0;
    Double_t     mClosestVpdEHit = 0.0;
    Double_t     mClosestVpdWHit = 0.0;
    Double_t     mLatestVpdEHit = 0.0;
    Double_t     mLatestVpdWHit = 0.0;

    StPhysicalHelixD* mBeamHelix;  //! beamline helix used for Run 8
    ///
    StEvent*          mEvent;
    StBTofHeader*     mBTofHeader;
    StMuDst*          mMuDst;
    Bool_t            mMuDstIn;
    Bool_t            isMcFlag;

    Bool_t            mOuterGeometry;
    Bool_t            mSlewingCorr;  //! switch for slewing correction since run 8
    Bool_t            mUseEventVertex = kFALSE; //! switch for using event vertices
    Bool_t            mInitFromFile; //! switch for reading from files
    Bool_t            mUseVpdStart;  //! switch for vpd start
    Bool_t            mForceTStartZero = false; //!switch to allow totally startless bTOF
    Bool_t            mFXTMode = kFALSE; //! FXT mode, protons included in calculating T0

    StVpdSimConfig*     mVpdResConfig; //! database access VPD resolutions
    std::map<int, StVpdSimConfig::SingleTubeParams>     mVpdRes;
    StBTofSimResParams* mBTofRes; //! database access BTOF resolutions

    Bool_t            mPPPAMode = kFALSE; //! pp and pA Mode
    Bool_t            mPPPAPionSel = kFALSE; //! Only use the particle selection cuts from pppAMode
    Bool_t            mPPPAOutlierRej = kFALSE; //! Only use the outlier rejection algorithm from pppAMode
    Bool_t            mNSigmaTofMode = kFALSE; //! Only use the nSigmaTOF value from pppAMode
    Bool_t            mRun15Slew = kFALSE; //! Trigger slewing corrections for Run 15

    string mCalibFilePvpd; //! filename for pvpd calibration parameters
    string mCalibFileTot;  //! filename for ToT calibration parameters
    string mCalibFileZhit; //! filename for Zhit calibration parameters
    string mCalibFileT0;   //! filename for T0 calibration parameters

    Bool_t   mHisto;            //! switch to fill QA histograms
    string   mHistoFileName;    //! histogram file name
    TH1D*    hEventCounter = nullptr;     //!

    Bool_t   mPPPAModeHist = kFALSE;
    string   mPPPAModeHistoFileName;
    Int_t    iYr; //! specifies which set of FF to use for dE/dx corretion
    TH1D*    hDtiArray[10];
    TH1D*    hGDcaArray[5];
    TProfile* hDtiVsPtot[11][2];

    virtual const char *GetCVS() const
      {static const char cvs[]="Tag $Name:  $ $Id: StBTofCalibMaker.h,v 1.15 2021/05/29 23:57:08 geurts Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

    ClassDef(StBTofCalibMaker,4)
};

inline void StBTofCalibMaker::forceTStartZero(  ) { mForceTStartZero = true; }
inline void StBTofCalibMaker::setVPDHitsCut(const Int_t ieast, const Int_t iwest) { mVPDEastHitsCut=ieast ; mVPDWestHitsCut=iwest; }
inline void StBTofCalibMaker::setOuterGeometry(const bool val) { mOuterGeometry=val; }
inline void StBTofCalibMaker::setSlewingCorr(const bool val) { mSlewingCorr=val; }
inline void StBTofCalibMaker::setUseEventVertex(const bool val) { mUseEventVertex=val; }
inline void StBTofCalibMaker::setMuDstIn(const bool val) { mMuDstIn = val; }
inline void StBTofCalibMaker::setHistoFileName(const Char_t* filename){ mHistoFileName=filename; }
inline void StBTofCalibMaker::setCreateHistoFlag(Bool_t histos)  { mHisto = histos; }
inline void StBTofCalibMaker::setInitFromFile(const Bool_t val)  {mInitFromFile = val; }
inline void StBTofCalibMaker::setCalibFilePvpd(const Char_t* filename) {mCalibFilePvpd = filename;}
inline void StBTofCalibMaker::setCalibFileTot(const Char_t* filename)  {mCalibFileTot = filename;}
inline void StBTofCalibMaker::setCalibFileZhit(const Char_t* filename) {mCalibFileZhit = filename;}
inline void StBTofCalibMaker::setCalibFileT0(const Char_t* filename)   {mCalibFileT0 = filename;}
inline Int_t StBTofCalibMaker::getZCalibType() {return Int_t(mZCalibType);}
inline Int_t StBTofCalibMaker::getTotCalibType() {return Int_t(mTotCalibType);}
inline void StBTofCalibMaker::setFXTMode(const Bool_t val) {mFXTMode = val;}

inline void StBTofCalibMaker::setPPPAMode(const Bool_t val) { mPPPAMode = val; if(val){LOG_INFO << "You are now using PPPAMode!" << endm;} if(!val){mPPPAModeHist = kFALSE;}; }
inline void StBTofCalibMaker::setPPPAPionSel(const Bool_t val) { mPPPAPionSel = val; if(mPPPAPionSel) { LOG_INFO << "mPPPAPionSel is on!" << endm;}}
inline void StBTofCalibMaker::setPPPAOutlierRej(const Bool_t val) { mPPPAOutlierRej = val; if(mPPPAOutlierRej) { LOG_INFO << "mPPPAOutlierRej is on!" << endm;}}
inline void StBTofCalibMaker::setNSigmaTofMode(const Bool_t val) { mNSigmaTofMode = val; if(mNSigmaTofMode) { LOG_INFO << "mNSigmaTofMode is on!" << endm;}}
inline void StBTofCalibMaker::setPPPAModeHist(const Bool_t val) { if(mPPPAMode){mPPPAModeHist = val; if(mPPPAModeHist){LOG_INFO << "mPPPAModeHist is on!" << endm;}} else if(val){LOG_INFO << "setPPPAModeHist() is only valid when pppAMode is on! " << endm;} else{mPPPAModeHist = val;}; }
inline void StBTofCalibMaker::setPPPAModeHistoFileName(const Char_t* filename) { if(mPPPAMode && mPPPAModeHist){mPPPAModeHistoFileName = filename;} else {LOG_INFO << "setPPPAMode() and setPPPAModeHist() must be turned on first before calling setPPPAModeHistoFileName()!" << endm;}; }
inline void StBTofCalibMaker::setRun15Slew(const Bool_t val) {if(val){mRun15Slew = val; LOG_INFO << "Using Run 15 slewing corrections" << endm;}; }

#endif
