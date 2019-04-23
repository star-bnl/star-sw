/*******************************************************************
 *
 * $Id: StBTofCalibMaker.h,v 1.12 2019/04/23 05:49:57 jdb Exp $
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
#include "StPhysicalHelixD.hh"

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

  /// enable QA histogram filling
  void setCreateHistoFlag(Bool_t histos=kTRUE);
  /// set histogram output file name
  void setHistoFileName(const Char_t*);

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
    
  
  /// book histograms
  void bookHistograms();
  /// write histograms
  void writeHistograms();
        
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

    string mCalibFilePvpd; //! filename for pvpd calibration parameters
    string mCalibFileTot;  //! filename for ToT calibration parameters
    string mCalibFileZhit; //! filename for Zhit calibration parameters
    string mCalibFileT0;   //! filename for T0 calibration parameters

    Bool_t   mHisto;            //! switch to fill QA histograms
    string   mHistoFileName;    //! histogram file name
    TH1D*    hEventCounter = nullptr;     //!
            
    virtual const char *GetCVS() const 
      {static const char cvs[]="Tag $Name:  $ $Id: StBTofCalibMaker.h,v 1.12 2019/04/23 05:49:57 jdb Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
    
    ClassDef(StBTofCalibMaker,3)
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

#endif
