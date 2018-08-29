/*******************************************************************
 *
 * $Id: StVpdCalibMaker.h,v 1.8 2017/03/02 18:26:50 jeromel Exp $
 *
 *******************************************************************/
/*!
 \class StVpdCalibMaker
 \author Xin Dong
 The VPD Calibration Maker applies the calibration for upVPD start detectors.
 The Maker reads the pre-determined calibration constants from either
 database or standard formatted files. The calibrated VPD information is
 stored in the btofheader.
*/
/*****************************************************************
 *
 * $Log: StVpdCalibMaker.h,v $
 * Revision 1.8  2017/03/02 18:26:50  jeromel
 * Updates to StVpdCalibMaker after review - changes by jdb, nl
 *
 * Revision 1.7  2014/08/06 11:43:52  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.6  2011/02/23 20:00:52  geurts
 * Change MaxBin for ToT arrays from 60 to 128 (in agreement with the IDL definition of vpdTotCorr)
 * Move the log message that informs the user about the start-timing mode outside the tube loop ... no need to see the same message 38 times.
 *
 * Revision 1.5  2011/01/07 21:28:51  geurts
 * Allow user to "force" VPD-start or startless mode, regardless of dbase setting
 *
 * Revision 1.4  2010/05/12 22:46:51  geurts
 * Startless BTOF self-calibration method (Xin)
 *
 * Revision 1.3  2010/05/06 22:37:41  geurts
 * Remove slower hits (outliers) in VPD timing calculations (Xin Dong)
 *
 * Revision 1.2  2009/11/09 21:26:02  geurts
 * basic doxygen added
 *
 * Revision 1.1  2009/11/09 20:55:54  geurts
 * first release
 *
 *
 *******************************************************************/
#ifndef STVPDCALIMAKER_H
#define STVPDCALIMAKER_H

#include "TMath.h"
#include "TF1.h"
#include "TH1D.h"
#include "TH2D.h"
#include "StMaker.h"
#include <vector>

class StEvent;
class StBTofCollection;
class StBTofHitCollection;
class StMuDst;

class StVpdCalibMaker : public StMaker{
public:

  /// Default constructor
  StVpdCalibMaker(const Char_t* name="vpdCalib");
  /// Destructor
  virtual ~StVpdCalibMaker();
    
  virtual Int_t Init();
  virtual Int_t InitRun(Int_t runnumber);
  //  virtual Int_t FinishRun(int);
  virtual Int_t Make();
  virtual Int_t Finish();

  /// enable QA histogram filling
  void setCreateHistoFlag(const Bool_t histos=kTRUE);
  /// set histogram output file name
  void setHistoFileName(const Char_t* filename);
                
  /// read calibration parameters from file
    // note: the default value will be changed back to kFALSE
  void setInitFromFile(const Bool_t initFromFile= kTRUE);
  void setCalibFilePvpd(const Char_t* filename);

  /// set the VPD # of hits cut
  void setVPDHitsCut(const Int_t eastVpdCut, const Int_t westVpdCut);
  /// switch to read in StEvent/MuDst
  void setMuDstIn();
  /// switch to force whether or not to use the VPD as the start time
  void setUseVpdStart(const Bool_t val=kTRUE);
  
  /// function for tofCalibMaker to know whether to use VPD as the start or not
  Bool_t useVpdStart() const;

private:
  /// Reset the calibration parameters
  void  resetPars();
  /// Reset the vpd time in every event
  void  resetVpd();
  /// Read in the vpd data
  Bool_t  loadVpdData();
  /// Write the vpd data
  Bool_t  writeVpdData() const;
  /// Initialize the calibration parameters from dbase
  Int_t initParameters(Int_t runnumber);

  /// calibrate the Vpd timing
  void  tsum(const Double_t *tot, const Double_t *time, std::vector<Int_t> qaTruth);
  /// find the Vpd vertex z
  void  vzVpdFinder();

  /// book histograms
  void bookHistograms();
  /// fill histograms
  void fillHistograms(); 
  /// write histograms
  void writeHistograms() const;

  static const Double_t VHRBIN2PS; // Very High resolution mode, pico-second per bin
                                   // 1000*25/1024 (ps/chn)
  static const Double_t HRBIN2PS;  // High resolution mode, pico-second per bin
                                   // 97.65625= 1000*100/1024  (ps/chn)
  static const Double_t TMAX;      // tdc limit
  static const Double_t VZDIFFCUT; //   VzVpd - VzProj cut
  static const Double_t TDIFFCUT;  // fabs(time - ave(others)) > cut, remove this hit
  static const Double_t FracTruncated; // fraction of truncation in mean calculation

  enum{
    NTDIG = 8,         // 8 per tray in Run 8++
    NVPD = 19,         // 19 tubes at each side
    MaxVpdVz = 20,
    NTray = 120,       // 120 BTof trays
    WestVpdTrayId = 121,
    EastVpdTrayId = 122,
    NBinMax = 128,      // 128 max bins for T-Tot, T-Z correction
  };

  Bool_t     mValidCalibPar;
  
  Int_t      mVPDEastHitsCut;
  Int_t      mVPDWestHitsCut;

  Double_t   mVPDTotEdge[2*NVPD][NBinMax];
  Double_t   mVPDTotCorr[2*NVPD][NBinMax];

  Double_t   mVPDLeTime[2*NVPD];
  Double_t   mVPDTot[2*NVPD];
  std::vector<Int_t> mVPD_qaTruth;

  Int_t      mFlag[2*NVPD];    
  Double_t   mTSumEast;
  Double_t   mTSumWest;
  UInt_t     mVPDHitPatternEast;
  UInt_t     mVPDHitPatternWest;
  Int_t      mNEast;
  Int_t      mNWest;                   //! for Run8 to save time, these stored first
  Double_t   mVPDVtxZ[MaxVpdVz];      //! vertex z from VPD
  Int_t      mNVzVpd;                   //! number of Vz vertex

  /// support two kinds of input
  Bool_t            mTruncation;       //! switch - do truncation
  StBTofCollection* mBTofColl;         //!
  StMuDst*          mMuDst;
  Bool_t            mMuDstIn;          //! switch - default is to read in StEvent

  Bool_t   mHisto;            //! switch to fill QA histograms
  string   mHistoFileName;    //! histogram file name
  TH1D*    mhEventCounter;     //!
  TH1D*    mhVpd[NVPD*2];     //! vpd resolution
  TH1D*    mhVpdAll;
  TH2D*    mhNVpdHits;         //! nhits west vs east
    TH1D*   mmVpdVertexHist;

  Bool_t mInitFromFile;  //! switch for reading from files
  string mCalibFilePvpd; //! filename for pvpd calibration parameters
  Bool_t mUseVpdStart;   //! switch for using Vpd as the start time (true by default)
  Bool_t mForceTofStart;   //! flag indicating that a user overrides any dbase-based start timing default

  virtual const char *GetCVS() const 
  {static const char cvs[]="Tag $Name:  $ $Id: StVpdCalibMaker.h,v 1.8 2017/03/02 18:26:50 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
    
  ClassDef(StVpdCalibMaker,1)
};

inline void StVpdCalibMaker::setVPDHitsCut(const Int_t ieast, const Int_t iwest) { mVPDEastHitsCut=ieast ; mVPDWestHitsCut=iwest; }
inline void StVpdCalibMaker::setMuDstIn() { mMuDstIn = kTRUE; }
inline void StVpdCalibMaker::setHistoFileName(const Char_t* filename){mHistoFileName=filename;}
inline void StVpdCalibMaker::setCreateHistoFlag(const Bool_t histos){mHisto = histos;}
inline void StVpdCalibMaker::setInitFromFile(const Bool_t val)  {mInitFromFile = val; }
inline void StVpdCalibMaker::setCalibFilePvpd(const Char_t* filename) {mCalibFilePvpd = filename;}
inline void StVpdCalibMaker::setUseVpdStart(const Bool_t val) {mUseVpdStart = val; mForceTofStart = kTRUE;}
inline Bool_t StVpdCalibMaker::useVpdStart() const { return mUseVpdStart; }
#endif
