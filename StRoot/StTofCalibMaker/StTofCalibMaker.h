/*******************************************************************
 *
 * $Id: StTofCalibMaker.h,v 1.9 2014/08/06 11:43:46 jeromel Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: Tof Calibration Maker to do the calibration for pVPD 
 *              (start timing) , TOFp and TOFr
 *              store into StTofHit
 *
 *****************************************************************
 *
 * $Log: StTofCalibMaker.h,v $
 * Revision 1.9  2014/08/06 11:43:46  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.8  2008/09/03 22:30:43  dongx
 * mTStart added, applicable for Run8
 *
 * Revision 1.7  2008/06/17 17:49:19  dongx
 * Update for Run 8 - first release
 *
 * Revision 1.6  2007/03/05 18:51:02  dongx
 * updated for Run V CuCu calibration
 *  - INL correction moved in this maker
 *  - Tot Corr and Z Corr use new tables in data base
 *  - pVPD calibrated information cannot be fully stored within current infrastructure, need update on TofCollection. Configurations better than (1,1) are all selected.
 *
 * Revision 1.5  2005/04/12 17:33:48  dongx
 * update for year 5 data. not completed, leave as empty now.
 *
 * Revision 1.4  2004/07/16 18:28:18  dongx
 * -Tofp Slewing function changed in AuAu200 GeV Run IV
 * -Include those runs with eastern PVPD dead
 *
 * Revision 1.3  2004/07/16 15:06:08  dongx
 * Z correction function separated for TOFp and TOFr.
 * Use a new one for RunIV AuAu 200GeV runs
 *
 * Revision 1.2  2004/07/15 18:11:22  dongx
 *  -introduce two new tables in dbase: tofAdcRange & tofResolution
 *  -continue update on writing StTofPidTraits
 *
 * Revision 1.1  2004/07/01 17:23:48  dongx
 * first release
 *
 *
 *******************************************************************/
#ifndef STTOFCALIMAKER_H
#define STTOFCALIMAKER_H

#include "TMath.h"
#include "TF1.h"
#include "StMaker.h"

#define VHRBIN2PS 24.4140625  // Very High resolution mode, pico-second per bin
                              // 1000*25/1024 (ps/chn)
#define HRBIN2PS 97.65625     // High resolution mode, pico-second per bin
                              // 97.65625= 1000*100/1024  (ps/chn)
#define TMAX 51200.           // tdc limit

#include <string>
#include <vector>
#ifndef ST_NO_NAMESPACES
using std::string;
using std::vector;
#endif

class StEvent;
class StTofGeometry;
class StTofCollection;
class StTofDataCollection;
class StTofCellCollection;
class StTofSlatCollection;
class StTofHitCollection;
#include "StPhysicalHelixD.hh"
#include "StTofUtil/StSortTofRawData.h"

#if !defined(ST_NO_TEMPLATE_DEF_ARGS) || defined(__CINT__)
typedef vector<Int_t>  IntVec;
typedef vector<Double_t>  DoubleVec;
#else
typedef vector<Int_t, allocator<Int_t>>  IntVec;
typedef vector<Double_t, allocator<Double_t>>  DoubleVec;
#endif

class StTofCalibMaker : public StMaker{
public:

  /// Default constructor
  StTofCalibMaker(const char* name="tofCalib");
  /// Destructor
  ~StTofCalibMaker();
    
  Int_t Init();
  Int_t InitRun(int);
  Int_t FinishRun(int);
  Int_t Make();
  Int_t Finish();

  Int_t processEventYear2to4();
  Int_t processEventYear5();
  Int_t processEventYear8();

  /// Reset the calibration parameters
  void  resetPars();
  /// Initialize the calibration parameters from dbase
  Int_t initParameters(int);
  /// Initialize the formulas of calibration functions
  void  initFormulas();
  /// Delete the calibration functions
  void  clearFormulars();

  void setTDCLimits(const Double_t, const Double_t);
  void setADCCut(const Double_t);
  void setTDCWidth(const Double_t);

  void setPVPDADCLimits(const Double_t, const Double_t);
  void setPVPDTDCLimits(const Double_t, const Double_t);
  void setPVPDHitsCut(const Int_t, const Int_t);
  
  Bool_t validPVPDADC(const Double_t);
  Bool_t validPVPDTDC(const Double_t);

  /// switch to select the Helix Geometry  
  void setOuterGeometry(const bool);

  /// switch to turn on slewing correction or not - maybe only T0 in the first step.
  void setSlewingCorr(const bool);

  /// tofr T0 calibration function
  Double_t tofrT0Corr(const Double_t tof, const Double_t Tstart, const Int_t iDaqChan);
  /// tofr TA slewing correction function
  Double_t tofrSlewingCorr(const Double_t tof, const Double_t adc, const Int_t iDaqChan);
  /// tofr local Zhit correction function
  Double_t tofrZCorr(const Double_t tof, const Double_t zlocal);
  /// tofr overall correction function
  Double_t tofrAllCorr(const Double_t tof, const Double_t Tstart, const Double_t adc, const Double_t zlocal, const Int_t iDaqChan);

  /// tofp T0 calibration function
  Double_t tofpT0Corr(const Double_t tof, const Double_t Tstart, const Int_t iDaqChan);
  /// tofp TA slewing correction function
  Double_t tofpSlewingCorr(const Double_t tof, const Double_t adc, const Int_t iDaqChan);
  /// tofp loca Zhit correction function
  Double_t tofpZCorr(const Double_t tof, const Double_t zlocal);
  /// tofp overall correction function
  Double_t tofpAllCorr(const Double_t tof, const Double_t Tstart, const Double_t adc, const Double_t zlocal, const Int_t iDaqChan);
    
  /// Start timing calculation function
  Double_t tstart(const Double_t *adc, const Double_t *tdc, const Double_t Vz);

  /// Run 5 ->
  Double_t GetINLcorr(const int edgeid,const int tempchan,const int bin);
  Double_t tstart5(const Double_t *tot, const Double_t *time, const Double_t Vz);
  Double_t tofr5AllCorr(const Double_t tof, const Double_t tot, const Double_t zlocal, const Int_t iModuleChan);

  /// Run 8 ->
  void tsum8(const Double_t *tot, const Double_t *time);
  Double_t tstart8(const Double_t Vz);  //! tstart calculation splitted into 2 steps in Run 8 since no primary vertex in the event
  Double_t tofr8AllCorr(const Double_t tof, const Double_t tot, const Double_t zlocal, const Int_t iTray, const Int_t iModuleChan);
  Double_t vpdVertexZ();

private:
  /// to check the tables from dbase in proper size -- year 4
    static Int_t const mNCell     = 6;     // number of cells
    static Int_t const mNTOFr     = 120;   // TOFr daq channels
    static Int_t const mNTOFp     = 41;    // TOFp daq channels
    static Int_t const mNPVPD     = 6;     // pVPD daq channels
    static Int_t const mNMax      = 200;   // maximum channles
    static Int_t const mNPar      = 10;    // maximum correction parameters
    /// Run 5
    static Int_t const mNTOFr5    = 192;   // TOFr daq channels in Run 5
    static Int_t const mTdigBoard = 10;    // INL tables
    static Int_t const mTdcOnBoard = 4;
    static Int_t const mTdcChannel = 1024;
    static Int_t const mNBinMax = 60;
    /// Run 8
    static const Int_t mNTOF = 192;        // 192 for tof in Run 8++
    static const Int_t mNTDIG = 8;         // 8 per tray in Run 8++
    static const Int_t mNTray8 = 5;        // 5 trays in Run 8
    static const Int_t mNModule = 32;      // 32 for tofr5++ 
    static const Int_t mNVPD = 19;         // 19 tubes at each side


    static const Int_t mNTray = 120;        // 120 trays in full
    static const Int_t mWestVpdTrayId = 121;
    static const Int_t mEastVpdTrayId = 122;
    
    Double_t   mTDCBinWidth;
    Double_t   mTDCLowLimit;
    Double_t   mTDCHighLimit;
    Double_t   mADCCut;
    Double_t   mTDCWidth;
    
    Double_t   mPVPDADCLowLimit;
    Double_t   mPVPDADCHighLimit;
    Double_t   mPVPDTDCLowLimit;
    Double_t   mPVPDTDCHighLimit;

    /// add more ADC cut for RUN IV tofp
    Double_t   mTofrADCMin[mNTOFr];
    Double_t   mTofrADCMax[mNTOFr];
    Double_t   mTofpADCMin[mNTOFp];
    Double_t   mTofpADCMax[mNTOFp];

    Bool_t     mYear2;
    Bool_t     mYear3;
    Bool_t     mYear4;
    Bool_t     mYear5;
    Bool_t     mYear8;

    Bool_t     mEastPVPDValid; // 022-035 east pVPD dead Run 4

    Bool_t     mValidCalibPar;
    Bool_t     mValidStartTime;

    Double_t   mTofrT0[mNTOFr];
    Double_t   mTofrTAPar[mNTOFr*mNPar];
    Double_t   mTofrZPar[mNPar];
    Double_t   mTofpT0[mNTOFp];
    Double_t   mTofpTAPar[mNTOFp*mNPar];
    Double_t   mTofpZPar[mNPar];
    Double_t   mPVPDTAPar[mNPVPD*mNPar];
    
    Double_t   mPVPDTdc[mNPVPD];
    Double_t   mPVPDAdc[mNPVPD];

    Int_t      mPVPDEastHitsCut;
    Int_t      mPVPDWestHitsCut;

    Double_t   mTofrRes[mNTOFr];    // resolution of each channel;
    Double_t   mTofpRes[mNTOFp];    //
    Double_t   mPVPDRes[mNPVPD];

    TF1*       mTofrSlewing;
    TF1*       mTofpSlewing;
    TF1*       mTofrZCorr;
    TF1*       mTofpZCorr;
    TF1*       mPVPDSlewing;

    /// Run 5 ->
    StSortTofRawData*     mSortTofRawData;
    Double_t   mINLtable[mTdigBoard][mTdcOnBoard][mTdcChannel];

    Double_t   mTofr5TotEdge[mNTOFr5][mNBinMax];
    Double_t   mTofr5TotCorr[mNTOFr5][mNBinMax];
    Double_t   mTofr5ZEdge[mNTOFr5][mNBinMax];
    Double_t   mTofr5ZCorr[mNTOFr5][mNBinMax];

    Double_t   mPVPDTotEdge[mNPVPD][mNBinMax];
    Double_t   mPVPDTotCorr[mNPVPD][mNBinMax];

    Double_t   mPVPDLeTime[mNPVPD];
    Double_t   mPVPDTot[mNPVPD];

    /// Run 8 ->
    Double_t   mTofTotEdge[mNTray][mNTDIG][mNBinMax];
    Double_t   mTofTotCorr[mNTray][mNTDIG][mNBinMax];
    Double_t   mTofZEdge[mNTray][mNTDIG][mNBinMax];
    Double_t   mTofZCorr[mNTray][mNTDIG][mNBinMax];  //! board-by-board slewing
    Double_t   mTofTZero[mNTray][mNModule][mNCell];  //! cell-by-cell T0

    Double_t   mVPDTotEdge[2*mNVPD][mNBinMax];
    Double_t   mVPDTotCorr[2*mNVPD][mNBinMax];
    Double_t   mVPDTZero[2*mNVPD];

    Double_t   mVPDLeTime[2*mNVPD];
    Double_t   mVPDTot[2*mNVPD];
    
    Double_t   mTSumEast;
    Double_t   mTSumWest;
    UInt_t     mVPDHitPatternEast;
    UInt_t     mVPDHitPatternWest;
    Int_t      mNEast;
    Int_t      mNWest;            //! for Run8 to save time, these stored first
    Double_t   mVPDVtxZ;          //! vertex z from VPD
    Double_t   mProjVtxZ;          //! vertex z from track projection, track cloest to beam line
    Double_t   mTDiff;            //! time difference between east and west
    Double_t   mTStart;           //! start time

    Double_t   mPhaseOffset8;     //! phase difference between e/w in run 8
    StPhysicalHelixD* mBeamHelix;  //! beamline helix used for Run 8
    ///
    StTofGeometry*    mTofpGeom; //! pointer to the TOF geometry utility class
    StEvent*          mEvent;
    Bool_t            mOuterGeometry;

    Bool_t            mSlewingCorr;  //! switch for slewing correction since run 8

    virtual const char *GetCVS() const 
      {static const char cvs[]="Tag $Name:  $ $Id: StTofCalibMaker.h,v 1.9 2014/08/06 11:43:46 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
    
    ClassDef(StTofCalibMaker,5)
};

inline void StTofCalibMaker::setTDCLimits(const Double_t tmin, const Double_t tmax) { mTDCLowLimit = tmin; mTDCHighLimit = tmax; }

inline void StTofCalibMaker::setADCCut(const Double_t cut) { mADCCut = cut; }

inline void StTofCalibMaker::setTDCWidth(const Double_t tdcwid) { mTDCWidth = tdcwid; }

inline void StTofCalibMaker::setPVPDTDCLimits(const Double_t tmin, const Double_t tmax) { mPVPDTDCLowLimit = tmin; mPVPDTDCHighLimit = tmax; }

inline void StTofCalibMaker::setPVPDADCLimits(const Double_t amin, const Double_t amax) { mPVPDADCLowLimit = amin; mPVPDADCHighLimit = amax; }

inline Bool_t StTofCalibMaker::validPVPDADC(const Double_t adc) { return (adc>=mPVPDADCLowLimit&&adc<mPVPDADCHighLimit); }

inline Bool_t StTofCalibMaker::validPVPDTDC(const Double_t tdc) { return (tdc>=mPVPDTDCLowLimit&&tdc<mPVPDTDCHighLimit); }

inline void StTofCalibMaker::setPVPDHitsCut(const Int_t ieast, const Int_t iwest) { mPVPDEastHitsCut=ieast ; mPVPDWestHitsCut=iwest; }

inline void StTofCalibMaker::setOuterGeometry(const bool val) { mOuterGeometry=val; }

inline void StTofCalibMaker::setSlewingCorr(const bool val) { mSlewingCorr=val; }

inline Double_t StTofCalibMaker::vpdVertexZ() { return mVPDVtxZ; }
#endif
