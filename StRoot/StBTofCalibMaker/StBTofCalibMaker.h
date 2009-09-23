/*******************************************************************
 *
 * $Id: StBTofCalibMaker.h,v 1.1 2009/09/23 02:28:41 geurts Exp $
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
#ifndef ST_NO_NAMESPACES
using std::string;
using std::vector;
#endif

class StEvent;
class StBTofGeometry;
class StBTofCollection;
class StBTofHitCollection;
class StBTofPidTraits;
#include "StPhysicalHelixD.hh"

#if !defined(ST_NO_TEMPLATE_DEF_ARGS) || defined(__CINT__)
typedef vector<Int_t>  IntVec;
typedef vector<Double_t>  DoubleVec;
#else
typedef vector<Int_t, allocator<Int_t>>  IntVec;
typedef vector<Double_t, allocator<Double_t>>  DoubleVec;
#endif

class StBTofCalibMaker : public StMaker{
public:

  /// Default constructor
  StBTofCalibMaker(const char* name="tofCalib");
  /// Destructor
  ~StBTofCalibMaker();
    
  Int_t Init();
  Int_t InitRun(int);
  Int_t FinishRun(int);
  Int_t Make();
  Int_t Finish();

  /// Reset the calibration parameters
  void  resetPars();
  /// Initialize the calibration parameters from dbase
  Int_t initParameters(int);

  /// switch to select the Helix Geometry  
  void setOuterGeometry(const bool);

  /// switch to turn on slewing correction or not - maybe only T0 in the first step.
  void setSlewingCorr(const bool);
  /// switch to turn on the use of event vertex
  void setUseEventVertex(const bool);
  /// read calibration parameters from file
  void setInitFromFile(const Bool_t = kTRUE);
  void setCalibFilePvpd(Char_t*);
  void setCalibFileTot(Char_t*);
  void setCalibFileZhit(Char_t*);
  void setCalibFileT0(Char_t*);


  void tsum(const Double_t *tot, const Double_t *time);
  void tstart(const Double_t Vz, Double_t *tstart, Double_t *tdiff);  //! tstart calculation splitted into 2 steps
  Double_t tofAllCorr(const Double_t tof, const Double_t tot, const Double_t zlocal, const Int_t iTray, const Int_t iModuleChan);
  Double_t vpdVertexZ();
  
  void     setVPDHitsCut(const Int_t, const Int_t);

private:
    static const Int_t mNTOF = 192;        // 192 for tof in Run 8++
    static const Int_t mNTDIG = 8;         // 8 per tray in Run 8++
    static const Int_t mNModule = 32;      // 32 for tofr5++ 
    static const Int_t mNVPD = 19;         // 19 tubes at each side
    static const Int_t mNCell = 6;         // 6 cells per module
    static const Int_t mNBinMax = 60;      // 60 bins for T-Tot, T-Z correction

    static const Int_t mNTray = 120;        // 120 trays in full
    static const Int_t mWestVpdTrayId = 121;
    static const Int_t mEastVpdTrayId = 122;
    

    static const Double_t VHRBIN2PS =  24.4140625;  // Very High resolution mode, pico-second per bin
                                                 // 1000*25/1024 (ps/chn)
    static const Double_t HRBIN2PS = 97.65625;     // High resolution mode, pico-second per bin
                                                 // 97.65625= 1000*100/1024  (ps/chn)
    static const Double_t TMAX = 51200.;           // tdc limit
    static const Double_t VZDIFFCUT=6.;          //   VzVpd - VzProj cut

    static const Double_t mC_Light = C_C_LIGHT/1.e9;

    Bool_t     mValidCalibPar;
    Bool_t     mValidStartTime;

    Int_t      mVPDEastHitsCut;
    Int_t      mVPDWestHitsCut;

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
    Double_t   mProjVtxZ;         //! vertex z from track projection, track closest to beam line
    Double_t   mEvtVtxZ;          //! vertex z from event vertex (mostly TPC vertex)
    Double_t   mTDiff;            //! time difference between east and west
    Double_t   mTStart;           //! start time

    StPhysicalHelixD* mBeamHelix;  //! beamline helix used for Run 8
    ///
    StEvent*          mEvent;
    Bool_t            mOuterGeometry;

    Bool_t            mSlewingCorr;  //! switch for slewing correction since run 8
    Bool_t            mUseEventVertex; //! switch for using event vertices
    Bool_t            mInitFromFile; //! switch for reading from files

    string mCalibFilePvpd; //! filename for pvpd calibration parameters
    string mCalibFileTot;  //! filename for ToT calibration parameters
    string mCalibFileZhit; //! filename for Zhit calibration parameters
    string mCalibFileT0;   //! filename for T0 calibration parameters

    virtual const char *GetCVS() const 
      {static const char cvs[]="Tag $Name:  $ $Id: StBTofCalibMaker.h,v 1.1 2009/09/23 02:28:41 geurts Exp $ built "__DATE__" "__TIME__ ; return cvs;}
    
    ClassDef(StBTofCalibMaker,1)
};

inline void StBTofCalibMaker::setVPDHitsCut(const Int_t ieast, const Int_t iwest) { mVPDEastHitsCut=ieast ; mVPDWestHitsCut=iwest; }
inline void StBTofCalibMaker::setOuterGeometry(const bool val) { mOuterGeometry=val; }
inline void StBTofCalibMaker::setSlewingCorr(const bool val) { mSlewingCorr=val; }
inline void StBTofCalibMaker::setUseEventVertex(const bool val) { mUseEventVertex=val; }
inline Double_t StBTofCalibMaker::vpdVertexZ() { return mVPDVtxZ; }
inline void StBTofCalibMaker::setInitFromFile(const Bool_t val)  {mInitFromFile = val; }
inline void StBTofCalibMaker::setCalibFilePvpd(Char_t* filename) {mCalibFilePvpd = filename;}
inline void StBTofCalibMaker::setCalibFileTot(Char_t* filename)  {mCalibFileTot = filename;}
inline void StBTofCalibMaker::setCalibFileZhit(Char_t* filename) {mCalibFileZhit = filename;}
inline void StBTofCalibMaker::setCalibFileT0(Char_t* filename)   {mCalibFileT0 = filename;}

#endif
