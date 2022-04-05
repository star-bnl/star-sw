/*******************************************************************
 *
 * $Id: StVpdAnalysisMaker.h,v 1.2 2014/08/06 11:43:48 jeromel Exp $
 *
 * Author: Xin Dong
 *****************************************************************
 *
 * Description: Vpd analysis Maker to do the calibration for pVPD 
 *              hit pattern, VzVpd, Tdiff, Tstart
 *
 *****************************************************************
 *
 * $Log: StVpdAnalysisMaker.h,v $
 * Revision 1.2  2014/08/06 11:43:48  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.1  2008/09/02 18:27:39  dongx
 * first release.
 * - Vpd analysis maker from MuDst to extract vz, Tstart, Tdiff etc.
 * - TPC primary vertex used for Tstart and Tdiff calculation
 *
 *
 *******************************************************************/
#ifndef STVPDANALYSISMAKER_H
#define STVPDANALYSISMAKER_H

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
class StTofCollection;
class StTofRawDataCollection;
class StMuDst;
class StMuDstMaker;
class StMuEvent;
#include "StTofUtil/StTofrDaqMap.h"
#include "StTofUtil/StTofINLCorr.h"
#include "StTofUtil/StSortTofRawData.h"

#if !defined(ST_NO_TEMPLATE_DEF_ARGS) || defined(__CINT__)
typedef vector<Int_t>  IntVec;
typedef vector<Double_t>  DoubleVec;
#else
typedef vector<Int_t, allocator<Int_t>>  IntVec;
typedef vector<Double_t, allocator<Double_t>>  DoubleVec;
#endif

class StVpdAnalysisMaker : public StMaker{
public:

  /// Default constructor
  StVpdAnalysisMaker(const char* name="vpdAna");
  /// Destructor
  ~StVpdAnalysisMaker();
    
  Int_t Init();
  Int_t InitRun(int);
  Int_t FinishRun(int);
  Int_t Make();
  Int_t Finish();
  
  Int_t processEventYear8();

  /// Reset the calibration parameters
  void  resetCalibPars();
  void  resetVpdPars();
  /// Initialize the calibration parameters from dbase
  Int_t initParameters(int);

  void setVPDHitsCut(const Int_t, const Int_t);
  
  /// switch to turn on slewing correction or not - maybe only T0 in the first step.
  void setSlewingCorr(const bool);
  
  /// Run 8++ ->
  void tsum(const Double_t *tot, const Double_t *time);

  Int_t    numberOfVpdEast() const;
  Int_t    numberOfVpdWest() const;
  Double_t tstart() const;
  Double_t tdiff() const;
  Double_t vzVpd() const;

private:
  /// to check the tables from dbase in proper size -- year 4
    static const Int_t mNTDIG = 8;         // 8 per tray in Run 8++
    static const Int_t mNTray8 = 5;        // 5 trays in Run 8
    static const Int_t mNVPD = 19;         // 19 tubes at each side
    static const Int_t mWestVpdTrayId = 121;
    static const Int_t mEastVpdTrayId = 122;
    static const Int_t mNValidTrays_Run8 = 5;
    static const Int_t mNBinMax = 60;
        
    StTofINLCorr*         mTofINLCorr;
    StTofrDaqMap*         mDaqMap;

    Double_t   mVPDTotEdge[2*mNVPD][mNBinMax];
    Double_t   mVPDTotCorr[2*mNVPD][mNBinMax];
    Double_t   mVPDTZero[2*mNVPD];

    Double_t   mPhaseOffset8;     //! phase difference between e/w in run 8

    Double_t   mVPDLeTime[2*mNVPD];
    Double_t   mVPDTot[2*mNVPD];
    
    Double_t   mTSumEast;
    Double_t   mTSumWest;
    UInt_t     mVPDHitPatternEast;
    UInt_t     mVPDHitPatternWest;
    Int_t      mNEast;
    Int_t      mNWest;            //! for Run8 to save time, these stored first
    Double_t   mVPDVtxZ;          //! vertex z from VPD
    Double_t   mTDiff;            //! time difference between east and west
    Double_t   mTStart;           //! start time

    StEvent*          mEvent;
    StTofCollection*  mTofCollection;
    StSortTofRawData* mSortTofRawData;
    
    StMuDstMaker*     mMuDstMaker;
    StMuDst*          mMuDst;
    StMuEvent*        mMuEvent;

    Double_t          mVertexZ;     //! vertex z from TPC
    Bool_t            mYear8;

    Bool_t            mValidCalibPar;
    Bool_t            mSlewingCorr;  //! switch for slewing correction since run 8    
    Double_t          mVPDEastHitsCut;
    Double_t          mVPDWestHitsCut;    

    virtual const char *GetCVS() const 
      {static const char cvs[]="Tag $Name:  $ $Id: StVpdAnalysisMaker.h,v 1.2 2014/08/06 11:43:48 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
    
    ClassDef(StVpdAnalysisMaker,1)
};

inline void StVpdAnalysisMaker::setVPDHitsCut(const Int_t ieast, const Int_t iwest) { mVPDEastHitsCut=ieast ; mVPDWestHitsCut=iwest; }

inline void StVpdAnalysisMaker::setSlewingCorr(const bool val) { mSlewingCorr=val; }

inline Int_t StVpdAnalysisMaker::numberOfVpdEast() const { return mNEast; }

inline Int_t StVpdAnalysisMaker::numberOfVpdWest() const { return mNWest; }

inline Double_t StVpdAnalysisMaker::tstart() const { return mTStart; }

inline Double_t StVpdAnalysisMaker::tdiff() const { return mTDiff; }

inline Double_t StVpdAnalysisMaker::vzVpd() const { return mVPDVtxZ; }

#endif
