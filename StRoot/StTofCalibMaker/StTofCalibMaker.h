/*******************************************************************
 *
 * $Id: StTofCalibMaker.h,v 1.1 2004/07/01 17:23:48 dongx Exp $
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

class StEvent;
class StTofGeometry;
class StTofCollection;
class StTofDataCollection;
class StTofCellCollection;
class StTofSlatCollection;
class StTofHitCollection;

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
  
  /// Reset the calibration parameters
  void  resetPars();
  /// Initialize the calibration parameters from dbase
  Int_t initParameters();
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
    
  // Start timing calculation function
  Double_t tstart(const Double_t *adc, const Double_t *tdc, const Double_t Vz);
    
private:
    // to check the tables from dbase in proper size -- year 4
    static Int_t const mNModule   = 20;    // number of modules
    static Int_t const mNCell     = 6;     // number of cells
    static Int_t const mNTOFr     = 120;   // TOFr daq channels
    static Int_t const mNTOFp     = 41;    // TOFp daq channels
    static Int_t const mNPVPD     = 6;     // pVPD daq channels
    static Int_t const mNMax      = 200;   // maximum channles
    static Int_t const mNPar      = 10;    // maximum correction parameters

    Double_t   mTDCBinWidth;
    Double_t   mTDCLowLimit;
    Double_t   mTDCHighLimit;
    Double_t   mADCCut;
    Double_t   mTDCWidth;
    
    Double_t   mPVPDADCLowLimit;
    Double_t   mPVPDADCHighLimit;
    Double_t   mPVPDTDCLowLimit;
    Double_t   mPVPDTDCHighLimit;

    Bool_t     mYear2;
    Bool_t     mYear3;
    Bool_t     mYear4;

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

    TF1*       mTofrSlewing;
    TF1*       mTofpSlewing;
    TF1*       mZCorr;
    TF1*       mPVPDSlewing;

    StTofGeometry*    mTofpGeom; //! pointer to the TOF geometry utility class
    StEvent*          mEvent;
    Bool_t            mOuterGeometry;

    virtual const char *GetCVS() const 
      {static const char cvs[]="Tag $Name:  $ $Id: StTofCalibMaker.h,v 1.1 2004/07/01 17:23:48 dongx Exp $ built "__DATE__" "__TIME__ ; return cvs;}
    
    ClassDef(StTofCalibMaker,1)
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

#endif
