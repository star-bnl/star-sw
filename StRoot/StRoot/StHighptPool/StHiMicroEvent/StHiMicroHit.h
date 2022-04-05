/***************************************************************************
 *
 * $Id: StHiMicroHit.h,v 1.1 2002/04/02 19:36:15 jklay Exp $                                                         
 *
 * Author: Bum Choi, UT Austin, Apr 2002
 *
 ***************************************************************************
 *
 * Description:  This is a uDST for highpt Analysis: Hit Information               
 *               
 ***************************************************************************
 *
 * $Log: StHiMicroHit.h,v $
 * Revision 1.1  2002/04/02 19:36:15  jklay
 * Bums highpt uDST format
 *
 *
 **************************************************************************/
#ifndef StHiMicroHit_H
#define StHiMicroHit_H

#include "TObject.h"
//class StHiMicroTrack;

class StHiMicroHit : public TObject{

 public:
  StHiMicroHit();
  virtual ~StHiMicroHit();

//Here are the accessor methods for the private data members
  // basic track info
  Float_t  PtPr()	const	{ return mPtPr; }
  Float_t  PtGl()	const	{ return mPtGl; }
  Float_t  Eta()	const	{ return mEta; }
  Float_t  Phi()	const	{ return mPhi; }
  UShort_t FitPts()	const	{ return mFitPts; }
  Float_t  SDcaGl()	const	{ return mSignedDcaGl; }
  Float_t  DipAngle()	const	{ return mDipAngle; }
  Float_t  ExitZ()	const	{ return mExitZ; }
  Short_t  Charge()	const	{ return mCharge; }

  // hit info
  Float_t  R()		const	{ return mR; }
  Float_t  Z()		const	{ return mZ; }
  Int_t    PadRow()	const	{ return mPadRow; } // ULong_t in stevent
  Int_t    Sector()	const	{ return mSector; } // ULong_t in stevent

  // residuals
  Float_t  ZResPr()	const	{ return mZResPr; }
  Float_t  XYResPr()	const	{ return mXYResPr; }
  Float_t  ZResGl()	const	{ return mZResGl; }
  Float_t  XYResGl()	const	{ return mXYResGl; }

//These are to set the values of the private data members
  void SetPtPr(Float_t val)	{ mPtPr=val; }
  void SetPtGl(Float_t val)	{ mPtGl=val; }
  void SetEta(Float_t val)	{ mEta=val; }
  void SetPhi(Float_t val)	{ mPhi=val; }
  void SetFitPts(UShort_t val)	{ mFitPts=val; }
  void SetSDcaGl(Float_t val)	{ mSignedDcaGl=val; }
  void SetDipAngle(Float_t val)	{ mDipAngle=val; }
  void SetExitZ(Float_t val)	{ mExitZ=val; }
  void SetCharge(Short_t val)	{ mCharge=val; }
  void SetR(Float_t val)	{ mR=val; }
  void SetZ(Float_t val)	{ mZ=val; }
  void SetPadRow(Int_t val)	{ mPadRow=val; }
  void SetSector(Int_t val)	{ mSector=val; }
  void SetZResPr(Float_t val)	{ mZResPr=val; }
  void SetXYResPr(Float_t val)	{ mXYResPr=val; }
  void SetZResGl(Float_t val)	{ mZResGl=val; }
  void SetXYResGl(Float_t val)	{ mXYResGl=val; }

  private:
  // basic track info
  Float_t  mPtPr;
  Float_t  mPtGl;
  Float_t  mEta;
  Float_t  mPhi;
  UShort_t mFitPts;
  Float_t  mSignedDcaGl;
  Float_t  mDipAngle;
  Float_t  mExitZ;
  Short_t  mCharge;

  // hit info
  Float_t  mR;
  Float_t  mZ;
  Int_t    mPadRow; // ULong_t in stevent
  Int_t    mSector; // ULong_t in stevent
  
  // residuals
  Float_t  mZResPr;
  Float_t  mXYResPr;
  Float_t  mZResGl;
  Float_t  mXYResGl;

  ClassDef(StHiMicroHit,1)
};

#endif
