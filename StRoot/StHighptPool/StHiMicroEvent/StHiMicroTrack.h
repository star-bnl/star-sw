/***************************************************************************
 *
 * $Id: StHiMicroTrack.h,v 1.1 2002/04/02 19:36:15 jklay Exp $                                                         
 *
 * Author: Bum Choi, UT Austin, Apr 2002
 *
 ***************************************************************************
 *
 * Description:  This is a uDST for highpt Analysis: Track information              
 *               
 ***************************************************************************
 *
 * $Log: StHiMicroTrack.h,v $
 * Revision 1.1  2002/04/02 19:36:15  jklay
 * Bums highpt uDST format
 *
 *
 **************************************************************************/
#ifndef StHiMicroTrack_H
#define StHiMicroTrack_H

#include "TObject.h"

class StHiMicroTrack : public TObject{
 public:
  StHiMicroTrack();
  virtual ~StHiMicroTrack();

//Accessor methods
  Float_t PtPr()		const	{ return mPtPr; }
  Float_t PtGl()		const	{ return mPtGl; }
  Float_t CurvPr()		const	{ return mCurvPr; }
  Float_t CurvGl()		const	{ return mCurvGl; }
  Float_t EtaPr()		const	{ return mEtaPr; }
  Float_t PhiPr()		const	{ return mPhiPr; }
  Float_t EtaGl()		const	{ return mEtaGl; }
  Float_t PhiGl()		const	{ return mPhiGl; }
  Float_t DcaPr()		const	{ return mDcaPr; }  //3d
  Float_t DcaGl()		const	{ return mDcaGl; }  //3d
  Float_t DcaXYPr()		const	{ return mDcaXYPr; }
  Float_t DcaXYGl()		const	{ return mDcaXYGl; }
  Float_t DcaZGl()		const	{ return mDcaZGl; }
  Float_t Dedx()		const	{ return mDedx; }
  Float_t Chi2()		const	{ return mChi2; }
  Float_t FirstZ()		const	{ return mFirstZ; }
  Float_t LastZ()		const	{ return mLastZ; }
  Short_t FirstPadRow()		const	{ return mFirstPadrow; }
  Short_t LastPadRow()  	const	{ return mLastPadrow; }
  Short_t InnerPadList()  	const	{ return mInnerPadList; }
  Int_t   OuterPadList()  	const	{ return mOuterPadList; }
  Short_t FirstSector()		const	{ return mFirstSector; }
  Short_t LastSector()		const	{ return mLastSector; } 
  Float_t DipAnglePr()		const	{ return mDipAnglePr; }
  Float_t DipAngleGl()		const	{ return mDipAngleGl; }
  Float_t CrossingAngle()	const	{ return mCrossingAngle; }
  Float_t ResPtGlPr()		const	{ return mResPtGlPr; }
  Float_t ResCurvGlPr() 	const	{ return mResCurvGlPr; }
  Short_t DedxPts()		const	{ return mDedxPts; }
  Short_t FitPts()		const	{ return mFitPts; }
  Short_t AllPts()		const	{ return mAllPts; }
  Short_t MaxPossPts()		const	{ return mMaxPossPts; }
  Short_t Flag()		const	{ return mFlag; }
  Short_t Charge()		const	{ return mCharge; }
  Float_t AvgOutXYRes()		const	{ return mAvgOutXYRes; }
  Float_t AvgInXYRes()		const	{ return mAvgInXYRes; }

  //Set methods
  void SetPtPr(Float_t val)		{ mPtPr=val;}
  void SetPtGl(Float_t val)		{ mPtGl=val;}
  void SetCurvPr(Float_t val)		{ mCurvPr=val;}
  void SetCurvGl(Float_t val)		{ mCurvGl=val;}

  void SetEtaPr(Float_t val)		{ mEtaPr=val;}
  void SetPhiPr(Float_t val)		{ mPhiPr=val;}
  void SetEtaGl(Float_t val)		{ mEtaGl=val;}
  void SetPhiGl(Float_t val)		{ mPhiGl=val;}
  void SetDcaPr(Float_t val)		{ mDcaPr=val;}
  void SetDcaGl(Float_t val)		{ mDcaGl=val;}
  void SetDcaXYPr(Float_t val)		{ mDcaXYPr=val; }
  void SetDcaXYGl(Float_t val)		{ mDcaXYGl=val; }
  void SetDcaZGl(Float_t val)		{ mDcaZGl=val; }
  void SetDedx(Float_t val)		{ mDedx=val; }
  void SetDedxPts(Int_t val)		{ mDedxPts=val; }
  void SetChi2(Float_t val)		{ mChi2=val; }
  void SetFirstZ(Float_t val)		{ mFirstZ=val; }
  void SetLastZ(Float_t val)		{ mLastZ=val; }
  void SetFirstPadrow(Short_t val) 	{ mFirstPadrow=val; }
  void SetLastPadrow(Short_t val)	{ mLastPadrow=val; }
  void SetInnerPadList(Short_t val)	{ mInnerPadList=val; }
  void SetOuterPadList(Int_t val)	{ mOuterPadList=val; }
  void SetFirstSector(Short_t val)	{ mFirstSector=val; }
  void SetLastSector(Short_t val)	{ mLastSector=val; }
  void SetDipAnglePr(Float_t val)	{ mDipAnglePr=val; }
  void SetDipAngleGl(Float_t val)	{ mDipAngleGl=val;}
  void SetCrossingAngle(Float_t val)	{ mCrossingAngle=val;}
  
  void SetFitPts(Short_t val)		{ mFitPts=val; }
  void SetAllPts(Short_t val)		{ mAllPts=val; }
  void SetMaxPossPts(Short_t val)	{ mMaxPossPts=val; }
  void SetFlag(Short_t val)		{ mFlag=val; }
  void SetCharge(Short_t val)		{ mCharge=val; }
  void SetAvgOutXYRes(Float_t val)	{ mAvgOutXYRes=val; }
  void SetAvgInXYRes(Float_t val)	{ mAvgInXYRes=val; }

  private:

  Float_t mPtPr;
  Float_t mPtGl;
  Float_t mCurvPr;
  Float_t mCurvGl;
  Float_t mEtaPr;
  Float_t mPhiPr;
  Float_t mEtaGl;
  Float_t mPhiGl;
  Float_t mDcaPr; // dca3d primary (0)
  Float_t mDcaGl; 
  Float_t mDcaXYPr;
  Float_t mDcaXYGl;
  Float_t mDcaZGl;
  Float_t mDedx;
  
  Float_t mChi2; // meaningless

  Float_t mFirstZ; 
  Float_t mLastZ;
  Short_t mFirstPadrow;
  Short_t mPadrowList;
  Short_t mLastPadrow;
  Short_t mInnerPadList;  //Bitwise Encoded list of hit pads (Inner) 
  Int_t   mOuterPadList;  //Bitwise Encoded list of hit pads (Outer)
  Short_t mFirstSector;
  Short_t mLastSector;

  Float_t mDipAnglePr;
  Float_t mDipAngleGl;
  
  Float_t mCrossingAngle;

  Float_t mResPtGlPr;
  Float_t mResCurvGlPr;
  Short_t mDedxPts;
  Short_t mFitPts;
  Short_t mAllPts;
  Short_t mMaxPossPts;
  Short_t mFlag;
  Short_t mCharge;
  
  Float_t mAvgOutXYRes;
  Float_t mAvgInXYRes;

 protected:
  ClassDef(StHiMicroTrack,1)
};  

#endif
