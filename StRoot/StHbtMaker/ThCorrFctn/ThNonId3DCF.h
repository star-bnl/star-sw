/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : Calculate the theoretical QInv correlation function 
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#ifndef ThNonId3DCF_hh
#define ThNonId3DCF_hh

#include "StHbtMaker/Base/StHbtRoot1DCF.hh"
#include "StHbtMaker/Base/StHbtThCorrFctn.hh"
#include "TProfile.h"

class StHbtThPair;

class ThNonId3DCF :  public virtual StHbtThCorrFctn  {
 public:
  ThNonId3DCF(char* title, const int& nbins, const float& QinvLo, const float& QinvHi);
  ThNonId3DCF(const ThNonId3DCF& ThCf);
  
  virtual ~ThNonId3DCF();

  void AddNum(StHbtThPair*);
  void AddDen(StHbtThPair*);
  
  virtual StHbtCorrFctn* Clone() {return 0;}   // Legacy code due to previous bug
                                               // in StHbtCorrFctn, do not use
  StHbtThCorrFctn* ThClone() const ;

  virtual void Write() ;
  virtual void Finish();
  virtual StHbtString Report();
  virtual void SetBtRange(double aBtMin, double aBtMax);
  virtual void SetUtRange(double aUtMin, double aUtMax);

 private:
  
  StHbt1DHisto* mNumOutP;
  StHbt1DHisto* mDenOutP;  
  StHbt1DHisto* mRatOutP;  
  StHbt1DHisto* mNumOutN;
  StHbt1DHisto* mDenOutN;  
  StHbt1DHisto* mRatOutN;
  StHbt1DHisto* mRatOut; 
  StHbt1DHisto* mRatOutNOverP;

  StHbt1DHisto* mNumSideP;
  StHbt1DHisto* mDenSideP;  
  StHbt1DHisto* mRatSideP;  
  StHbt1DHisto* mNumSideN;
  StHbt1DHisto* mDenSideN;  
  StHbt1DHisto* mRatSideN;
  StHbt1DHisto* mRatSide; 
  StHbt1DHisto* mRatSideNOverP;

  StHbt1DHisto* mNumLongP;
  StHbt1DHisto* mDenLongP;  
  StHbt1DHisto* mRatLongP;  
  StHbt1DHisto* mNumLongN;
  StHbt1DHisto* mDenLongN;  
  StHbt1DHisto* mRatLongN;
  StHbt1DHisto* mRatLong; 
  StHbt1DHisto* mRatLongNOverP;

  TProfile*     mProfOutP;
  TProfile*     mProfOutN;
  TProfile*     mProfSideP;
  TProfile*     mProfSideN;
  TProfile*     mProfLongP;
  TProfile*     mProfLongN;
  
  TProfile*     mProfDenOutP;
  TProfile*     mProfDenOutN;
  TProfile*     mProfDenSideP;
  TProfile*     mProfDenSideN;
  TProfile*     mProfDenLongP;
  TProfile*     mProfDenLongN;
  

  StHbt2DHisto* mHOutKSame;
  StHbt2DHisto* mHOutKDiff;
  StHbt2DHisto* mHSideKSame;
  StHbt2DHisto* mHSideKDiff;
  StHbt2DHisto* mHLongKSame;
  StHbt2DHisto* mHLongKDiff; 
  
  double mBtMin;
  double mBtMax;

  double mUtMin;
  double mUtMax;

#ifdef __ROOT__
ClassDef(ThNonId3DCF, 1)
#endif
};

#endif
