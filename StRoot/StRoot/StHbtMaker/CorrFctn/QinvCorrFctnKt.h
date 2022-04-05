/***************************************************************************
 *
 * $Id: QinvCorrFctnKt.h,v 1.1 2002/05/17 14:26:17 mercedes Exp $
 *
 * Author: Mercedes Lopez Noriega, OSU, mercedes@pacific.mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   QinvCFs for a given number of CFs (nCFs) between ktLo and ktHi 
 *   where kt is the four-momentum of the pair 
 *
 ***************************************************************************
 *
 * $Log: QinvCorrFctnKt.h,v $
 * Revision 1.1  2002/05/17 14:26:17  mercedes
 * N Qinv CFs (kt bins) between ktmin and ktmax
 *
 *
 **************************************************************************/

#ifndef QinvCorrFctnKt_hh
#define QinvCorrFctnKt_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
//#include "StHbtMaker/Infrastructure/StHbtHisto.hh"
#include "StHbtMaker/Base/StHbtPairCut.h"

class QinvCorrFctnKt : public StHbtCorrFctn {
public:
  QinvCorrFctnKt(char* title, const int& nbins, const float& QinvLo, const float& QinvHi,
		 const int& nCFs=20, const float& KtLo=0.0, const float& KtHi=1.0);
  virtual ~QinvCorrFctnKt();
  
  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);
  
  virtual void Finish();
  
  StHbt1DHisto* Numerator(int j);
  StHbt1DHisto* Denominator(int j);
  StHbt1DHisto* Ratio(int j);
  
 private:
  StHbt1DHisto* mNumerator;
  StHbt1DHisto* mDenominator;
  StHbt1DHisto* mRatio;

  int mNumberCFs;
  float mKtMin;
  float mKtMax;

  int* mIndex;
  float mDeltaKt;
  
#ifdef __ROOT__
  ClassDef(QinvCorrFctnKt, 1)
#endif
    };
    
inline  StHbt1DHisto* QinvCorrFctnKt::Numerator(int j){return &mNumerator[j];}
inline  StHbt1DHisto* QinvCorrFctnKt::Denominator(int j){return &mDenominator[j];}
inline  StHbt1DHisto* QinvCorrFctnKt::Ratio(int j){return &mRatio[j];}

#endif

