/***************************************************************************
 *
 * $Id: BPLCMSFrame3DCorrFctnKt.h,v 1.1 2002/05/17 14:25:17 mercedes Exp $
 *
 * Author: Mercedes Lopez Noriega, OSU, mercedes@pacific.mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   This one does 3D Bertsch-Pratt decomposition in the LCMS frame,
 *   for kt between ktLo and ktHi max for a given number of bins (nCFs)
 *   kt is the transverse four-momentum of the pair
 *
 ***************************************************************************
 *
 * $Log: BPLCMSFrame3DCorrFctnKt.h,v $
 * Revision 1.1  2002/05/17 14:25:17  mercedes
 * N 3D Bertsch-Pratt decomposition (kt bins) between ktmin and ktmax (LCMSFrame)
 *
 *
 **************************************************************************/

#ifndef BPLCMSFrame3DCorrFctnKt_hh
#define BPLCMSFrame3DCorrFctnKt_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#include "StHbtMaker/Base/StHbtPairCut.h"
#include "StHbtMaker/Infrastructure/StHbtCoulomb.h"

class BPLCMSFrame3DCorrFctnKt : public StHbtCorrFctn {
public:
  BPLCMSFrame3DCorrFctnKt(char* title, const int& nbins, const float& QLo, const float& QHi,
			  const int& nCFs=20, const float& KtLo=0.0, const float& KtHi=1.0);
  virtual ~BPLCMSFrame3DCorrFctnKt();

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);

  virtual void Finish();

  StHbt3DHisto* Numerator(int j);
  StHbt3DHisto* Denominator(int j);
  StHbt3DHisto* Ratio(int j);
  StHbt3DHisto* QinvHisto(int j);

  // here are get and set for the range over which the correlation function 
  // is normalized (in Qinv).  The range is set to 0.15..0.18 in the constuctor
  // by default, but the Set's below override this
  void SetNormRangeLo(float qLo);
  void SetNormRangeHi(float qHi);
  float GetNormRangeLo();
  float GetNormRangeHi();

  void SetCoulombCorrection(StHbtCoulomb* Correction);

private:
  StHbt3DHisto* mNumerator;
  StHbt3DHisto* mDenominator;
  StHbt3DHisto* mRatio;
  StHbt3DHisto* mQinvHisto;

  //for the kt binning:
  int mNumberCFs;
  float mKtMin;
  float mKtMax;

  int* mIndex;
  float mDeltaKt;

  // upper and lower bounds of Qinv region where to do normalization
  float mQinvNormLo;
  float mQinvNormHi;

  // and here are the number of pairs in that region...
  unsigned long int mNumRealsNorm;
  unsigned long int mNumMixedNorm;

  StHbtCoulomb* mCorrection; //!

#ifdef __ROOT__
  ClassDef(BPLCMSFrame3DCorrFctnKt, 1)
#endif
};

inline  StHbt3DHisto* BPLCMSFrame3DCorrFctnKt::Numerator(int j){return &mNumerator[j];}
inline  StHbt3DHisto* BPLCMSFrame3DCorrFctnKt::Denominator(int j){return &mDenominator[j];}
inline  StHbt3DHisto* BPLCMSFrame3DCorrFctnKt::Ratio(int j){return &mRatio[j];}
inline  StHbt3DHisto* BPLCMSFrame3DCorrFctnKt::QinvHisto(int j){return &mQinvHisto[j];}
inline  void BPLCMSFrame3DCorrFctnKt::SetNormRangeLo(float qLo){mQinvNormLo = qLo;}
inline  void BPLCMSFrame3DCorrFctnKt::SetNormRangeHi(float qHi){mQinvNormHi = qHi;}
inline  float BPLCMSFrame3DCorrFctnKt::GetNormRangeLo(){return mQinvNormLo;}
inline  float BPLCMSFrame3DCorrFctnKt::GetNormRangeHi(){return mQinvNormHi;}
inline  void BPLCMSFrame3DCorrFctnKt::SetCoulombCorrection(StHbtCoulomb* Correction){mCorrection = Correction;}

#endif

