/***************************************************************************
 *
 * Author: Mercedes Lopez Noriega, OSU, mercedes@pacific.mps.ohio-state.edu
 *
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   2D correlation function: Qinv vs. Fraction of merged rows.
 *
 **************************************************************************/

#ifndef FracMergRowvsQinv_hh
#define FracMergRowvsQinv_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"

class FracMergRowvsQinv : public StHbtCorrFctn {
public:
  FracMergRowvsQinv(char* title, const int& nbinsX, const float& XLo, const float& XHi,
		      const int& nbinsY, const float& YLo, const float& YHi);
  virtual ~FracMergRowvsQinv();

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);

  virtual void Finish();

  StHbt2DHisto* Numerator2D();
  StHbt2DHisto* Denominator2D();
  StHbt2DHisto* Ratio2D();

private:
  StHbt2DHisto* mNumerator2D;
  StHbt2DHisto* mDenominator2D;
  StHbt2DHisto* mRatio2D;

#ifdef __ROOT__ 
  ClassDef(FracMergRowvsQinv, 1)
#endif
};

inline  StHbt2DHisto* FracMergRowvsQinv::Numerator2D(){return mNumerator2D;}
inline  StHbt2DHisto* FracMergRowvsQinv::Denominator2D(){return mDenominator2D;}
inline  StHbt2DHisto* FracMergRowvsQinv::Ratio2D(){return mRatio2D;}

#endif
