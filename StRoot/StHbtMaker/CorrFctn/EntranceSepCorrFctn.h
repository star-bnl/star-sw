/***************************************************************************
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a simple entrance-seperation correlation function
 *   for studying 2-track cuts...
 *
 ***************************************************************************
 *
 *
 **************************************************************************/

#ifndef EntranceSepCorrFctn_hh
#define EntranceSepCorrFctn_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"

class EntranceSepCorrFctn : public StHbtCorrFctn {
public:
  EntranceSepCorrFctn(char* title, const int& nbinsQ, const float& QLo, const float& QHi,
		       const int& nbinExSep, const float& ExSepLo, const float& ExSepHi);
  virtual ~EntranceSepCorrFctn();

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
  ClassDef(EntranceSepCorrFctn, 1)
#endif

};

inline  StHbt2DHisto* EntranceSepCorrFctn::Numerator2D(){return mNumerator2D;}
inline  StHbt2DHisto* EntranceSepCorrFctn::Denominator2D(){return mDenominator2D;}
inline  StHbt2DHisto* EntranceSepCorrFctn::Ratio2D(){return mRatio2D;}


#endif

