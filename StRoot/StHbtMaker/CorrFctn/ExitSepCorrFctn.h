/***************************************************************************
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a simple exit-seperation correlation function
 *   for studying 2-track cuts...
 *
 ***************************************************************************
 *
 *
 **************************************************************************/

#ifndef ExitSepCorrFctn_hh
#define ExitSepCorrFctn_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"

class ExitSepCorrFctn : public StHbtCorrFctn {
public:
  ExitSepCorrFctn(char* title, const int& nbinsQ, const float& QLo, const float& QHi,
		       const int& nbinExSep, const float& ExSepLo, const float& ExSepHi);
  virtual ~ExitSepCorrFctn();

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
  ClassDef(ExitSepCorrFctn, 1)
#endif

};

inline  StHbt2DHisto* ExitSepCorrFctn::Numerator2D(){return mNumerator2D;}
inline  StHbt2DHisto* ExitSepCorrFctn::Denominator2D(){return mDenominator2D;}
inline  StHbt2DHisto* ExitSepCorrFctn::Ratio2D(){return mRatio2D;}


#endif

