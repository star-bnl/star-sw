/***************************************************************************
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a simple opening-angle correlation function
 *   for studying 2-track cuts...
 *
 ***************************************************************************
 *
 *
 **************************************************************************/

#ifndef OpeningAngleCorrFctn_hh
#define OpeningAngleCorrFctn_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"

class OpeningAngleCorrFctn : public StHbtCorrFctn {
public:
  OpeningAngleCorrFctn(char* title, const int& nbinsQ, const float& QLo, const float& QHi,
		       const int& nbinAng, const float& AngLo, const float& AngHi);
  virtual ~OpeningAngleCorrFctn();

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
  ClassDef(OpeningAngleCorrFctn, 1)
#endif
};

inline  StHbt2DHisto* OpeningAngleCorrFctn::Numerator2D(){return mNumerator2D;}
inline  StHbt2DHisto* OpeningAngleCorrFctn::Denominator2D(){return mDenominator2D;}
inline  StHbt2DHisto* OpeningAngleCorrFctn::Ratio2D(){return mRatio2D;}


#endif

