/***************************************************************************
 *
 * $Id: MinvCorrFctnArmenteros.h,v 1.1 2000/02/28 14:31:52 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *     A simple invariant-mass correlation function
 *
 ***************************************************************************
 *
 * $Log: MinvCorrFctnArmenteros.h,v $
 * Revision 1.1  2000/02/28 14:31:52  laue
 * Correlation function to make the Armenteros-Podolanski plot.
 *
 * Revision 1.2  1999/07/06 22:33:20  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef MinvCorrFctnArmenteros_hh
#define MinvCorrFctnArmenteros_hh

#include "StHbtMaker/Infrastructure/StHbtAnalysis.h"
#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#include "StHbtMaker/Cut/mikesEventCut.h"

class MinvCorrFctnArmenteros : public StHbtCorrFctn {
public:
  MinvCorrFctnArmenteros(char* title, 
		      const int& nbins1, const float& MinvLo1, const float& MinvHi1,
		      const int& nbins2, const float& MinvLo2, const float& MinvHi2);
  virtual ~MinvCorrFctnArmenteros();

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);

  virtual void Finish();

  void SetMassWindow( double, double );
  
  StHbt2DHisto* Numerator();
  StHbt2DHisto* Denominator();
  StHbt2DHisto* Difference();

private:
  double mLo,mHi;
  long mRealPairs, mMixedPairs;
  StHbt2DHisto* mNumerator;
  StHbt2DHisto* mDenominator;
  StHbt2DHisto* mDifference;
#ifdef __ROOT__
  ClassDef(MinvCorrFctnArmenteros, 1)
#endif
};

inline  StHbt2DHisto* MinvCorrFctnArmenteros::Numerator(){return mNumerator;}
inline  StHbt2DHisto* MinvCorrFctnArmenteros::Denominator(){return mDenominator;}
inline  StHbt2DHisto* MinvCorrFctnArmenteros::Difference(){return mDifference;}

#endif

