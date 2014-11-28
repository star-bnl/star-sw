/***************************************************************************
 *
 * $Id: MinvCorrFctnY_vs_Pt.h,v 1.2 2000/03/16 01:56:36 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *     A simple invariant-mass correlation function
 *
 ***************************************************************************
 *
 * $Log: MinvCorrFctnY_vs_Pt.h,v $
 * Revision 1.2  2000/03/16 01:56:36  laue
 * Copy constructor added to some correlation functions
 *
 * Revision 1.1  2000/02/28 14:39:31  laue
 * Correlation function to fill phasespace rapidity vs pt
 *
 * Revision 1.2  1999/07/06 22:33:20  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef MinvCorrFctnY_vs_Pt_hh
#define MinvCorrFctnY_vs_Pt_hh

#include "StHbtMaker/Infrastructure/StHbtAnalysis.h"
#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#include "StHbtMaker/Cut/mikesEventCut.h"

class MinvCorrFctnY_vs_Pt : public StHbtCorrFctn {
public:
  MinvCorrFctnY_vs_Pt(char* title, 
		      const int& nbins1, const float& MinvLo1, const float& MinvHi1,
		      const int& nbins2, const float& MinvLo2, const float& MinvHi2);
  MinvCorrFctnY_vs_Pt(const MinvCorrFctnY_vs_Pt&);
  virtual ~MinvCorrFctnY_vs_Pt();

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);

  virtual void Finish();

  StHbt2DHisto* Numerator();
  StHbt2DHisto* Denominator();
  StHbt2DHisto* Difference();

private:
  double mRealPairs,mMixedPairs;
  StHbt2DHisto* mNumerator;
  StHbt2DHisto* mDenominator;
  StHbt2DHisto* mDifference;

#ifdef __ROOT__
  ClassDef(MinvCorrFctnY_vs_Pt, 1)
#endif
};
 
inline  StHbt2DHisto* MinvCorrFctnY_vs_Pt::Numerator(){return mNumerator;}
inline  StHbt2DHisto* MinvCorrFctnY_vs_Pt::Denominator(){return mDenominator;}
inline  StHbt2DHisto* MinvCorrFctnY_vs_Pt::Difference(){return mDifference;}
inline MinvCorrFctnY_vs_Pt::MinvCorrFctnY_vs_Pt(const MinvCorrFctnY_vs_Pt& fctn) :StHbtCorrFctn() {
  mNumerator =  new StHbt2DHisto(*(fctn.mNumerator));
  mDenominator= new StHbt2DHisto(*(fctn.mDenominator));
  mDifference = new StHbt2DHisto(*(fctn.mDifference));
}

#endif

