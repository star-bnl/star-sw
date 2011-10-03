/***************************************************************************
 *
 * $Id: MinvCorrFctn.h,v 1.1.1.1 1999/06/29 16:02:57 lisa Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *     A simple invariant-mass correlation function
 *
 ***************************************************************************
 *
 * $Log: MinvCorrFctn.h,v $
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef MinvCorrFctn_hh
#define MinvCorrFctn_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#include "StHbtMaker/Infrastructure/StHbtHisto.hh"

class MinvCorrFctn : public StHbtCorrFctn {
public:
  MinvCorrFctn(char* title, const int& nbins, const float& MinvLo, const float& MinvHi);
  virtual ~MinvCorrFctn();

  virtual string Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);

  virtual void Finish();

  StHbt1DHisto* Numerator();
  StHbt1DHisto* Denominator();
  StHbt1DHisto* Difference();

private:
  StHbt1DHisto* mNumerator;
  StHbt1DHisto* mDenominator;
  StHbt1DHisto* mDifference;

  ClassDef(MinvCorrFctn, 1)

};

inline  StHbt1DHisto* MinvCorrFctn::Numerator(){return mNumerator;}
inline  StHbt1DHisto* MinvCorrFctn::Denominator(){return mDenominator;}
inline  StHbt1DHisto* MinvCorrFctn::Difference(){return mDifference;}


#endif

