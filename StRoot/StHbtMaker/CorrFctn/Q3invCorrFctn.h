/***************************************************************************
 *
 * $Id: Q3invCorrFctn.h,v 1.2 2000/04/12 01:53:28 willson Exp $
 *
 * Author: Robert Willson, Ohio State, willson@bnl.gov 
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   A simple Q-invariant correlation function for three particle analyses.    
 *
 ***************************************************************************
 *
 * $Log: Q3invCorrFctn.h,v $
 * Revision 1.2  2000/04/12 01:53:28  willson
 * Initial Installation - Comments Added
 *
 * 
 ***************************************************************************/


#ifndef Q3invCorrFctn_hh
#define Q3invCorrFctn_hh

#include "StHbtMaker/Base/StHbtThreeParticleCorrFctn.hh"
//#include "StHbtMaker/Infrastructure/StHbtHisto.hh"

class Q3invCorrFctn : public StHbtThreeParticleCorrFctn {
public:
  Q3invCorrFctn(char* title, const int& nbins, const float& QinvLo, const float& QinvHi);
  virtual ~Q3invCorrFctn();

  virtual StHbtString Report();
  virtual void AddRealTriplet(const StHbtTriplet*);
  virtual void AddMixedTriplet(const StHbtTriplet*);

  virtual void Finish();

  StHbt1DHisto* Numerator();
  StHbt1DHisto* Denominator();
  StHbt1DHisto* Ratio();

private:
  StHbt1DHisto* mNumerator;
  StHbt1DHisto* mDenominator;
  StHbt1DHisto* mRatio;

  ClassDef(Q3invCorrFctn, 1)

};

inline  StHbt1DHisto* Q3invCorrFctn::Numerator(){return mNumerator;}
inline  StHbt1DHisto* Q3invCorrFctn::Denominator(){return mDenominator;}
inline  StHbt1DHisto* Q3invCorrFctn::Ratio(){return mRatio;}


#endif

