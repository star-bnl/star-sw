/***************************************************************************
 *
 * $Id: Q3invCorrFctn.h,v 1.5 2001/06/03 21:05:31 willson Exp $
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
 * Revision 1.5  2001/06/03 21:05:31  willson
 * Bins in entrance separation
 *
 * Revision 1.3  2000/05/11 21:17:30  willson
 * Modified CorrFctn class
 *
 * Revision 1.2  2000/04/12 01:53:28  willson
 * Initial Installation - Comments Added
 *
 * 
 ***************************************************************************/


#ifndef Q3invCorrFctn_hh
#define Q3invCorrFctn_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#include "StHbtMaker/Infrastructure/StHbtCoulomb.h"
//#include "StHbtMaker/Infrastructure/StHbtHisto.hh"

class Q3invCorrFctn : public StHbtCorrFctn {
public:
  Q3invCorrFctn(char* title, const int& nbinsQ, const float& QinvLo, const float& QinvHi, const int& nbinsMerge, const float& MergeLo, const float& MergeHi, const float& Split);
  virtual ~Q3invCorrFctn();

  virtual StHbtString Report();
  virtual void AddRealTriplet(const StHbtTriplet*);
  virtual void AddMixedTriplet(const StHbtTriplet*);

  void AddCorrection(StHbtCoulomb*);
  void AddPHisto(TH1D*);

  virtual void Finish();

  StHbt2DHisto* Numerator();
  StHbt2DHisto* Denominator();
  StHbt2DHisto* Ratio();
  
private:
  StHbt2DHisto* mNumerator;
  StHbt2DHisto* mDenominator;
  StHbt2DHisto* mRatio;
  TH1D* mPHist;

  ClassDef(Q3invCorrFctn, 1)

  float mSplit;
  StHbtCoulomb mCorrection;

};

inline  StHbt2DHisto* Q3invCorrFctn::Numerator(){return mNumerator;}
inline  StHbt2DHisto* Q3invCorrFctn::Denominator(){return mDenominator;}
inline  StHbt2DHisto* Q3invCorrFctn::Ratio(){return mRatio;}
inline  void Q3invCorrFctn::AddPHisto(TH1D *x){mPHist = x;}

#endif

