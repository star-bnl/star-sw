/***************************************************************************
 *
 * $Id: QinvCorrFctn.h,v 1.3 2000/01/25 17:34:45 laue Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a simple Q-invariant correlation function           
 *
 ***************************************************************************
 *
 * $Log: QinvCorrFctn.h,v $
 * Revision 1.3  2000/01/25 17:34:45  laue
 * I. In order to run the stand alone version of the StHbtMaker the following
 * changes have been done:
 * a) all ClassDefs and ClassImps have been put into #ifdef __ROOT__ statements
 * b) unnecessary includes of StMaker.h have been removed
 * c) the subdirectory StHbtMaker/doc/Make has been created including everything
 * needed for the stand alone version
 *
 * II. To reduce the amount of compiler warning
 * a) some variables have been type casted
 * b) some destructors have been declared as virtual
 *
 * Revision 1.2  1999/07/06 22:33:20  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef QinvCorrFctn_hh
#define QinvCorrFctn_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
//#include "StHbtMaker/Infrastructure/StHbtHisto.hh"

class QinvCorrFctn : public StHbtCorrFctn {
public:
  QinvCorrFctn(char* title, const int& nbins, const float& QinvLo, const float& QinvHi);
  virtual ~QinvCorrFctn();

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);

  virtual void Finish();

  StHbt1DHisto* Numerator();
  StHbt1DHisto* Denominator();
  StHbt1DHisto* Ratio();

private:
  StHbt1DHisto* mNumerator;
  StHbt1DHisto* mDenominator;
  StHbt1DHisto* mRatio;

#ifdef __ROOT__
  ClassDef(QinvCorrFctn, 1)
#endif
};

inline  StHbt1DHisto* QinvCorrFctn::Numerator(){return mNumerator;}
inline  StHbt1DHisto* QinvCorrFctn::Denominator(){return mDenominator;}
inline  StHbt1DHisto* QinvCorrFctn::Ratio(){return mRatio;}


#endif

