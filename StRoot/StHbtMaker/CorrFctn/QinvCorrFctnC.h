/***************************************************************************
 *
 * $Id: QinvCorrFctnC.h,v 1.2 2000/01/25 17:34:45 laue Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a simple Q-invariant correlation function           
 *
 ***************************************************************************
 *
 * $Log: QinvCorrFctnC.h,v $
 * Revision 1.2  2000/01/25 17:34:45  laue
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
 * Revision 1.1  1999/09/23 23:28:02  lisa
 * add helensV0Cut  AND  rename mikes and franks ParticleCuts to TrackCuts  AND  update documentation
 *
 * Revision 1.2  1999/07/06 22:33:20  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef QinvCorrFctnC_hh
#define QinvCorrFctnC_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#include "StHbtMaker/Infrastructure/StHbtCoulomb.h"

class QinvCorrFctnC : public StHbtCorrFctn {
public:
  QinvCorrFctnC(char* title, const int& nbins, const float& QinvLo, const float& QinvHi);
  virtual ~QinvCorrFctnC();

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);
  void AddCorrection(const StHbtCoulomb*);

  virtual void Finish();

  StHbt1DHisto* Numerator();
  StHbt1DHisto* Denominator();
  StHbt1DHisto* Ratio();

private:
  StHbt1DHisto* mNumerator;
  StHbt1DHisto* mDenominator;
  StHbt1DHisto* mRatio;
  StHbtCoulomb mCorrection;    // Didn't like a pointer?????

#ifdef __ROOT__ 
  ClassDef(QinvCorrFctnC, 1)
#endif
};

inline  StHbt1DHisto* QinvCorrFctnC::Numerator(){return mNumerator;}
inline  StHbt1DHisto* QinvCorrFctnC::Denominator(){return mDenominator;}
inline  StHbt1DHisto* QinvCorrFctnC::Ratio(){return mRatio;}


#endif

