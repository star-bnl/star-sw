/***************************************************************************
 *
 * $Id: QinvCorrFctnC.h,v 1.1 1999/09/23 23:28:02 lisa Exp $
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

  ClassDef(QinvCorrFctnC, 1)

};

inline  StHbt1DHisto* QinvCorrFctnC::Numerator(){return mNumerator;}
inline  StHbt1DHisto* QinvCorrFctnC::Denominator(){return mDenominator;}
inline  StHbt1DHisto* QinvCorrFctnC::Ratio(){return mRatio;}


#endif

