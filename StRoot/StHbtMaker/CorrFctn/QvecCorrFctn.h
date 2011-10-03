/***************************************************************************
 *
 * $Id: QvecCorrFctn.h,v 1.1.1.1 1999/06/29 16:02:57 lisa Exp $
 *
 * Author: Randy Wells, Ohio State, rcwells@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *      a simple correlation function in the magnitude of 3-vector q        
 *
 ***************************************************************************
 *
 * $Log: QvecCorrFctn.h,v $
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef QvecCorrFctn_hh
#define QvecCorrFctn_hh
#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#include "StHbtMaker/Infrastructure/StHbtHisto.hh"

class QvecCorrFctn : public StHbtCorrFctn {
public:
  QvecCorrFctn(char* title, const int& nbins, const float& QinvLo, const float& QinvHi);
  virtual ~QvecCorrFctn();

  virtual string Report();
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

  ClassDef(QvecCorrFctn, 1)

};

inline  StHbt1DHisto* QvecCorrFctn::Numerator(){return mNumerator;}
inline  StHbt1DHisto* QvecCorrFctn::Denominator(){return mDenominator;}
inline  StHbt1DHisto* QvecCorrFctn::Ratio(){return mRatio;}


#endif

