/***************************************************************************
 *
 * $Id: QinvCorrFctn.h,v 1.1.1.1 1999/06/29 16:02:57 lisa Exp $
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
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef QinvCorrFctn_hh
#define QinvCorrFctn_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#include "StHbtMaker/Infrastructure/StHbtHisto.hh"

class QinvCorrFctn : public StHbtCorrFctn {
public:
  QinvCorrFctn(char* title, const int& nbins, const float& QinvLo, const float& QinvHi);
  virtual ~QinvCorrFctn();

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

  ClassDef(QinvCorrFctn, 1)

};

inline  StHbt1DHisto* QinvCorrFctn::Numerator(){return mNumerator;}
inline  StHbt1DHisto* QinvCorrFctn::Denominator(){return mDenominator;}
inline  StHbt1DHisto* QinvCorrFctn::Ratio(){return mRatio;}


#endif

