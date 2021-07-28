/***************************************************************************
 *
 * $Id: QinvCorrFctnPidProbWeight.h,v 1.1 2001/09/05 20:41:14 laue Exp $
 *
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a simple Q-invariant correlation function with probability weighting           
 *
 ***************************************************************************
 *
 * $Log: QinvCorrFctnPidProbWeight.h,v $
 * Revision 1.1  2001/09/05 20:41:14  laue
 * Updates of the hbtMuDstTree microDSTs
 *
 *
 **************************************************************************/

#ifndef QinvCorrFctnPidProbWeight_hh
#define QinvCorrFctnPidProbWeight_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
//#include "StHbtMaker/Infrastructure/StHbtHisto.hh"

class QinvCorrFctnPidProbWeight : public StHbtCorrFctn {
public:
  QinvCorrFctnPidProbWeight(char* title1, char* title2, const int& nbins, const float& QinvLo, const float& QinvHi);
  virtual ~QinvCorrFctnPidProbWeight();

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
  ClassDef(QinvCorrFctnPidProbWeight, 1)
#endif
};

inline  StHbt1DHisto* QinvCorrFctnPidProbWeight::Numerator(){return mNumerator;}
inline  StHbt1DHisto* QinvCorrFctnPidProbWeight::Denominator(){return mDenominator;}
inline  StHbt1DHisto* QinvCorrFctnPidProbWeight::Ratio(){return mRatio;}


#endif

