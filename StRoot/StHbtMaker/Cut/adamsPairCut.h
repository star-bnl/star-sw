/***************************************************************************
 *
 * $Id: adamsPairCut.h,v 1.1 2002/12/12 17:03:51 kisiel Exp $
 *
 * Author: KAdam Kisiel, Warsaw University of Technoogy, kisiel@if.pw.edu.pl
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   Cuts on probability of a pair being e+e-, pi+pi- or K+K-
 *   using dEdx NSigma PID information
 *
 ***************************************************************************
 *
 *
 **************************************************************************/


#ifndef adamsPairCut_hh
#define adamsPairCut_hh

// do I need these lines ?
//#ifndef StMaker_H
//#include "StMaker.h"
//#endif

#include "Cut/HitMergingPairCut.h"
class ostrstream;

class adamsPairCut : public HitMergingPairCut{
public:
  adamsPairCut();
  adamsPairCut(const adamsPairCut&);
  //~adamsPairCut();

  virtual bool Pass(const StHbtPair*);
  virtual StHbtString Report();
  adamsPairCut* Clone();
  void setElSigma (double aElSigma);
  void setPiSigma (double aPiSigma);
  void setKSigma  (double aKSigma);
  void setElPairPIDMax (double aElPIDMax);
  void setPiPairPIDMax (double aPiPIDMax);
  void setKPairPIDMax  (double aKPIDMax);
  void SetPIDPThreshold(const float&);
  ostrstream* finalReport() const;

private:
  double mElSigma;
  double mPiSigma;
  double mKSigma;

  double mElPIDMax;
  double mPiPIDMax;
  double mKPIDMax;

  float             mPIDPThreshold;
#ifdef __ROOT__
  ClassDef(adamsPairCut, 1)
#endif
};

inline adamsPairCut::adamsPairCut(const adamsPairCut& c) : HitMergingPairCut(c) {
  mNPairsPassed = 0;
  mNPairsFailed = 0;

}
inline adamsPairCut* adamsPairCut::Clone() { adamsPairCut* c = new adamsPairCut(*this); return c;}

inline void adamsPairCut::setElSigma(double aElSigma){
  mElSigma = aElSigma;
}
inline void adamsPairCut::setPiSigma(double aPiSigma){
  mPiSigma = aPiSigma;
}
inline void adamsPairCut::setKSigma(double aKSigma){
  mKSigma = aKSigma;
}
inline void adamsPairCut::setElPairPIDMax(double aElPIDMax){
  mElPIDMax = aElPIDMax;
}
inline void adamsPairCut::setPiPairPIDMax(double aPiPIDMax){
  mPiPIDMax = aPiPIDMax;
}
inline void adamsPairCut::setKPairPIDMax(double aKPIDMax){
  mKPIDMax = aKPIDMax;
}
inline void adamsPairCut::SetPIDPThreshold(const float& pidpt){mPIDPThreshold = pidpt;}
#endif
