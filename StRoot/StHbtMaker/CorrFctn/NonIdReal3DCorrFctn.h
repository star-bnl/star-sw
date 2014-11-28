/***************************************************************************
 *
 * $Id: NonIdReal3DCorrFctn.h,v 1.1 2002/12/12 17:02:49 kisiel Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   This one does 3D Bertsch-Pratt decomposition in the Lab (STAR c.m.) frame
 *
 ***************************************************************************
 *
 * $Log: NonIdReal3DCorrFctn.h,v $
 * Revision 1.1  2002/12/12 17:02:49  kisiel
 * Use KStar instead of 2*KStar for non-identical particles
 *
 * Revision 1.3  2000/10/26 19:48:50  rcwells
 * Added functionality for Coulomb correction of <qInv> in 3D correltions
 *
 * Revision 1.2  2000/08/02 01:25:10  lisa
 * Add Coulomb correction capability to 3D Bertsch-Pratt CorrFctn
 *
 * Revision 1.1  2000/07/31 01:19:23  lisa
 * add PairCut which contains collection of PairCuts - also 3D bertsch-pratt CorrFctn
 *
 *
 **************************************************************************/

#ifndef NonIdReal3DCorrFctn_hh
#define NonIdReal3DCorrFctn_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#include "StHbtMaker/Infrastructure/StHbtCoulomb.h"
//#include "StHbtMaker/Infrastructure/StHbtHisto.hh"

class NonIdReal3DCorrFctn : public StHbtCorrFctn {
public:
  NonIdReal3DCorrFctn(char* title, const int& nbins, const float& QLo, const float& QHi);
  virtual ~NonIdReal3DCorrFctn();

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);

  virtual void Finish();

  void Write();
  
  StHbt3DHisto* Numerator();
  StHbt3DHisto* Denominator();
  StHbt3DHisto* Ratio();
  StHbt3DHisto* QinvHisto();

  // here are get and set for the range over which the correlation function 
  // is normalized (in Qinv).  The range is set to 0.15..0.18 in the constuctor
  // by default, but the Set's below override this
  void SetNormRangeLo(float qLo);
  void SetNormRangeHi(float qHi);
  float GetNormRangeLo();
  float GetNormRangeHi();

private:
  StHbt3DHisto* mNumerator;
  StHbt3DHisto* mDenominator;
  StHbt3DHisto* mRatio;
  StHbt3DHisto* mQinvHisto;

  // upper and lower bounds of Qinv region where to do normalization
  float mQinvNormLo;
  float mQinvNormHi;

  // and here are the number of pairs in that region...
  unsigned long int mNumRealsNorm;
  unsigned long int mNumMixedNorm;

#ifdef __ROOT__
  ClassDef(NonIdReal3DCorrFctn, 1)
#endif
};

inline  StHbt3DHisto* NonIdReal3DCorrFctn::Numerator(){return mNumerator;}
inline  StHbt3DHisto* NonIdReal3DCorrFctn::Denominator(){return mDenominator;}
inline  StHbt3DHisto* NonIdReal3DCorrFctn::Ratio(){return mRatio;}
inline  StHbt3DHisto* NonIdReal3DCorrFctn::QinvHisto(){return mQinvHisto;}
inline  void NonIdReal3DCorrFctn::SetNormRangeLo(float qLo){mQinvNormLo = qLo;}
inline  void NonIdReal3DCorrFctn::SetNormRangeHi(float qHi){mQinvNormHi = qHi;}
inline  float NonIdReal3DCorrFctn::GetNormRangeLo(){return mQinvNormLo;}
inline  float NonIdReal3DCorrFctn::GetNormRangeHi(){return mQinvNormHi;}

#endif

