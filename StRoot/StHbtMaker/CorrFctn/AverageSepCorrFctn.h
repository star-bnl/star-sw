/***************************************************************************
 *
 * $Id: AverageSepCorrFctn.h,v 1.1 2000/10/05 23:08:59 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a simple average-seperation correlation function
 *   for studying 2-track cuts...
 *
 ***************************************************************************
 *
 *$Log: AverageSepCorrFctn.h,v $
 *Revision 1.1  2000/10/05 23:08:59  lisa
 *Added kT-dependent radii to mixed-event simulator AND implemented AverageSeparation Cut and CorrFctn
 *
 *
 **************************************************************************/

#ifndef AverageSepCorrFctn_hh
#define AverageSepCorrFctn_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"

class AverageSepCorrFctn : public StHbtCorrFctn {
public:
  AverageSepCorrFctn(char* title, const int& nbinsQ, const float& QLo, const float& QHi,
		       const int& nbinExSep, const float& ExSepLo, const float& ExSepHi);
  virtual ~AverageSepCorrFctn();

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);

  virtual void Finish();

  StHbt2DHisto* Numerator2D();
  StHbt2DHisto* Denominator2D();
  StHbt2DHisto* Ratio2D();

private:

  StHbt2DHisto* mNumerator2D;
  StHbt2DHisto* mDenominator2D;
  StHbt2DHisto* mRatio2D;

#ifdef __ROOT__
  ClassDef(AverageSepCorrFctn, 1)
#endif

};

inline  StHbt2DHisto* AverageSepCorrFctn::Numerator2D(){return mNumerator2D;}
inline  StHbt2DHisto* AverageSepCorrFctn::Denominator2D(){return mDenominator2D;}
inline  StHbt2DHisto* AverageSepCorrFctn::Ratio2D(){return mRatio2D;}


#endif

