/***************************************************************************
 *
 * $Id: BPLCMSFrame3DCorrFctn.h,v 1.1 2000/08/17 20:48:39 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   This one does 3D Bertsch-Pratt decomposition in the LCMS frame
 *
 ***************************************************************************
 *
 * $Log: BPLCMSFrame3DCorrFctn.h,v $
 * Revision 1.1  2000/08/17 20:48:39  lisa
 * Adding correlationfunction in LCMS frame
 *
 *
 *
 **************************************************************************/

#ifndef BPLCMSFrame3DCorrFctn_hh
#define BPLCMSFrame3DCorrFctn_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#include "StHbtMaker/Infrastructure/StHbtCoulomb.h"
//#include "StHbtMaker/Infrastructure/StHbtHisto.hh"

class BPLCMSFrame3DCorrFctn : public StHbtCorrFctn {
public:
  BPLCMSFrame3DCorrFctn(char* title, const int& nbins, const float& QLo, const float& QHi);
  virtual ~BPLCMSFrame3DCorrFctn();

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);

  virtual void Finish();

  StHbt3DHisto* Numerator();
  StHbt3DHisto* Denominator();
  StHbt3DHisto* Ratio();


  // here are get and set for the range over which the correlation function 
  // is normalized (in Qinv).  The range is set to 0.15..0.18 in the constuctor
  // by default, but the Set's below override this
  void SetNormRangeLo(float qLo);
  void SetNormRangeHi(float qHi);
  float GetNormRangeLo();
  float GetNormRangeHi();

  void SetCoulombCorrection(StHbtCoulomb* Correction);


private:
  StHbt3DHisto* mNumerator;
  StHbt3DHisto* mDenominator;
  StHbt3DHisto* mRatio;

  // upper and lower bounds of Qinv region where to do normalization
  float mQinvNormLo;
  float mQinvNormHi;

  // and here are the number of pairs in that region...
  unsigned long int mNumRealsNorm;
  unsigned long int mNumMixedNorm;

  StHbtCoulomb* mCorrection; //!


#ifdef __ROOT__
  ClassDef(BPLCMSFrame3DCorrFctn, 1)
#endif
};

inline  StHbt3DHisto* BPLCMSFrame3DCorrFctn::Numerator(){return mNumerator;}
inline  StHbt3DHisto* BPLCMSFrame3DCorrFctn::Denominator(){return mDenominator;}
inline  StHbt3DHisto* BPLCMSFrame3DCorrFctn::Ratio(){return mRatio;}
inline  void BPLCMSFrame3DCorrFctn::SetNormRangeLo(float qLo){mQinvNormLo = qLo;}
inline  void BPLCMSFrame3DCorrFctn::SetNormRangeHi(float qHi){mQinvNormHi = qHi;}
inline  float BPLCMSFrame3DCorrFctn::GetNormRangeLo(){return mQinvNormLo;}
inline  float BPLCMSFrame3DCorrFctn::GetNormRangeHi(){return mQinvNormHi;}
inline  void BPLCMSFrame3DCorrFctn::SetCoulombCorrection(StHbtCoulomb* Correction){mCorrection = Correction;}

#endif

