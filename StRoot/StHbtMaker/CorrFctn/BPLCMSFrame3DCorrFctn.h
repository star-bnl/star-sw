/***************************************************************************
 *
 * $Id: BPLCMSFrame3DCorrFctn.h,v 1.5 2002/06/07 22:51:39 lisa Exp $
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
 * Revision 1.5  2002/06/07 22:51:39  lisa
 * Widely used BPLCMSFrame3DCorrFctn class now accumulates UNcorrected denominator and has a WriteOutHistos method
 *
 * Revision 1.4  2001/05/23 00:19:04  lisa
 * Add in Smearing classes and methods needed for momentum resolution studies and correction
 *
 * Revision 1.3  2000/10/26 19:48:50  rcwells
 * Added functionality for Coulomb correction of <qInv> in 3D correltions
 *
 * Revision 1.2  2000/09/14 18:36:53  lisa
 * Added Qinv and ExitSep pair cuts and BPLCMSFrame3DCorrFctn_SIM CorrFctn
 *
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
#include "StHbtMaker/Base/StHbtPairCut.h"
//#include "StHbtMaker/Infrastructure/StHbtHisto.hh"

#include "StHbtMaker/Infrastructure/StHbtSmearPair.h"

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
  StHbt3DHisto* UncorrectedDenominator();
  StHbt3DHisto* Ratio();
  StHbt3DHisto* QinvHisto();

  // here are get and set for the range over which the correlation function 
  // is normalized (in Qinv).  The range is set to 0.15..0.18 in the constuctor
  // by default, but the Set's below override this
  void SetNormRangeLo(float qLo);
  void SetNormRangeHi(float qHi);
  float GetNormRangeLo();
  float GetNormRangeHi();

  void WriteOutHistos();

  void SetCoulombCorrection(StHbtCoulomb* Correction);

  void SetSpecificPairCut(StHbtPairCut*);

  void SetSmearPair(StHbtSmearPair*);
  void SetRout(double guess);
  void SetRside(double guess);
  void SetRlong(double guess);
  void SetLambda(double guess);


  // here are a whole bunch of histos that get filled if we do resolution correction
  StHbt3DHisto* mIDNumHisto;
  StHbt3DHisto* mIDDenHisto;
  StHbt3DHisto* mIDRatHisto;
  //
  StHbt3DHisto* mSMNumHisto;
  StHbt3DHisto* mSMDenHisto;
  StHbt3DHisto* mSMRatHisto;
  //
  StHbt3DHisto* mCorrectionHisto;
  StHbt3DHisto* mCorrCFHisto;




private:
  StHbt3DHisto* mNumerator;
  StHbt3DHisto* mDenominator;
  StHbt3DHisto* mUncorrectedDenominator;
  StHbt3DHisto* mRatio;
  StHbt3DHisto* mQinvHisto;

  // for resolution correction
  StHbtSmearPair* mSmearPair; //!
  double mLambda;
  double mRout2;
  double mRside2;
  double mRlong2;

  StHbtPairCut* mPairCut;    //! this is a PairCut specific to THIS CorrFctn, not the Analysis

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
inline  StHbt3DHisto* BPLCMSFrame3DCorrFctn::UncorrectedDenominator(){return mUncorrectedDenominator;}
inline  StHbt3DHisto* BPLCMSFrame3DCorrFctn::Ratio(){return mRatio;}
inline  StHbt3DHisto* BPLCMSFrame3DCorrFctn::QinvHisto(){return mQinvHisto;}
inline  void BPLCMSFrame3DCorrFctn::SetNormRangeLo(float qLo){mQinvNormLo = qLo;}
inline  void BPLCMSFrame3DCorrFctn::SetNormRangeHi(float qHi){mQinvNormHi = qHi;}
inline  float BPLCMSFrame3DCorrFctn::GetNormRangeLo(){return mQinvNormLo;}
inline  float BPLCMSFrame3DCorrFctn::GetNormRangeHi(){return mQinvNormHi;}
inline  void BPLCMSFrame3DCorrFctn::SetCoulombCorrection(StHbtCoulomb* Correction){mCorrection = Correction;}
inline  void BPLCMSFrame3DCorrFctn::SetSpecificPairCut(StHbtPairCut* pc){mPairCut=pc;}
inline  void BPLCMSFrame3DCorrFctn::SetSmearPair(StHbtSmearPair* sp){mSmearPair = sp;}

inline  void BPLCMSFrame3DCorrFctn::SetRout(double r){mRout2 = r*r;}
inline  void BPLCMSFrame3DCorrFctn::SetRside(double r){mRside2 = r*r;}
inline  void BPLCMSFrame3DCorrFctn::SetRlong(double r){mRlong2 = r*r;}
inline  void BPLCMSFrame3DCorrFctn::SetLambda(double l){mLambda = l;}

#endif

