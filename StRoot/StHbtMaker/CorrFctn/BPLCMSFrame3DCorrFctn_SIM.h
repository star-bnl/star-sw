/***************************************************************************
 *
 * $Id: BPLCMSFrame3DCorrFctn_SIM.h,v 1.3 2001/05/23 00:19:05 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   This is a SIMULATION/TESTING CORRELATION FUNCTION CLASS!!
 *   This one does 3D Bertsch-Pratt decomposition in the LCMS frame
 *
 ***************************************************************************
 *
 * $Log: BPLCMSFrame3DCorrFctn_SIM.h,v $
 * Revision 1.3  2001/05/23 00:19:05  lisa
 * Add in Smearing classes and methods needed for momentum resolution studies and correction
 *
 * Revision 1.2  2000/10/05 23:08:59  lisa
 * Added kT-dependent radii to mixed-event simulator AND implemented AverageSeparation Cut and CorrFctn
 *
 * Revision 1.1  2000/09/14 18:36:53  lisa
 * Added Qinv and ExitSep pair cuts and BPLCMSFrame3DCorrFctn_SIM CorrFctn
 *
 *
 **************************************************************************/

#ifndef BPLCMSFrame3DCorrFctn_SIM_hh
#define BPLCMSFrame3DCorrFctn_SIM_hh

#include "StHbtMaker/Base/StHbtCorrFctn.hh"
#include "StHbtMaker/Infrastructure/StHbtCoulomb.h"
#include "StHbtMaker/Base/StHbtPairCut.h"
//#include "StHbtMaker/Infrastructure/StHbtHisto.hh"

class StHbtSmearPair;

class BPLCMSFrame3DCorrFctn_SIM : public StHbtCorrFctn {
public:
  BPLCMSFrame3DCorrFctn_SIM(char* title, const int& nbins, const float& QLo, const float& QHi);
  virtual ~BPLCMSFrame3DCorrFctn_SIM();

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);

  virtual void Finish();

  StHbt3DHisto* Numerator();
  StHbt3DHisto* Denominator();
  StHbt3DHisto* Ratio();

  StHbt2DHisto* ResolutionPlot(int index);

  // here are get and set for the range over which the correlation function 
  // is normalized (in Qinv).  The range is set to 0.15..0.18 in the constuctor
  // by default, but the Set's below override this
  void SetNormRangeLo(float qLo);
  void SetNormRangeHi(float qHi);
  float GetNormRangeLo();
  float GetNormRangeHi();

  void SetCoulombCorrection(StHbtCoulomb* Correction);

  void SetSpecificPairCut(StHbtPairCut*);

  void SetLambda(float lam);
  void SetRout(float Rout);
  void SetRside(float Rside);
  void SetRlong(float Rlong);

  void SetRoutAlpha(float alpha);
  void SetRsideAlpha(float alpha);
  void SetRlongAlpha(float alpha);


  void SetSmearPair(StHbtSmearPair* smearer);

private:
  StHbt3DHisto* mNumerator;
  StHbt3DHisto* mDenominator;
  StHbt3DHisto* mRatio;

  StHbtPairCut* mPairCut;    //! this is a PairCut specific to THIS CorrFctn, not the Analysis

  // upper and lower bounds of Qinv region where to do normalization
  float mQinvNormLo;
  float mQinvNormHi;

  // and here are the number of pairs in that region...
  unsigned long int mNumRealsNorm;
  unsigned long int mNumMixedNorm;

  StHbtCoulomb* mCorrection; //!

  float mLambda;
  float mRout2;
  float mRside2;
  float mRlong2;

  // following describe evolution in radii with mT
  // the evolution model is R = R(0)*mt^alpha
  // alpha is set zero by default, so if you don't change
  // them, then R = R(0) = what you input for all mT
  float mRout_alpha;
  float mRside_alpha;
  float mRlong_alpha;


  StHbtSmearPair* mSmearPair;  // for putting in detector resolution

  StHbt2DHisto* mResolutionHistos[4];

  bool mToggleNumDen; //!

#ifdef __ROOT__
  ClassDef(BPLCMSFrame3DCorrFctn_SIM, 0)
#endif
};

inline  StHbt2DHisto* BPLCMSFrame3DCorrFctn_SIM::ResolutionPlot(int index){return mResolutionHistos[index];}

inline  void BPLCMSFrame3DCorrFctn_SIM::SetSmearPair(StHbtSmearPair* smearer){mSmearPair = smearer;}

inline  StHbt3DHisto* BPLCMSFrame3DCorrFctn_SIM::Numerator(){return mNumerator;}
inline  StHbt3DHisto* BPLCMSFrame3DCorrFctn_SIM::Denominator(){return mDenominator;}
inline  StHbt3DHisto* BPLCMSFrame3DCorrFctn_SIM::Ratio(){return mRatio;}
inline  void BPLCMSFrame3DCorrFctn_SIM::SetNormRangeLo(float qLo){mQinvNormLo = qLo;}
inline  void BPLCMSFrame3DCorrFctn_SIM::SetNormRangeHi(float qHi){mQinvNormHi = qHi;}
inline  float BPLCMSFrame3DCorrFctn_SIM::GetNormRangeLo(){return mQinvNormLo;}
inline  float BPLCMSFrame3DCorrFctn_SIM::GetNormRangeHi(){return mQinvNormHi;}
inline  void BPLCMSFrame3DCorrFctn_SIM::SetCoulombCorrection(StHbtCoulomb* Correction){mCorrection = Correction;}
inline  void BPLCMSFrame3DCorrFctn_SIM::SetSpecificPairCut(StHbtPairCut* pc){mPairCut=pc;}

inline  void BPLCMSFrame3DCorrFctn_SIM::SetLambda(float lam){mLambda = lam;}
inline  void BPLCMSFrame3DCorrFctn_SIM::SetRout(float Rout){mRout2 = Rout*Rout;}
inline  void BPLCMSFrame3DCorrFctn_SIM::SetRside(float Rside){mRside2 = Rside*Rside;}
inline  void BPLCMSFrame3DCorrFctn_SIM::SetRlong(float Rlong){mRlong2 = Rlong*Rlong;}

inline  void BPLCMSFrame3DCorrFctn_SIM::SetRoutAlpha(float alpha){mRout_alpha = alpha;}
inline  void BPLCMSFrame3DCorrFctn_SIM::SetRsideAlpha(float alpha){mRside_alpha = alpha;}
inline  void BPLCMSFrame3DCorrFctn_SIM::SetRlongAlpha(float alpha){mRlong_alpha = alpha;}



#endif

