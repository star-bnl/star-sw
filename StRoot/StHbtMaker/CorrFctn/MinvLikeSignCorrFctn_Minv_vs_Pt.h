/***************************************************************************
 *
 * $Id: MinvLikeSignCorrFctn_Minv_vs_Pt.h,v 1.2 2001/01/23 15:12:55 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *     A simple invariant-mass correlation function
 *
 ***************************************************************************
 *
 **************************************************************************/

#ifndef MinvLikeSignCorrFctn_Minv_vs_Pt_hh
#define MinvLikeSignCorrFctn_Minv_vs_Pt_hh

#include "StHbtMaker/Infrastructure/StHbtAnalysis.h"
#include "StHbtMaker/Base/StHbtLikeSignCorrFctn.hh"
#include "StHbtMaker/Cut/mikesEventCut.h"

class MinvLikeSignCorrFctn_Minv_vs_Pt : public StHbtLikeSignCorrFctn {
public:
  MinvLikeSignCorrFctn_Minv_vs_Pt(const MinvLikeSignCorrFctn_Minv_vs_Pt& ); // copy constructor
  MinvLikeSignCorrFctn_Minv_vs_Pt(char* title1, char* title2, 
				  const int& nxbins, const float& xLo, const float& xHi,
				  const int& nybins, const float& yLo, const float& yHi);
  virtual ~MinvLikeSignCorrFctn_Minv_vs_Pt();

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);
  virtual void AddLikeSignPositivePair(const StHbtPair*);
  virtual void AddLikeSignNegativePair(const StHbtPair*);
  virtual void Finish();
  MinvLikeSignCorrFctn_Minv_vs_Pt* Clone();

  StHbt2DHisto* Numerator();
  StHbt2DHisto* MixedEventDenominator();
  StHbt2DHisto* PositiveDenominator();
  StHbt2DHisto* NegativeDenominator();
  StHbt2DHisto* MixedEventDifference();
  StHbt2DHisto* LikeSignDifference();

private:
  StHbt2DHisto* mNumerator;
  StHbt2DHisto* mMixedEventDenominator;
  StHbt2DHisto* mPositiveDenominator;
  StHbt2DHisto* mNegativeDenominator;
  StHbt2DHisto* mMixedEventDifference;
  StHbt2DHisto* mLikeSignDifference;


#ifdef __ROOT__
  ClassDef(MinvLikeSignCorrFctn_Minv_vs_Pt, 1)   
#endif 
};


inline  StHbt2DHisto* MinvLikeSignCorrFctn_Minv_vs_Pt::Numerator(){return mNumerator;}
inline  StHbt2DHisto* MinvLikeSignCorrFctn_Minv_vs_Pt::MixedEventDenominator(){return mMixedEventDenominator;}
inline  StHbt2DHisto* MinvLikeSignCorrFctn_Minv_vs_Pt::PositiveDenominator(){return mPositiveDenominator;}
inline  StHbt2DHisto* MinvLikeSignCorrFctn_Minv_vs_Pt::NegativeDenominator(){return mNegativeDenominator;}
inline  StHbt2DHisto* MinvLikeSignCorrFctn_Minv_vs_Pt::MixedEventDifference(){return mMixedEventDifference;}
inline  StHbt2DHisto* MinvLikeSignCorrFctn_Minv_vs_Pt::LikeSignDifference(){return mLikeSignDifference;}

inline MinvLikeSignCorrFctn_Minv_vs_Pt* MinvLikeSignCorrFctn_Minv_vs_Pt::Clone() { MinvLikeSignCorrFctn_Minv_vs_Pt* c = new MinvLikeSignCorrFctn_Minv_vs_Pt(*this); return c;}
inline MinvLikeSignCorrFctn_Minv_vs_Pt::MinvLikeSignCorrFctn_Minv_vs_Pt(const MinvLikeSignCorrFctn_Minv_vs_Pt& fctn) :StHbtLikeSignCorrFctn() {
    mNumerator            = new StHbt2DHisto(*(fctn.mNumerator));
    mMixedEventDenominator= new StHbt2DHisto(*(fctn.mMixedEventDenominator));
    mPositiveDenominator  = new StHbt2DHisto(*(fctn.mPositiveDenominator));
    mNegativeDenominator  = new StHbt2DHisto(*(fctn.mNegativeDenominator));
    mMixedEventDifference = new StHbt2DHisto(*(fctn.mMixedEventDifference));
    mLikeSignDifference   = new StHbt2DHisto(*(fctn.mLikeSignDifference));
}


#endif

