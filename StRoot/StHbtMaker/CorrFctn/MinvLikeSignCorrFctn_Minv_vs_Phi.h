/***************************************************************************
 *
 * $Id: MinvLikeSignCorrFctn_Minv_vs_Phi.h,v 1.3 2001/06/04 19:09:51 rcwells Exp $
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

#ifndef MinvLikeSignCorrFctn_Minv_vs_Phi_hh
#define MinvLikeSignCorrFctn_Minv_vs_Phi_hh

# define M_PI           3.14159265358979323846  /* pi */
# define M_PIl          3.1415926535897932384626433832795029L  /* pi */

#include "StHbtMaker/Infrastructure/StHbtAnalysis.h"
#include "StHbtMaker/Base/StHbtLikeSignCorrFctn.hh"
#include "StHbtMaker/Cut/mikesEventCut.h"
#include <cmath>


class MinvLikeSignCorrFctn_Minv_vs_Phi : public StHbtLikeSignCorrFctn {
public:
  MinvLikeSignCorrFctn_Minv_vs_Phi(const MinvLikeSignCorrFctn_Minv_vs_Phi& ); // copy constructor
  MinvLikeSignCorrFctn_Minv_vs_Phi(char* title1, char* title2, 
				  const int& nxbins=100, const float& xLo=.98, const float& xHi=1.08,
				  const int& nybins=12, const float& yLo=0., const float& yHi=M_PIl);
  virtual ~MinvLikeSignCorrFctn_Minv_vs_Phi();

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);
  virtual void AddLikeSignPositivePair(const StHbtPair*);
  virtual void AddLikeSignNegativePair(const StHbtPair*);
  virtual void Finish();
  virtual void EventBegin(const StHbtEvent* ev) { 
    mReactionPlane = ev->ReactionPlane();
    mReactionPlaneError = ev->ReactionPlaneSubEventDifference();
  }

  MinvLikeSignCorrFctn_Minv_vs_Phi* Clone();

  StHbt2DHisto* Numerator();
  StHbt2DHisto* MixedEventDenominator();
  StHbt2DHisto* PositiveDenominator();
  StHbt2DHisto* NegativeDenominator();
  StHbt2DHisto* MixedEventDifference();
  StHbt2DHisto* LikeSignDifference();

private:
  float mReactionPlane;
  float mReactionPlaneError;
  StHbt2DHisto* mNumerator;
  StHbt2DHisto* mMixedEventDenominator;
  StHbt2DHisto* mPositiveDenominator;
  StHbt2DHisto* mNegativeDenominator;
  StHbt2DHisto* mMixedEventDifference;
  StHbt2DHisto* mLikeSignDifference;


#ifdef __ROOT__
  ClassDef(MinvLikeSignCorrFctn_Minv_vs_Phi, 1)   
#endif 
};


inline  StHbt2DHisto* MinvLikeSignCorrFctn_Minv_vs_Phi::Numerator(){return mNumerator;}
inline  StHbt2DHisto* MinvLikeSignCorrFctn_Minv_vs_Phi::MixedEventDenominator(){return mMixedEventDenominator;}
inline  StHbt2DHisto* MinvLikeSignCorrFctn_Minv_vs_Phi::PositiveDenominator(){return mPositiveDenominator;}
inline  StHbt2DHisto* MinvLikeSignCorrFctn_Minv_vs_Phi::NegativeDenominator(){return mNegativeDenominator;}
inline  StHbt2DHisto* MinvLikeSignCorrFctn_Minv_vs_Phi::MixedEventDifference(){return mMixedEventDifference;}
inline  StHbt2DHisto* MinvLikeSignCorrFctn_Minv_vs_Phi::LikeSignDifference(){return mLikeSignDifference;}

inline MinvLikeSignCorrFctn_Minv_vs_Phi* MinvLikeSignCorrFctn_Minv_vs_Phi::Clone() { MinvLikeSignCorrFctn_Minv_vs_Phi* c = new MinvLikeSignCorrFctn_Minv_vs_Phi(*this); return c;}
inline MinvLikeSignCorrFctn_Minv_vs_Phi::MinvLikeSignCorrFctn_Minv_vs_Phi(const MinvLikeSignCorrFctn_Minv_vs_Phi& fctn) :StHbtLikeSignCorrFctn() {
    mNumerator            = new StHbt2DHisto(*(fctn.mNumerator));
    mMixedEventDenominator= new StHbt2DHisto(*(fctn.mMixedEventDenominator));
    mPositiveDenominator  = new StHbt2DHisto(*(fctn.mPositiveDenominator));
    mNegativeDenominator  = new StHbt2DHisto(*(fctn.mNegativeDenominator));
    mMixedEventDifference = new StHbt2DHisto(*(fctn.mMixedEventDifference));
    mLikeSignDifference   = new StHbt2DHisto(*(fctn.mLikeSignDifference));
}


#endif

