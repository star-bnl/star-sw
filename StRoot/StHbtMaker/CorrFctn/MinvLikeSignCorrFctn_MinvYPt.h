/***************************************************************************
 *
 * $Id: MinvLikeSignCorrFctn_MinvYPt.h,v 1.1 2001/06/21 19:08:41 laue Exp $
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

#ifndef MinvLikeSignCorrFctn_MinvYPt_hh
#define MinvLikeSignCorrFctn_MinvYPt_hh

#include "StHbtMaker/Infrastructure/StHbtAnalysis.h"
#include "StHbtMaker/Base/StHbtLikeSignCorrFctn.hh"
#include "StHbtMaker/Cut/mikesEventCut.h"

class MinvLikeSignCorrFctn_MinvYPt : public StHbtLikeSignCorrFctn {
public:
  MinvLikeSignCorrFctn_MinvYPt(const MinvLikeSignCorrFctn_MinvYPt& ); // copy constructor
  MinvLikeSignCorrFctn_MinvYPt(char* title1, char* title2, 
			       const int& nxbins, const double& xLo, const double& xHi,
			       const int& nybins, const double& yLo, const double& yHi,
			       const int& nzbins, const double& zLo, const double& zHi,
			       const double& m0=1.019417);
  virtual ~MinvLikeSignCorrFctn_MinvYPt();

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);
  virtual void AddLikeSignPositivePair(const StHbtPair*);
  virtual void AddLikeSignNegativePair(const StHbtPair*);
  virtual void Finish();
  MinvLikeSignCorrFctn_MinvYPt* Clone();

  StHbt3DHisto* NumeratorPt();
  StHbt3DHisto* MixedEventDenominatorPt();
  StHbt3DHisto* PositiveDenominatorPt();
  StHbt3DHisto* NegativeDenominatorPt();
  StHbt3DHisto* MixedEventDifferencePt();
  StHbt3DHisto* LikeSignDifferencePt();
  StHbt3DHisto* NumeratorMt();
  StHbt3DHisto* MixedEventDenominatorMt();
  StHbt3DHisto* PositiveDenominatorMt();
  StHbt3DHisto* NegativeDenominatorMt();
  StHbt3DHisto* MixedEventDifferenceMt();
  StHbt3DHisto* LikeSignDifferenceMt();

private:
  StHbt3DHisto* mNumeratorPt;
  StHbt3DHisto* mMixedEventDenominatorPt;
  StHbt3DHisto* mPositiveDenominatorPt;
  StHbt3DHisto* mNegativeDenominatorPt;
  StHbt3DHisto* mMixedEventDifferencePt;
  StHbt3DHisto* mLikeSignDifferencePt;
  StHbt3DHisto* mNumeratorMt;
  StHbt3DHisto* mMixedEventDenominatorMt;
  StHbt3DHisto* mPositiveDenominatorMt;
  StHbt3DHisto* mNegativeDenominatorMt;
  StHbt3DHisto* mMixedEventDifferenceMt;
  StHbt3DHisto* mLikeSignDifferenceMt;

  double mM0;
  double mMinv;
  double mY;
  double mPt;
  double mMt;

#ifdef __ROOT__
  ClassDef(MinvLikeSignCorrFctn_MinvYPt, 1)   
#endif 
};


inline  StHbt3DHisto* MinvLikeSignCorrFctn_MinvYPt::NumeratorPt(){return mNumeratorPt;}
inline  StHbt3DHisto* MinvLikeSignCorrFctn_MinvYPt::MixedEventDenominatorPt(){return mMixedEventDenominatorPt;}
inline  StHbt3DHisto* MinvLikeSignCorrFctn_MinvYPt::PositiveDenominatorPt(){return mPositiveDenominatorPt;}
inline  StHbt3DHisto* MinvLikeSignCorrFctn_MinvYPt::NegativeDenominatorPt(){return mNegativeDenominatorPt;}
inline  StHbt3DHisto* MinvLikeSignCorrFctn_MinvYPt::MixedEventDifferencePt(){return mMixedEventDifferencePt;}
inline  StHbt3DHisto* MinvLikeSignCorrFctn_MinvYPt::LikeSignDifferencePt(){return mLikeSignDifferencePt;}
inline  StHbt3DHisto* MinvLikeSignCorrFctn_MinvYPt::NumeratorMt(){return mNumeratorMt;}
inline  StHbt3DHisto* MinvLikeSignCorrFctn_MinvYPt::MixedEventDenominatorMt(){return mMixedEventDenominatorMt;}
inline  StHbt3DHisto* MinvLikeSignCorrFctn_MinvYPt::PositiveDenominatorMt(){return mPositiveDenominatorMt;}
inline  StHbt3DHisto* MinvLikeSignCorrFctn_MinvYPt::NegativeDenominatorMt(){return mNegativeDenominatorMt;}
inline  StHbt3DHisto* MinvLikeSignCorrFctn_MinvYPt::MixedEventDifferenceMt(){return mMixedEventDifferenceMt;}
inline  StHbt3DHisto* MinvLikeSignCorrFctn_MinvYPt::LikeSignDifferenceMt(){return mLikeSignDifferenceMt;}

inline MinvLikeSignCorrFctn_MinvYPt* MinvLikeSignCorrFctn_MinvYPt::Clone() { MinvLikeSignCorrFctn_MinvYPt* c = new MinvLikeSignCorrFctn_MinvYPt(*this); return c;}
inline MinvLikeSignCorrFctn_MinvYPt::MinvLikeSignCorrFctn_MinvYPt(const MinvLikeSignCorrFctn_MinvYPt& fctn) :StHbtLikeSignCorrFctn() {
  mM0 = fctn.mM0;
  mNumeratorPt            = new StHbt3DHisto(*(fctn.mNumeratorPt));
  mMixedEventDenominatorPt= new StHbt3DHisto(*(fctn.mMixedEventDenominatorPt));
  mPositiveDenominatorPt  = new StHbt3DHisto(*(fctn.mPositiveDenominatorPt));
  mNegativeDenominatorPt  = new StHbt3DHisto(*(fctn.mNegativeDenominatorPt));
  mMixedEventDifferencePt = new StHbt3DHisto(*(fctn.mMixedEventDifferencePt));
  mLikeSignDifferencePt   = new StHbt3DHisto(*(fctn.mLikeSignDifferencePt));
  mNumeratorMt            = new StHbt3DHisto(*(fctn.mNumeratorMt));
  mMixedEventDenominatorMt= new StHbt3DHisto(*(fctn.mMixedEventDenominatorMt));
  mPositiveDenominatorMt  = new StHbt3DHisto(*(fctn.mPositiveDenominatorMt));
  mNegativeDenominatorMt  = new StHbt3DHisto(*(fctn.mNegativeDenominatorMt));
  mMixedEventDifferenceMt = new StHbt3DHisto(*(fctn.mMixedEventDifferenceMt));
  mLikeSignDifferenceMt   = new StHbt3DHisto(*(fctn.mLikeSignDifferenceMt));
}


#endif

