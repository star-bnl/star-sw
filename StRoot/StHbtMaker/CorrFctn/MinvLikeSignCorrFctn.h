/***************************************************************************
 *
 * $Id: MinvLikeSignCorrFctn.h,v 1.2 2001/01/23 15:12:55 laue Exp $
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

#ifndef MinvLikeSignCorrFctn_hh
#define MinvLikeSignCorrFctn_hh

#include "StHbtMaker/Infrastructure/StHbtAnalysis.h"
#include "StHbtMaker/Base/StHbtLikeSignCorrFctn.hh"
#include "StHbtMaker/Cut/mikesEventCut.h"

#ifdef __ROOT__
  #include "StHbtMaker/Infrastructure/StHbtTagWriter.hh"
#endif

class MinvLikeSignCorrFctn : public StHbtLikeSignCorrFctn {
public:
  MinvLikeSignCorrFctn(const MinvLikeSignCorrFctn& ); // copy constructor
  MinvLikeSignCorrFctn(char* title1, char* title2, const int& nbins, const float& MinvLo, const float& MinvHi);
  virtual ~MinvLikeSignCorrFctn();

  virtual StHbtString Report();
  virtual void AddRealPair(const StHbtPair*);
  virtual void AddMixedPair(const StHbtPair*);
  virtual void AddLikeSignPositivePair(const StHbtPair*);
  virtual void AddLikeSignNegativePair(const StHbtPair*);
  virtual void Finish();
  MinvLikeSignCorrFctn* Clone();

  StHbt1DHisto* Numerator();
  StHbt1DHisto* MixedEventDenominator();
  StHbt1DHisto* PositiveDenominator();
  StHbt1DHisto* NegativeDenominator();
  StHbt1DHisto* MixedEventDifference();
  StHbt1DHisto* LikeSignDifference();

private:
  StHbt1DHisto* mNumerator;
  StHbt1DHisto* mMixedEventDenominator;
  StHbt1DHisto* mPositiveDenominator;
  StHbt1DHisto* mNegativeDenominator;
  StHbt1DHisto* mMixedEventDifference;
  StHbt1DHisto* mLikeSignDifference;


#ifdef __ROOT__
  StHbtTagWriter* mTagWriter;  //! <-- this is a singleton
  ClassDef(MinvLikeSignCorrFctn, 1)   
#endif 
};


inline  StHbt1DHisto* MinvLikeSignCorrFctn::Numerator(){return mNumerator;}
inline  StHbt1DHisto* MinvLikeSignCorrFctn::MixedEventDenominator(){return mMixedEventDenominator;}
inline  StHbt1DHisto* MinvLikeSignCorrFctn::PositiveDenominator(){return mPositiveDenominator;}
inline  StHbt1DHisto* MinvLikeSignCorrFctn::NegativeDenominator(){return mNegativeDenominator;}
inline  StHbt1DHisto* MinvLikeSignCorrFctn::MixedEventDifference(){return mMixedEventDifference;}
inline  StHbt1DHisto* MinvLikeSignCorrFctn::LikeSignDifference(){return mLikeSignDifference;}

inline MinvLikeSignCorrFctn* MinvLikeSignCorrFctn::Clone() { MinvLikeSignCorrFctn* c = new MinvLikeSignCorrFctn(*this); return c;}
inline MinvLikeSignCorrFctn::MinvLikeSignCorrFctn(const MinvLikeSignCorrFctn& fctn) :StHbtLikeSignCorrFctn() {
#ifdef __ROOT__
    mTagWriter = StHbtTagWriter::Instance();  
#endif
    mNumerator = new StHbt1DHisto(*(fctn.mNumerator));
    mMixedEventDenominator= new StHbt1DHisto(*(fctn.mMixedEventDenominator));
    mPositiveDenominator= new StHbt1DHisto(*(fctn.mPositiveDenominator));
    mNegativeDenominator= new StHbt1DHisto(*(fctn.mNegativeDenominator));
    mMixedEventDifference = new StHbt1DHisto(*(fctn.mMixedEventDifference));
    mLikeSignDifference = new StHbt1DHisto(*(fctn.mLikeSignDifference));
}


#endif

