/***************************************************************************
 *
 * $Id: MinvLikeSignCorrFctn.cxx,v 1.1 2000/09/01 18:37:27 laue Exp $
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
//#ifndef __CINT__
//#include "fortranc.h"
//#define fortrantest F77_NAME(fortrantest,FORTRANTEST)
//extern "C" {int type_of_call F77_NAME(fortrantest,FORTRANTEST)(int*);}
//#endif

#include "StHbtMaker/CorrFctn/MinvLikeSignCorrFctn.h"
#include <cstdio>

#ifdef __ROOT__
ClassImp(MinvLikeSignCorrFctn) 
#endif

//____________________________
MinvLikeSignCorrFctn::MinvLikeSignCorrFctn(char* title, const int& nbins, const float& MinvLo, const float& MinvHi){
#ifdef __ROOT__
  mTagWriter = StHbtTagWriter::Instance();  // get the singleton
#endif

  char theTitle[100];
  // set up numerator
  char *TitNum = "MinvLikeSignCorrFctn_Num";
  sprintf(theTitle,"Num%s\n",title);
  mNumerator = new StHbt1DHisto(TitNum,theTitle,nbins,MinvLo,MinvHi);
  // set up denominator
  char *TitMixedEventDen= "MinvLikeSignCorrFctn_MixedEventDen";
  sprintf(theTitle,"MixedEventDen%s\n",title);
  mMixedEventDenominator = new StHbt1DHisto(TitMixedEventDen,theTitle,nbins,MinvLo,MinvHi);
  // set up denominator
  char *TitPositiveDen= "MinvLikeSignCorrFctn_PositiveDen";
  sprintf(theTitle,"PositiveDen%s\n",title);
  mPositiveDenominator = new StHbt1DHisto(TitPositiveDen,theTitle,nbins,MinvLo,MinvHi);
  // set up denominator
  char *TitNegativeDen= "MinvLikeSignCorrFctn_NegativeDen";
  sprintf(theTitle,"NegativeDen%s\n",title);
  mNegativeDenominator = new StHbt1DHisto(TitNegativeDen,theTitle,nbins,MinvLo,MinvHi);
  // set up difference
  char *TitMixedEventDif = "MinvLikeSignCorrFctn_MixedEventDif";
  sprintf(theTitle,"MixedEventDif%s\n",title);
  mMixedEventDifference = new StHbt1DHisto(TitMixedEventDif,theTitle,nbins,MinvLo,MinvHi);
  // set up difference
  char *TitLikeSignDif = "MinvLikeSignCorrFctn_LikeSignDif";
  sprintf(theTitle,"LikeSignDif%s\n",title);
  mLikeSignDifference = new StHbt1DHisto(TitLikeSignDif,theTitle,nbins,MinvLo,MinvHi);
  // this next bit is unfortunately needed so that we can have many histos of same "title"
  // it is neccessary if we typedef StHbt1DHisto to TH1d (which we do)
  mNumerator->SetDirectory(0);
  mMixedEventDenominator->SetDirectory(0);
  mPositiveDenominator->SetDirectory(0);
  mNegativeDenominator->SetDirectory(0);
  mMixedEventDifference->SetDirectory(0);
  mLikeSignDifference->SetDirectory(0);

  mNumerator->Sumw2();
  mMixedEventDenominator->Sumw2();
  mPositiveDenominator->Sumw2();
  mNegativeDenominator->Sumw2();
  mMixedEventDifference->Sumw2();
  mLikeSignDifference->Sumw2();

  //  for (int i=0; i < 100; i++) {
  //    int j = fortrantest( &i );
  //  }
}   
 
//____________________________
MinvLikeSignCorrFctn::~MinvLikeSignCorrFctn(){
  delete mNumerator;
  delete mMixedEventDenominator;
  delete mPositiveDenominator;
  delete mNegativeDenominator;
  delete mMixedEventDifference;
  delete mLikeSignDifference;
}
//_________________________
void MinvLikeSignCorrFctn::Finish(){
  int NEvents = 1;
  if (   dynamic_cast<StHbtAnalysis*>( HbtAnalysis() )   ) {
    if (   dynamic_cast<mikesEventCut*>( ((StHbtAnalysis*)HbtAnalysis())->EventCut() )   )
      NEvents = ((mikesEventCut*)((StHbtAnalysis*)HbtAnalysis())->EventCut())->NEventsPassed();
  }

  mNumerator->Scale(1./NEvents);
  mMixedEventDenominator->Scale(1./NEvents);
  mPositiveDenominator->Scale(1./NEvents);
  mNegativeDenominator->Scale(1./NEvents);
  mMixedEventDifference->Scale(1./NEvents);
  mLikeSignDifference->Scale(1./NEvents);

  double NumeratorInt = mNumerator->Integral();
  double MixedEventDenominatorInt = mMixedEventDenominator->Integral();
  double PositiveDenominatorInt = mPositiveDenominator->Integral();
  double NegativeDenominatorInt = mNegativeDenominator->Integral();
  mMixedEventDifference->Add(mNumerator,mMixedEventDenominator,1.0,-1*NumeratorInt/MixedEventDenominatorInt);
  mLikeSignDifference->Add(mNumerator,mPositiveDenominator,1.,-1.);
  mLikeSignDifference->Add(mLikeSignDifference,mNegativeDenominator,1.,-1.);

}    
//____________________________
StHbtString MinvLikeSignCorrFctn::Report(){
  string stemp = "Minv Correlation Function Report:\n";
  char ctemp[100];  
  sprintf(ctemp,"Number of entries in numerator:\t%E\n",mNumerator->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in mixed event denominator:\t%E\n",mMixedEventDenominator->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in like sign positive denominator:\t%E\n",mPositiveDenominator->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in like sign negative denominator:\t%E\n",mNegativeDenominator->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in mixed event difference:\t%E\n",mMixedEventDifference->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in like sign difference:\t%E\n",mLikeSignDifference->GetEntries());
  stemp += ctemp;
  StHbtString returnThis = stemp;
  return returnThis;
}
//____________________________
inline void MinvLikeSignCorrFctn::AddRealPair(const StHbtPair* pair){
  mNumerator->Fill(pair->mInv());
}
//____________________________
inline void MinvLikeSignCorrFctn::AddMixedPair(const StHbtPair* pair){
  mMixedEventDenominator->Fill(pair->mInv());
}
//____________________________
inline void MinvLikeSignCorrFctn::AddLikeSignPositivePair(const StHbtPair* pair){
  mPositiveDenominator->Fill(pair->mInv());
}
//____________________________
inline void MinvLikeSignCorrFctn::AddLikeSignNegativePair(const StHbtPair* pair){
  mNegativeDenominator->Fill(pair->mInv());
}


