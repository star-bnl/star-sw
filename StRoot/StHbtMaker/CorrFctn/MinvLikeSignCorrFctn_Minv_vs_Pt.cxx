/***************************************************************************
 *
 * $Id: MinvLikeSignCorrFctn_Minv_vs_Pt.cxx,v 1.4 2003/01/31 19:21:09 magestro Exp $
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

#include "StHbtMaker/CorrFctn/MinvLikeSignCorrFctn_Minv_vs_Pt.h"
#include <cstdio>

#ifdef __ROOT__
ClassImp(MinvLikeSignCorrFctn_Minv_vs_Pt) 
#endif

//____________________________
MinvLikeSignCorrFctn_Minv_vs_Pt::MinvLikeSignCorrFctn_Minv_vs_Pt(char* title1, char* title2, 
								 const int& nxbins, const float& xLo, const float& xHi,
								 const int& nybins, const float& yLo, const float& yHi){

  char theTitle1[100];
  char theTitle2[100];
  sprintf(theTitle2,"%s",title2);
  // set up numerator
  sprintf(theTitle1,"MinvLikeSignCorrFctn_Minv_vs_Pt_Num_%s",title1);
  mNumerator = new StHbt2DHisto(theTitle1,theTitle2,nxbins,xLo,xHi,nybins,yLo,yHi);
  // set up denominator
  sprintf(theTitle1,"MinvLikeSignCorrFctn_Minv_vs_Pt_MixedEventDen_%s",title1);
  mMixedEventDenominator = new StHbt2DHisto(theTitle1,theTitle2,nxbins,xLo,xHi,nybins,yLo,yHi);
  // set up denominator
  sprintf(theTitle1,"MinvLikeSignCorrFctn_Minv_vs_Pt_PositiveDen_%s",title1);
  mPositiveDenominator = new StHbt2DHisto(theTitle1,theTitle2,nxbins,xLo,xHi,nybins,yLo,yHi);
  // set up denominator
  sprintf(theTitle1,"MinvLikeSignCorrFctn_Minv_vs_Pt_NegativeDen_%s",title1);
  mNegativeDenominator = new StHbt2DHisto(theTitle1,theTitle2,nxbins,xLo,xHi,nybins,yLo,yHi);
  // set up difference
  sprintf(theTitle1,"MinvLikeSignCorrFctn_Minv_vs_Pt_MixedEventDif_%s",title1);
  mMixedEventDifference = new StHbt2DHisto(theTitle1,theTitle2,nxbins,xLo,xHi,nybins,yLo,yHi);
  // set up difference
  sprintf(theTitle1,"MinvLikeSignCorrFctn_Minv_vs_Pt_LikeSignDif_%s",title1);
  mLikeSignDifference = new StHbt2DHisto(theTitle1,theTitle2,nxbins,xLo,xHi,nybins,yLo,yHi);
  // this next bit is unfortunately needed so that we can have many histos of same "title"
  // it is neccessary if we typedef StHbt2DHisto to TH1d (which we do)
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

}   
 
//____________________________
MinvLikeSignCorrFctn_Minv_vs_Pt::~MinvLikeSignCorrFctn_Minv_vs_Pt(){
  delete mNumerator;
  delete mMixedEventDenominator;
  delete mPositiveDenominator;
  delete mNegativeDenominator;
  delete mMixedEventDifference;
  delete mLikeSignDifference;
}
//_________________________
void MinvLikeSignCorrFctn_Minv_vs_Pt::Finish(){
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
  mMixedEventDifference->Add(mNumerator,mMixedEventDenominator,1.0,-1*NumeratorInt/MixedEventDenominatorInt);
  mLikeSignDifference->Add(mNumerator,mPositiveDenominator,1.,-1.);
  mLikeSignDifference->Add(mLikeSignDifference,mNegativeDenominator,1.,-1.);

}    
//____________________________
StHbtString MinvLikeSignCorrFctn_Minv_vs_Pt::Report(){
  string stemp = "MinvLikeSignCorrFctn_Minv_vs_Pt Report():\n";
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
inline void MinvLikeSignCorrFctn_Minv_vs_Pt::AddRealPair(const StHbtPair* pair){
  mNumerator->Fill(pair->mInv(),pair->fourMomentumSum().vect().perp());
}
//____________________________
inline void MinvLikeSignCorrFctn_Minv_vs_Pt::AddMixedPair(const StHbtPair* pair){
  mMixedEventDenominator->Fill(pair->mInv(),pair->fourMomentumSum().vect().perp());
}
//____________________________
inline void MinvLikeSignCorrFctn_Minv_vs_Pt::AddLikeSignPositivePair(const StHbtPair* pair){
  mPositiveDenominator->Fill(pair->mInv(),pair->fourMomentumSum().vect().perp());
}
//____________________________
inline void MinvLikeSignCorrFctn_Minv_vs_Pt::AddLikeSignNegativePair(const StHbtPair* pair){
  mNegativeDenominator->Fill(pair->mInv(),pair->fourMomentumSum().vect().perp());
}


