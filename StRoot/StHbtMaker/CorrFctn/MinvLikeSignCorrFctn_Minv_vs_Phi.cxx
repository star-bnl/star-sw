/***************************************************************************
 *
 * $Id: MinvLikeSignCorrFctn_Minv_vs_Phi.cxx,v 1.1 2000/09/01 18:37:27 laue Exp $
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

#include "StHbtMaker/CorrFctn/MinvLikeSignCorrFctn_Minv_vs_Phi.h"
#include <cstdio>

#ifdef __ROOT__
ClassImp(MinvLikeSignCorrFctn_Minv_vs_Phi) 
#endif

//____________________________
MinvLikeSignCorrFctn_Minv_vs_Phi::MinvLikeSignCorrFctn_Minv_vs_Phi(char* title, 
								 const int& nxbins, const float& xLo, const float& xHi,
								 const int& nybins, const float& yLo, const float& yHi){

  char theTitle[100];
  // set up numerator
  char *TitNum = "MinvLikeSignCorrFctn_Minv_vs_Phi_Num";
  sprintf(theTitle,"Num%s\n",title);
  mNumerator = new StHbt2DHisto(TitNum,theTitle,nxbins,xLo,xHi,nybins,yLo,yHi);
  // set up denominator
  char *TitMixedEventDen= "MinvLikeSignCorrFctn_Minv_vs_Phi_MixedEventDen";
  sprintf(theTitle,"MixedEventDen%s\n",title);
  mMixedEventDenominator = new StHbt2DHisto(TitMixedEventDen,theTitle,nxbins,xLo,xHi,nybins,yLo,yHi);
  // set up denominator
  char *TitPositiveDen= "MinvLikeSignCorrFctn_Minv_vs_Phi_PositiveDen";
  sprintf(theTitle,"PositiveDen%s\n",title);
  mPositiveDenominator = new StHbt2DHisto(TitPositiveDen,theTitle,nxbins,xLo,xHi,nybins,yLo,yHi);
  // set up denominator
  char *TitNegativeDen= "MinvLikeSignCorrFctn_Minv_vs_Phi_NegativeDen";
  sprintf(theTitle,"NegativeDen%s\n",title);
  mNegativeDenominator = new StHbt2DHisto(TitNegativeDen,theTitle,nxbins,xLo,xHi,nybins,yLo,yHi);
  // set up difference
  char *TitMixedEventDif = "MinvLikeSignCorrFctn_Minv_vs_Phi_MixedEventDif";
  sprintf(theTitle,"MixedEventDif%s\n",title);
  mMixedEventDifference = new StHbt2DHisto(TitMixedEventDif,theTitle,nxbins,xLo,xHi,nybins,yLo,yHi);
  // set up difference
  char *TitLikeSignDif = "MinvLikeSignCorrFctn_Minv_vs_Phi_LikeSignDif";
  sprintf(theTitle,"LikeSignDif%s\n",title);
  mLikeSignDifference = new StHbt2DHisto(TitLikeSignDif,theTitle,nxbins,xLo,xHi,nybins,yLo,yHi);
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
MinvLikeSignCorrFctn_Minv_vs_Phi::~MinvLikeSignCorrFctn_Minv_vs_Phi(){
  delete mNumerator;
  delete mMixedEventDenominator;
  delete mPositiveDenominator;
  delete mNegativeDenominator;
  delete mMixedEventDifference;
  delete mLikeSignDifference;
}
//_________________________
void MinvLikeSignCorrFctn_Minv_vs_Phi::Finish(){
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
StHbtString MinvLikeSignCorrFctn_Minv_vs_Phi::Report(){
  string stemp = "MinvLikeSignCorrFctn_Minv_vs_Phi Report():\n";
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
inline void MinvLikeSignCorrFctn_Minv_vs_Phi::AddRealPair(const StHbtPair* pair){
  //  if ( fabs(pair->mInv()-1.02)<0.01) 
    if ( !isnan(mReactionPlaneError) && mReactionPlaneError!=0. )
      mNumerator->Fill(pair->mInv(),pair->fourMomentumSum().vect().phi()-mReactionPlane);
}
//____________________________
inline void MinvLikeSignCorrFctn_Minv_vs_Phi::AddMixedPair(const StHbtPair* pair){
  //  if ( fabs(pair->mInv()-1.02)<0.01) 
    if ( !isnan(mReactionPlaneError) && mReactionPlaneError!=0. )
      mMixedEventDenominator->Fill(pair->mInv(),pair->fourMomentumSum().vect().phi()-mReactionPlane);
}
//____________________________
inline void MinvLikeSignCorrFctn_Minv_vs_Phi::AddLikeSignPositivePair(const StHbtPair* pair){
  //  if ( fabs(pair->mInv()-1.02)<0.01) 
    if ( !isnan(mReactionPlaneError) && mReactionPlaneError!=0. )
      mPositiveDenominator->Fill(pair->mInv(),pair->fourMomentumSum().vect().phi()-mReactionPlane);
}
//____________________________
inline void MinvLikeSignCorrFctn_Minv_vs_Phi::AddLikeSignNegativePair(const StHbtPair* pair){
  //  if ( fabs(pair->mInv()-1.02)<0.01) 
    if ( !isnan(mReactionPlaneError) && mReactionPlaneError!=0. )
      mNegativeDenominator->Fill(pair->mInv(),pair->fourMomentumSum().vect().phi()-mReactionPlane);
}


