/***************************************************************************
 *
 * $Id: MinvLikeSignCorrFctn_MinvYPt.cxx,v 1.2 2003/09/02 17:58:20 perev Exp $
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

#include "StHbtMaker/CorrFctn/MinvLikeSignCorrFctn_MinvYPt.h"
#include <cstdio>

#ifdef __ROOT__
ClassImp(MinvLikeSignCorrFctn_MinvYPt) 
#endif

//____________________________
MinvLikeSignCorrFctn_MinvYPt::MinvLikeSignCorrFctn_MinvYPt(char* title1, char* title2, 
							   const int& nxbins, const double& xLo, const double& xHi,
							   const int& nybins, const double& yLo, const double& yHi,
							   const int& nzbins, const double& zLo, const double& zHi,
							   const double& m0){
  
  mM0 = m0;

  char theTitle1[100];
  char theTitle2[100];
  sprintf(theTitle2,"%s",title2);

  // set up numerator
  sprintf(theTitle1,"MinvLikeSignCorrFctn_MinvYPt_Num_%s",title1);
  mNumeratorPt = new StHbt3DHisto(theTitle1,theTitle2,nxbins,xLo,xHi,nybins,yLo,yHi, nybins,zLo,zHi);
  // set up denominator
  sprintf(theTitle1,"MinvLikeSignCorrFctn_MinvYPt_MixedEventDen_%s",title1);
  mMixedEventDenominatorPt = new StHbt3DHisto(theTitle1,theTitle2,nxbins,xLo,xHi,nybins,yLo,yHi, nzbins,zLo,zHi);
  // set up denominator
  sprintf(theTitle1,"MinvLikeSignCorrFctn_MinvYPt_PositiveDen_%s",title1);
  mPositiveDenominatorPt = new StHbt3DHisto(theTitle1,theTitle2,nxbins,xLo,xHi,nybins,yLo,yHi, nzbins,zLo,zHi);
  // set up denominato6r
  sprintf(theTitle1,"MinvLikeSignCorrFctn_MinvYPt_NegativeDen_%s",title1);
  mNegativeDenominatorPt = new StHbt3DHisto(theTitle1,theTitle2,nxbins,xLo,xHi,nybins,yLo,yHi, nzbins,zLo,zHi);
  // set up difference
  sprintf(theTitle1,"MinvLikeSignCorrFctn_MinvYPt_MixedEventDif_%s",title1);
  mMixedEventDifferencePt = new StHbt3DHisto(theTitle1,theTitle2,nxbins,xLo,xHi,nybins,yLo,yHi, nzbins,zLo,zHi);
  // set up difference
  sprintf(theTitle1,"MinvLikeSignCorrFctn_MinvYPt_LikeSignDif_%s",title1);
  mLikeSignDifferencePt = new StHbt3DHisto(theTitle1,theTitle2,nxbins,xLo,xHi,nybins,yLo,yHi, nzbins,zLo,zHi);
  // this next bit is unfortunately needed so that we can have many histos of same "title"
  // it is neccessary if we typedef StHbt3DHisto to TH1d (which we do)
  mNumeratorPt->SetDirectory(0);
  mMixedEventDenominatorPt->SetDirectory(0);
  mPositiveDenominatorPt->SetDirectory(0);
  mNegativeDenominatorPt->SetDirectory(0);
  mMixedEventDifferencePt->SetDirectory(0);
  mLikeSignDifferencePt->SetDirectory(0);

  mNumeratorPt->Sumw2();
  mMixedEventDenominatorPt->Sumw2();
  mPositiveDenominatorPt->Sumw2();
  mNegativeDenominatorPt->Sumw2();
  mMixedEventDifferencePt->Sumw2();
  mLikeSignDifferencePt->Sumw2();

  // set up numerator
  sprintf(theTitle1,"MinvLikeSignCorrFctn_MinvYMt_Num_%s",title1);
  mNumeratorMt = new StHbt3DHisto(theTitle1,theTitle2,nxbins,xLo,xHi,nybins,yLo,yHi, nybins,zLo,zHi);
  // set up denominator
  sprintf(theTitle1,"MinvLikeSignCorrFctn_MinvYMt_MixedEventDen_%s",title1);
  mMixedEventDenominatorMt = new StHbt3DHisto(theTitle1,theTitle2,nxbins,xLo,xHi,nybins,yLo,yHi, nzbins,zLo,zHi);
  // set up denominator
  sprintf(theTitle1,"MinvLikeSignCorrFctn_MinvYMt_PositiveDen_%s",title1);
  mPositiveDenominatorMt = new StHbt3DHisto(theTitle1,theTitle2,nxbins,xLo,xHi,nybins,yLo,yHi, nzbins,zLo,zHi);
  // set up denominator
  sprintf(theTitle1,"MinvLikeSignCorrFctn_MinvYMt_NegativeDen_%s",title1);
  mNegativeDenominatorMt = new StHbt3DHisto(theTitle1,theTitle2,nxbins,xLo,xHi,nybins,yLo,yHi, nzbins,zLo,zHi);
  // set up difference
  sprintf(theTitle1,"MinvLikeSignCorrFctn_MinvYMt_MixedEventDif_%s",title1);
  mMixedEventDifferenceMt = new StHbt3DHisto(theTitle1,theTitle2,nxbins,xLo,xHi,nybins,yLo,yHi, nzbins,zLo,zHi);
  // set up difference
  sprintf(theTitle1,"MinvLikeSignCorrFctn_MinvYMt_LikeSignDif_%s",title1);
  mLikeSignDifferenceMt = new StHbt3DHisto(theTitle1,theTitle2,nxbins,xLo,xHi,nybins,yLo,yHi, nzbins,zLo,zHi);
  // this next bit is unfortunately needed so that we can have many histos of same "title"
  // it is neccessary if we typedef StHbt3DHisto to TH1d (which we do)
  mNumeratorMt->SetDirectory(0);
  mMixedEventDenominatorMt->SetDirectory(0);
  mPositiveDenominatorMt->SetDirectory(0);
  mNegativeDenominatorMt->SetDirectory(0);
  mMixedEventDifferenceMt->SetDirectory(0);
  mLikeSignDifferenceMt->SetDirectory(0);

  mNumeratorMt->Sumw2();
  mMixedEventDenominatorMt->Sumw2();
  mPositiveDenominatorMt->Sumw2();
  mNegativeDenominatorMt->Sumw2();
  mMixedEventDifferenceMt->Sumw2();
  mLikeSignDifferenceMt->Sumw2();

}   
 
//____________________________
MinvLikeSignCorrFctn_MinvYPt::~MinvLikeSignCorrFctn_MinvYPt(){
  delete mNumeratorPt;
  delete mMixedEventDenominatorPt;
  delete mPositiveDenominatorPt;
  delete mNegativeDenominatorPt;
  delete mMixedEventDifferencePt;
  delete mLikeSignDifferencePt;
  delete mNumeratorMt;
  delete mMixedEventDenominatorMt;
  delete mPositiveDenominatorMt;
  delete mNegativeDenominatorMt;
  delete mMixedEventDifferenceMt;
  delete mLikeSignDifferenceMt;
}
//_________________________
void MinvLikeSignCorrFctn_MinvYPt::Finish(){
  int NEvents = 1;
  if (   dynamic_cast<StHbtAnalysis*>( HbtAnalysis() )   ) {
    if (   dynamic_cast<mikesEventCut*>( ((StHbtAnalysis*)HbtAnalysis())->EventCut() )   )
      NEvents = ((mikesEventCut*)((StHbtAnalysis*)HbtAnalysis())->EventCut())->NEventsPassed();
  }

  double NumeratorInt;
  double MixedEventDenominatorInt;
  double PositiveDenominatorInt;
  double NegativeDenominatorInt;

  mNumeratorPt->Scale(1./NEvents);
  mMixedEventDenominatorPt->Scale(1./NEvents);
  mPositiveDenominatorPt->Scale(1./NEvents);
  mNegativeDenominatorPt->Scale(1./NEvents);
  mMixedEventDifferencePt->Scale(1./NEvents);
  mLikeSignDifferencePt->Scale(1./NEvents);


  NumeratorInt = mNumeratorPt->Integral();
  MixedEventDenominatorInt = mMixedEventDenominatorPt->Integral();
  PositiveDenominatorInt = mPositiveDenominatorPt->Integral();
  NegativeDenominatorInt = mNegativeDenominatorPt->Integral();
  mMixedEventDifferencePt->Add(mNumeratorPt,mMixedEventDenominatorPt,1.0,-1*NumeratorInt/MixedEventDenominatorInt);
  mLikeSignDifferencePt->Add(mNumeratorPt,mPositiveDenominatorPt,1.,-1.);
  mLikeSignDifferencePt->Add(mLikeSignDifferencePt,mNegativeDenominatorPt,1.,-1.);

  mNumeratorMt->Scale(1./NEvents);
  mMixedEventDenominatorMt->Scale(1./NEvents);
  mPositiveDenominatorMt->Scale(1./NEvents);
  mNegativeDenominatorMt->Scale(1./NEvents);
  mMixedEventDifferenceMt->Scale(1./NEvents);
  mLikeSignDifferenceMt->Scale(1./NEvents);


  NumeratorInt = mNumeratorMt->Integral();
  MixedEventDenominatorInt = mMixedEventDenominatorMt->Integral();
  PositiveDenominatorInt = mPositiveDenominatorMt->Integral();
  NegativeDenominatorInt = mNegativeDenominatorMt->Integral();
  mMixedEventDifferenceMt->Add(mNumeratorMt,mMixedEventDenominatorMt,1.0,-1*NumeratorInt/MixedEventDenominatorInt);
  mLikeSignDifferenceMt->Add(mNumeratorMt,mPositiveDenominatorMt,1.,-1.);
  mLikeSignDifferenceMt->Add(mLikeSignDifferenceMt,mNegativeDenominatorMt,1.,-1.);

}    
//____________________________
StHbtString MinvLikeSignCorrFctn_MinvYPt::Report(){
  string stemp = "MinvLikeSignCorrFctn_MinvYPt Report():\n";
  char ctemp[100];  
  sprintf(ctemp,"Number of entries in numerator:\t%E\n",mNumeratorPt->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in mixed event denominator:\t%E\n",mMixedEventDenominatorPt->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in like sign positive denominator:\t%E\n",mPositiveDenominatorPt->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in like sign negative denominator:\t%E\n",mNegativeDenominatorPt->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in mixed event difference:\t%E\n",mMixedEventDifferencePt->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in like sign difference:\t%E\n",mLikeSignDifferencePt->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in numerator:\t%E\n",mNumeratorMt->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in mixed event denominator:\t%E\n",mMixedEventDenominatorMt->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in like sign positive denominator:\t%E\n",mPositiveDenominatorMt->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in like sign negative denominator:\t%E\n",mNegativeDenominatorMt->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in mixed event difference:\t%E\n",mMixedEventDifferenceMt->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in like sign difference:\t%E\n",mLikeSignDifferenceMt->GetEntries());
  stemp += ctemp;
  StHbtString returnThis = stemp;
  return returnThis;
}
//____________________________
inline void MinvLikeSignCorrFctn_MinvYPt::AddRealPair(const StHbtPair* pair){
  mMinv = pair->mInv(); mY = pair->fourMomentumSum().rapidity(); 
  mPt = pair->fourMomentumSum().vect().perp(); mMt = ::sqrt(::pow(mMinv,2.)+::pow(mPt,2.)); 
  mNumeratorPt->Fill(mMinv,mY,mPt);
  mNumeratorMt->Fill(mMinv,mY,mMt-mM0);
}
//____________________________
inline void MinvLikeSignCorrFctn_MinvYPt::AddMixedPair(const StHbtPair* pair){
  mMinv = pair->mInv(); mY = pair->fourMomentumSum().rapidity(); 
  mPt = pair->fourMomentumSum().vect().perp(); mMt = ::sqrt(::pow(mMinv,2.)+::pow(mPt,2.));
  mMixedEventDenominatorPt->Fill(mMinv,mY,mPt);
  mMixedEventDenominatorMt->Fill(mMinv,mY,mMt-mM0);
}
//____________________________
inline void MinvLikeSignCorrFctn_MinvYPt::AddLikeSignPositivePair(const StHbtPair* pair){
  mMinv = pair->mInv(); mY = pair->fourMomentumSum().rapidity(); 
  mPt = pair->fourMomentumSum().vect().perp(); mMt = ::sqrt(::pow(mMinv,2.)+::pow(mPt,2.));
  mPositiveDenominatorPt->Fill(mMinv,mY,mPt);
  mPositiveDenominatorMt->Fill(mMinv,mY,mMt-mM0);
}
//____________________________
inline void MinvLikeSignCorrFctn_MinvYPt::AddLikeSignNegativePair(const StHbtPair* pair){
  mMinv = pair->mInv(); mY = pair->fourMomentumSum().rapidity(); 
  mPt = pair->fourMomentumSum().vect().perp(); mMt = ::sqrt(::pow(mMinv,2.)+::pow(mPt,2.));
  mNegativeDenominatorPt->Fill(mMinv,mY,mPt);
  mNegativeDenominatorMt->Fill(mMinv,mY,mMt-mM0);
}


