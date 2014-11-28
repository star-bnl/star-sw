/***************************************************************************
 *
 * $Id: MinvCorrFctnY_vs_Pt.cxx,v 1.2 2000/06/15 18:52:42 willson Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *     A simple invariant-mass correlation function
 *
 ***************************************************************************
 *
 * $Log: MinvCorrFctnY_vs_Pt.cxx,v $
 * Revision 1.2  2000/06/15 18:52:42  willson
 * HbtAnalysis() method must be cast to specific analysis
 * rotateEventCut installed
 *
 * Revision 1.1  2000/02/28 14:39:30  laue
 * Correlation function to fill phasespace rapidity vs pt
 *
 * Revision 1.2  1999/07/06 22:33:19  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/


#include "StHbtMaker/CorrFctn/MinvCorrFctnY_vs_Pt.h"
//#include "StHbtMaker/Infrastructure/StHbtHisto.hh"
#include <cstdio>
 
#ifdef __ROOT__
ClassImp(MinvCorrFctnY_vs_Pt)
#endif
//____________________________
MinvCorrFctnY_vs_Pt::MinvCorrFctnY_vs_Pt(char* title, 
					 const int& nbins1, const float& MinvLo1, const float& MinvHi1,
					 const int& nbins2, const float& MinvLo2, const float& MinvHi2){
  char theTitle[100];
  // set up numerator
  char* TitNum = "MinvCorrFctnY_vs_Pt_Num";
  sprintf(theTitle,"Num %s",title);
  mNumerator = new StHbt2DHisto(TitNum,theTitle,nbins1,MinvLo1,MinvHi1,nbins2,MinvLo2,MinvHi2);
  // set up denominator
  char* TitDen = "MinvCorrFctnY_vs_Pt_Den";
  sprintf(theTitle,"Den %s",title);
  mDenominator = new StHbt2DHisto(TitDen,theTitle,nbins1,MinvLo1,MinvHi1,nbins2,MinvLo2,MinvHi2);
  // set up difference
  char* TitDif = "MinvCorrFctnY_vs_Pt_Dif";
  sprintf(theTitle,"Dif %s",title);
  mDifference = new StHbt2DHisto(TitDif,theTitle,nbins1,MinvLo1,MinvHi1,nbins2,MinvLo2,MinvHi2);
  // this next bit is unfortunately needed so that we can have many histos of same "title"
  // it is neccessary if we typedef StHbt1DHisto to TH1d (which we do)
  mNumerator->SetDirectory(0);
  mDenominator->SetDirectory(0);
  mDifference->SetDirectory(0);
  mNumerator->Sumw2();
  mDenominator->Sumw2();
  mDifference->Sumw2(); 
}

//____________________________
MinvCorrFctnY_vs_Pt::~MinvCorrFctnY_vs_Pt(){
  delete mNumerator;
  delete mDenominator;
  delete mDifference;
}
//_________________________
void MinvCorrFctnY_vs_Pt::Finish(){
  // here is where we should normalize, fit, etc...
  // we should NOT Draw() the histos (as I had done it below),
  // since we want to insulate ourselves from root at this level
  // of the code.  Do it instead at root command line with browser.
  //  mNumerator->Draw();
  //  mDenominator->Draw();
  //mRatio->Draw();
  
  // normalized by number of events
  int NEvents = 1;
  if (   dynamic_cast<StHbtAnalysis*>( HbtAnalysis() )   ) {
    if (   dynamic_cast<mikesEventCut*>( ((StHbtAnalysis*)HbtAnalysis())->EventCut() )   )
      NEvents = ((mikesEventCut*)((StHbtAnalysis*)HbtAnalysis())->EventCut())->NEventsPassed();
  }
 
  mNumerator->Scale(1./NEvents);
  mDenominator->Scale(1./NEvents);
  mDifference->Scale(1./NEvents);
    
  // scale numerator and denominator
  //double NumeratorInt = mNumerator->Integral();
  //double DenominatorInt = mDenominator->Integral();
  double NumeratorInt = mRealPairs;
  double DenominatorInt = mMixedPairs;
  mDifference->Add(mNumerator,mDenominator,1.0,-1*NumeratorInt/DenominatorInt);

}

//____________________________
StHbtString MinvCorrFctnY_vs_Pt::Report(){
  string stemp = "Minv Correlation Function Report:\n";
  char ctemp[100];
  sprintf(ctemp,"Number of entries in numerator:\t%E\n",mNumerator->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in denominator:\t%E\n",mDenominator->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in difference:\t%E\n",mDifference->GetEntries());
  stemp += ctemp;
  StHbtString returnThis = stemp;
  return returnThis;
}
//____________________________
void MinvCorrFctnY_vs_Pt::AddRealPair(const StHbtPair* pair){
  if ( fabs( pair->mInv()-1.02 ) < .01 ) 
    mNumerator->Fill( pair->fourMomentumSum().rapidity(), pair->fourMomentumSum().perp(), 1.);
  else
    mRealPairs++;
}
//____________________________
void MinvCorrFctnY_vs_Pt::AddMixedPair(const StHbtPair* pair){
  if ( fabs( pair->mInv()-1.02 ) < .01 ) 
    mDenominator->Fill( pair->fourMomentumSum().rapidity(), pair->fourMomentumSum().perp(), 1.);
  else
    mMixedPairs++;
}


