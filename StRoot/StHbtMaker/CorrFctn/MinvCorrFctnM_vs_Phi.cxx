/***************************************************************************
 *
 * $Id: MinvCorrFctnM_vs_Phi.cxx,v 1.1 2000/05/25 22:09:15 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *     A simple invariant-mass vs azimuth angle correlation function
 *
 ***************************************************************************
 *
 * $Log: MinvCorrFctnM_vs_Phi.cxx,v $
 * Revision 1.1  2000/05/25 22:09:15  laue
 * New correlation function. Minv vs azimuth angle
 *
 *
 **************************************************************************/


#include "StHbtMaker/CorrFctn/MinvCorrFctnM_vs_Phi.h"
//#include "StHbtMaker/Infrastructure/StHbtHisto.hh"
#include <cstdio>

#ifdef __ROOT__
ClassImp(MinvCorrFctnM_vs_Phi)
#endif

//____________________________
MinvCorrFctnM_vs_Phi::MinvCorrFctnM_vs_Phi(char* title, 
					 const int& nbins1, const float& MinvLo1, const float& MinvHi1,
					 const int& nbins2, const float& MinvLo2, const float& MinvHi2){
  // set up numerator
  char theTitle[100];
  char* TitNum = "MinvCorrFctnM_vs_Phi_Num";
  sprintf(theTitle,"Num %s",title);
  mNumerator = new StHbt2DHisto(TitNum,theTitle,nbins1,MinvLo1,MinvHi1,nbins2,MinvLo2,MinvHi2);
  // set up denominator
  char* TitDen = "MinvCorrFctnM_vs_Phi_Den";
  sprintf(theTitle,"Den %s",title);
  mDenominator = new StHbt2DHisto(TitDen,theTitle,nbins1,MinvLo1,MinvHi1,nbins2,MinvLo2,MinvHi2);
  // set up difference
  char* TitDif = "MinvCorrFctnM_vs_Phi_Dif";
  sprintf(theTitle,"Dif %s",title);
  mDifference = new StHbt2DHisto(TitDif,theTitle,nbins1,MinvLo1,MinvHi1,nbins2,MinvLo2,MinvHi2);
  // this next bit is unfortunately needed so that we can have many histos of same "title"
  // it is neccessary if we typedef StHbt1DHisto to TH1d (which we do)

  mNumerator->Sumw2();
  mDenominator->Sumw2();
  mDifference->Sumw2();

  mNumerator->SetDirectory(0);
  mDenominator->SetDirectory(0);
  mDifference->SetDirectory(0);

}

//____________________________
MinvCorrFctnM_vs_Phi::~MinvCorrFctnM_vs_Phi(){
  delete mNumerator;
  delete mDenominator;
  delete mDifference;
}
//_________________________
void MinvCorrFctnM_vs_Phi::Finish(){
  // here is where we should normalize, fit, etc...
  // we should NOT Draw() the histos (as I had done it below),
  // since we want to insulate ourselves from root at this level
  // of the code.  Do it instead at root command line with browser.
  //  mNumerator->Draw();
  //  mDenominator->Draw();
  //mRatio->Draw();
  double NumeratorInt = mNumerator->Integral();
  double DenominatorInt = mDenominator->Integral();
  mDifference->Add(mNumerator,mDenominator,1.0,-1*NumeratorInt/DenominatorInt);

}

//____________________________
StHbtString MinvCorrFctnM_vs_Phi::Report(){
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
void MinvCorrFctnM_vs_Phi::AddRealPair(const StHbtPair* pair){
  mNumerator->Fill( pair->mInv(), pair->fourMomentumSum().vect().phi(), 1.);
}
//____________________________
void MinvCorrFctnM_vs_Phi::AddMixedPair(const StHbtPair* pair){
  mDenominator->Fill( pair->mInv(), pair->fourMomentumSum().vect().phi(), 1.);
}


