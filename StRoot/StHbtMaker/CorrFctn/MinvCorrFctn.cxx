/***************************************************************************
 *
 * $Id: MinvCorrFctn.cxx,v 1.1.1.1 1999/06/29 16:02:57 lisa Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *     A simple invariant-mass correlation function
 *
 ***************************************************************************
 *
 * $Log: MinvCorrFctn.cxx,v $
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/


#include "StHbtMaker/CorrFctn/MinvCorrFctn.h"
#include "StHbtMaker/Infrastructure/StHbtHisto.hh"
#include <cstdio>

ClassImp(MinvCorrFctn)

//____________________________
MinvCorrFctn::MinvCorrFctn(char* title, const int& nbins, const float& MinvLo, const float& MinvHi){
  // set up numerator
  char TitNum[100] = "Num";
  strcat(TitNum,title);
  mNumerator = new StHbt1DHisto(TitNum,title,nbins,MinvLo,MinvHi);
  // set up denominator
  char TitDen[100] = "Den";
  strcat(TitDen,title);
  mDenominator = new StHbt1DHisto(TitDen,title,nbins,MinvLo,MinvHi);
  // set up difference
  char TitDif[100] = "Dif";
  strcat(TitDif,title);
  mDifference = new StHbt1DHisto(TitDif,title,nbins,MinvLo,MinvHi);
  // this next bit is unfortunately needed so that we can have many histos of same "title"
  // it is neccessary if we typedef StHbt1DHisto to TH1d (which we do)
  //mNumerator->SetDirectory(0);
  //mDenominator->SetDirectory(0);
  //mRatio->SetDirectory(0);
}

//____________________________
MinvCorrFctn::~MinvCorrFctn(){
  delete mNumerator;
  delete mDenominator;
  delete mDifference;
}
//_________________________
void MinvCorrFctn::Finish(){
  // here is where we should normalize, fit, etc...
  // we should NOT Draw() the histos (as I had done it below),
  // since we want to insulate ourselves from root at this level
  // of the code.  Do it instead at root command line with browser.
  //  mNumerator->Draw();
  //  mDenominator->Draw();
  //mRatio->Draw();
  double MumeratorInt = mNumerator->Integral();
  double DenominatorInt = mDenominator->Integral();
  mDifference->Add(mNumerator,mDenominator,1.0,-1*MumeratorInt/DenominatorInt);

}

//____________________________
string MinvCorrFctn::Report(){
  string stemp = "Minv Correlation Function Report:\n";
  char ctemp[100];
  sprintf(ctemp,"Number of entries in numerator:\t%E\n",mNumerator->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in denominator:\t%E\n",mDenominator->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in difference:\t%E\n",mDifference->GetEntries());
  stemp += ctemp;
  return stemp;
}
//____________________________
void MinvCorrFctn::AddRealPair(const StHbtPair* pair){
  double Minv = fabs(pair->mInv());   
  mNumerator->Fill(Minv);
  //  cout << "MinvCorrFctn::AddRealPair : " << pair->mInv() << " " << Minv <<
  //" " << pair->track1().FourMomentum() << " " << pair->track2().FourMomentum() << endl;
}
//____________________________
void MinvCorrFctn::AddMixedPair(const StHbtPair* pair){
  double weight = 1.0;
  double Minv = fabs(pair->mInv());   
  mDenominator->Fill(Minv,weight);
}


