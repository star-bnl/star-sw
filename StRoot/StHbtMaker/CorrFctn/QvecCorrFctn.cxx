/***************************************************************************
 *
 * $Id: QvecCorrFctn.cxx,v 1.1.1.1 1999/06/29 16:02:57 lisa Exp $
 *
 * Author: Randy Wells, Ohio State, rcwells@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *      a simple correlation function in the magnitude of 3-vector q        
 *
 ***************************************************************************
 *
 * $Log: QvecCorrFctn.cxx,v $
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#include "StHbtMaker/CorrFctn/QvecCorrFctn.h"
#include "StHbtMaker/Infrastructure/StHbtHisto.hh"
#include <cstdio>
#ifndef StMaker_H
#include "StMaker.h"
#endif

ClassImp(QvecCorrFctn)

//____________________________
QvecCorrFctn::QvecCorrFctn(char* title, const int& nbins, const float& QinvLo, const float& QinvHi){
  // set up numerator
  //  title = "Num Qinv (MeV/c)";
  char TitNum[100] = "Num";
  strcat(TitNum,title);
  mNumerator = new StHbt1DHisto(TitNum,title,nbins,QinvLo,QinvHi);
  // set up denominator
  //title = "Den Qinv (MeV/c)";
  char TitDen[100] = "Den";
  strcat(TitDen,title);
  mDenominator = new StHbt1DHisto(TitDen,title,nbins,QinvLo,QinvHi);
  // set up ratio
  //title = "Ratio Qinv (MeV/c)";
  char TitRat[100] = "Rat";
  strcat(TitRat,title);
  mRatio = new StHbt1DHisto(TitRat,title,nbins,QinvLo,QinvHi);
  // this next bit is unfortunately needed so that we can have many histos of same "title"
  // it is neccessary if we typedef StHbt1DHisto to TH1d (which we do)
  //mNumerator->SetDirectory(0);
  //mDenominator->SetDirectory(0);
  //mRatio->SetDirectory(0);
}

//____________________________
QvecCorrFctn::~QvecCorrFctn(){
  delete mNumerator;
  delete mDenominator;
  delete mRatio;
}
//_________________________
void QvecCorrFctn::Finish(){
  // here is where we should normalize, fit, etc...
  // we should NOT Draw() the histos (as I had done it below),
  // since we want to insulate ourselves from root at this level
  // of the code.  Do it instead at root command line with browser.
  //  mNumerator->Draw();
  //  mDenominator->Draw();
  int mTop = mNumerator->GetBinContent(52);
  int mBottom = mDenominator->GetBinContent(52);
  mRatio->Divide(mNumerator,mDenominator,mBottom,mTop,"PE");
  //mRatio->Draw();
}

//____________________________
string QvecCorrFctn::Report(){
  string stemp = "Qvec Correlation Function Report:\n";
  char ctemp[100];
  sprintf(ctemp,"Number of entries in numerator:\t%E\n",mNumerator->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in denominator:\t%E\n",mDenominator->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in ratio:\t%E\n",mRatio->GetEntries());
  stemp += ctemp;
  //  stemp += mCoulombWeight->Report();
  return stemp;
}
//____________________________
void QvecCorrFctn::AddRealPair(const StHbtPair* pair){
  double Q = fabs(pair->fourMomentum().vect().mag());
  mNumerator->Fill(Q);
}
//____________________________
void QvecCorrFctn::AddMixedPair(const StHbtPair* pair){
  double weight = 1.0;
  double Q = fabs(pair->fourMomentum().vect().mag());
  mDenominator->Fill(Q,weight);
}


