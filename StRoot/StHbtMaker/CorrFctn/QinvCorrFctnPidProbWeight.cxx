/***************************************************************************
 *
 * $Id: QinvCorrFctnPidProbWeight.cxx,v 1.1 2001/09/05 20:41:14 laue Exp $
 *
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a simple Q-invariant correlation function with pid weighting          
 *
 ***************************************************************************
 *
 * $Log: QinvCorrFctnPidProbWeight.cxx,v $
 * Revision 1.1  2001/09/05 20:41:14  laue
 * Updates of the hbtMuDstTree microDSTs
 *
 *
 **************************************************************************/

#include "StHbtMaker/CorrFctn/QinvCorrFctnPidProbWeight.h"
//#include "StHbtMaker/Infrastructure/StHbtHisto.hh"
#include <cstdio>

#ifdef __ROOT__ 
ClassImp(QinvCorrFctnPidProbWeight)
#endif

//____________________________
QinvCorrFctnPidProbWeight::QinvCorrFctnPidProbWeight(char* title1, char* title2, const int& nbins, const float& QinvLo, const float& QinvHi){
  // set up numerator
  //  title = "Num Qinv (MeV/c)";
  char TitNum[100] = "Num";
  strcat(TitNum,title1);
  mNumerator = new StHbt1DHisto(TitNum,title2,nbins,QinvLo,QinvHi);
  // set up denominator
  //title1 = "Den Qinv (MeV/c)";
  char TitDen[100] = "Den";
  strcat(TitDen,title1);
  mDenominator = new StHbt1DHisto(TitDen,title2,nbins,QinvLo,QinvHi);
  // set up ratio
  //title1 = "Ratio Qinv (MeV/c)";
  char TitRat[100] = "Rat";
  strcat(TitRat,title1);
  mRatio = new StHbt1DHisto(TitRat,title2,nbins,QinvLo,QinvHi);
  // this next bit is unfortunately needed so that we can have many histos of same "title"
  // it is neccessary if we typedef StHbt1DHisto to TH1d (which we do)
  //mNumerator->SetDirectory(0);
  //mDenominator->SetDirectory(0);
  //mRatio->SetDirectory(0);

  // to enable error bar calculation...
  mNumerator->Sumw2();
  mDenominator->Sumw2();
  mRatio->Sumw2();

}

//____________________________
QinvCorrFctnPidProbWeight::~QinvCorrFctnPidProbWeight(){
  delete mNumerator;
  delete mDenominator;
  delete mRatio;
}
//_________________________
void QinvCorrFctnPidProbWeight::Finish(){
  // here is where we should normalize, fit, etc...
  // we should NOT Draw() the histos (as I had done it below),
  // since we want to insulate ourselves from root at this level
  // of the code.  Do it instead at root command line with browser.
  //  mNumerator->Draw();
  //mDenominator->Draw();
  //mRatio->Draw();
  mRatio->Divide(mNumerator,mDenominator,1.0,1.0);

}

//____________________________
StHbtString QinvCorrFctnPidProbWeight::Report(){
  string stemp = "Qinv Correlation Function Report:\n";
  char ctemp[100];
  sprintf(ctemp,"Number of entries in numerator:\t%E\n",mNumerator->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in denominator:\t%E\n",mDenominator->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in ratio:\t%E\n",mRatio->GetEntries());
  stemp += ctemp;
  //  stemp += mCoulombWeight->Report();
  StHbtString returnThis = stemp;
  return returnThis;
}
//____________________________
void QinvCorrFctnPidProbWeight::AddRealPair(const StHbtPair* pair){
  mNumerator->Fill(pair->qInv(),pair->track1()->Track()->PidProbKaon()*pair->track2()->Track()->PidProbKaon());
}
//____________________________
void QinvCorrFctnPidProbWeight::AddMixedPair(const StHbtPair* pair){
  mDenominator->Fill(pair->qInv(),pair->track1()->Track()->PidProbKaon()*pair->track2()->Track()->PidProbKaon());
}


