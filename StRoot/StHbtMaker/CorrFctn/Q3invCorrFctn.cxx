/***************************************************************************
 *
 * $Id: Q3invCorrFctn.cxx,v 1.2 2000/04/12 01:53:28 willson Exp $
 *
 * Author: Robert Willson, Ohio State, willson@bnl.gov
 ***************************************************************************
 *  
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   A simple Q-invariant correlation function for three particle analyses.    
 * 
 ***************************************************************************
 *
 * $Log: Q3invCorrFctn.cxx,v $
 * Revision 1.2  2000/04/12 01:53:28  willson
 * Initial Installation - Comments Added
 *
 * 
 ***************************************************************************/

#include "StHbtMaker/CorrFctn/Q3invCorrFctn.h"
//#include "StHbtMaker/Infrastructure/StHbtHisto.hh"
#include <cstdio>

ClassImp(Q3invCorrFctn)

//____________________________
Q3invCorrFctn::Q3invCorrFctn(char* title, const int& nbins, const float& QinvLo, const float& QinvHi){
  // set up numerator
  //  title = "Num Q3inv (MeV/c)";
  char TitNum[100] = "Num";
  strcat(TitNum,title);
  mNumerator = new StHbt1DHisto(TitNum,title,nbins,QinvLo,QinvHi);
  // set up denominator
  //title = "Den Q3inv (MeV/c)";
  char TitDen[100] = "Den";
  strcat(TitDen,title);
  mDenominator = new StHbt1DHisto(TitDen,title,nbins,QinvLo,QinvHi);
  // set up ratio
  //title = "Ratio Q3inv (MeV/c)";
  char TitRat[100] = "Rat";
  strcat(TitRat,title);
  mRatio = new StHbt1DHisto(TitRat,title,nbins,QinvLo,QinvHi);
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
Q3invCorrFctn::~Q3invCorrFctn(){
  delete mNumerator;
  delete mDenominator;
  delete mRatio;
}
//_________________________
void Q3invCorrFctn::Finish(){
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
StHbtString Q3invCorrFctn::Report(){
  string stemp = "Q3inv Correlation Function Report:\n";
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
void Q3invCorrFctn::AddRealTriplet(const StHbtTriplet* triplet){
  double Q3inv = fabs(triplet->qInv());   // note - qInv() will be negative for identical triplets...
  mNumerator->Fill(Q3inv);
  //  cout << "Q3invCorrFctn::AddRealTriplet : " << triplet->qInv() << " " << Q3inv <<
  //" " << triplet->track1().FourMomentum() << " " << triplet->track2().FourMomentum() << endl;
}
//____________________________
void Q3invCorrFctn::AddMixedTriplet(const StHbtTriplet* triplet){
  double weight = 1.0;
  double Q3inv = fabs(triplet->qInv());   // note - qInv() will be negative for identical triplets...
  mDenominator->Fill(Q3inv,weight);
}


