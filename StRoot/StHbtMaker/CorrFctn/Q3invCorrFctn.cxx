/***************************************************************************
 *
 * $Id: Q3invCorrFctn.cxx,v 1.4 2001/06/03 21:05:42 willson Exp $
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
 * Revision 1.4  2001/06/03 21:05:42  willson
 * Bins in entrance separation
 *
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
Q3invCorrFctn::Q3invCorrFctn(char* title, const int& nbinsQ, const float& QinvLo, const float& QinvHi, const int& nbinsMerge, const float& MergeLo, const float& MergeHi, const float& Split){
  // This one does not do a quality calulation
  mSplit = Split;
  mPHist = 0;
  // set up numerator
  //  title = "Num Q3inv (MeV/c)";
  char TitNum[100] = "Num";
  strcat(TitNum,title);
  mNumerator = new StHbt2DHisto(TitNum,title,nbinsQ,QinvLo,QinvHi,nbinsMerge,MergeLo,MergeHi);
  // set up denominator
  //title = "Den Q3inv (MeV/c)";
  char TitDen[100] = "Den";
  strcat(TitDen,title);
  mDenominator = new StHbt2DHisto(TitDen,title,nbinsQ,QinvLo,QinvHi,nbinsMerge,MergeLo,MergeHi);
  // set up ratio
  //title = "Ratio Q3inv (MeV/c)";
  char TitRat[100] = "Rat";
  strcat(TitRat,title);
  mRatio = new StHbt2DHisto(TitRat,title,nbinsQ,QinvLo,QinvHi,nbinsMerge,MergeLo,MergeHi);
  // this next bit is unfortunately needed so that we can have many histos of same "title"
  // it is neccessary if we typedef StHbt1DHisto to TH1d (which we do)
  //mNumer->SetDirectory(0);
  //mDenom->SetDirectory(0);
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
  if (mPHist) {
    mPHist->Fill(triplet->track1()->Track()->P().mag());
    mPHist->Fill(triplet->track2()->Track()->P().mag());
    mPHist->Fill(triplet->track3()->Track()->P().mag());
  }
  double Qual = triplet->quality();
  double entSep = triplet->NominalTpcEntranceSeparation();
  double Q3inv = fabs(triplet->qInv());   // note - qInv() will be negative for identical triplets...  
  if (Qual<mSplit) mNumerator->Fill(Q3inv, entSep, 1.0);
}

//____________________________
void Q3invCorrFctn::AddMixedTriplet(const StHbtTriplet* triplet){
  double weight = 1.0;
  if (mCorrection.GetRadius()!=-1) {
    StHbtPair pair1(triplet->track1(), triplet->track2());
    StHbtPair pair2(triplet->track2(), triplet->track3());
    StHbtPair pair3(triplet->track3(), triplet->track1());
    
    double weight1 = mCorrection.CoulombCorrect(&pair1);
    double weight2 = mCorrection.CoulombCorrect(&pair2);
    double weight3 = mCorrection.CoulombCorrect(&pair3);
    
    weight = weight1*weight2*weight3;
  }
  else 
    weight = 1.0;
  
  double Qual = triplet->quality();
  double entSep = triplet->NominalTpcEntranceSeparation();
  double Q3inv = fabs(triplet->qInv());   // note - qInv() will be negative for identical triplets...  
  if (Qual<mSplit) mDenominator->Fill(Q3inv, entSep, weight);
}
//____________________________
void Q3invCorrFctn::AddCorrection(StHbtCoulomb* correction) {
  mCorrection = *correction;
}
