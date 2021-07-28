/***************************************************************************
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a simple QualityFactor correlation function used for studying 2-track cuts
 *
 ***************************************************************************
 *
 **************************************************************************/

#include "StHbtMaker/CorrFctn/QvecQualCorrFctn.h"
#include <cstdio>

#ifdef __ROOT__
ClassImp(QvecQualCorrFctn)
#endif
//____________________________
QvecQualCorrFctn::  QvecQualCorrFctn(char* title, const int& nbinsQ, const float& QLo, const float& QHi,
					     const int& nbinsQual, const float& QualLo, const float& QualHi){
  // set up numeratorS
  char Tit[100];
  sprintf(Tit,"Num2D");
  strcat(Tit,title);
  mNumerator2D = new StHbt2DHisto(Tit,title,nbinsQ,QLo,QHi,nbinsQual,QualLo,QualHi);

  // set up denominatorS
  sprintf(Tit,"Den2D");
  strcat(Tit,title);
  mDenominator2D = new StHbt2DHisto(Tit,title,nbinsQ,QLo,QHi,nbinsQual,QualLo,QualHi);

  // set up ratioS
  sprintf(Tit,"Rat2D");
  strcat(Tit,title);
  mRatio2D = new StHbt2DHisto(Tit,title,nbinsQ,QLo,QHi,nbinsQual,QualLo,QualHi);

  // these histograms should have errors associated with them...
  mNumerator2D->Sumw2();
  mDenominator2D->Sumw2();
  mRatio2D->Sumw2();

}

//____________________________
QvecQualCorrFctn::~QvecQualCorrFctn(){
  delete mNumerator2D;
  delete mDenominator2D;
  delete mRatio2D;
}
//_________________________
void QvecQualCorrFctn::Finish(){
  // here is where we should normalize, fit, etc...
  // we should NOT Draw() the histos (as I had done it below),
  // since we want to insulate ourselves from root at this level
  // of the code.  Do it instead at root command line with browser.
  mRatio2D->Divide(mNumerator2D,mDenominator2D,1.0,1.0);
}

//____________________________
StHbtString QvecQualCorrFctn::Report(){
  string stemp = "Qvector-QualityFactor Correlation Function Report:\n";
  char ctemp[100];
  sprintf(ctemp,"Number of entries in numerator:\t%E\n",
	  mNumerator2D->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in denominator:\t%E\n",
	  mDenominator2D->GetEntries());
  stemp += ctemp;
  StHbtString returnThis = stemp;
  return returnThis;
}
//____________________________
void QvecQualCorrFctn::AddRealPair(const StHbtPair* pair){

  double Qual = pair->quality();
  double Q = fabs(pair->fourMomentumDiff().vect().mag());

  mNumerator2D->Fill(Q,Qual,1.0);
}
//____________________________
void QvecQualCorrFctn::AddMixedPair(const StHbtPair* pair){

  double Qual = pair->quality();
  double Q = fabs(pair->fourMomentumDiff().vect().mag());

  mDenominator2D->Fill(Q,Qual,1.0);

}



