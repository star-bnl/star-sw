/***************************************************************************
 *
 * Author: Mercedes Lopez Noriega, OSU, mercedes@pacific.mps.ohio-state.edu
 *
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   2D correlation function: Qinv vs. Fraction of merged rows.
 *
 **************************************************************************/

#include "TMath.h"
#include "StHbtMaker/CorrFctn/FracMergRowvsQinv.h"
#include <cstdio>

#ifdef __ROOT__ 
ClassImp(FracMergRowvsQinv)
#endif

//____________________________
  FracMergRowvsQinv::FracMergRowvsQinv(char* title, const int& nbinsX, const float& XLo, const float& XHi,
					   const int& nbinsY, const float& YLo, const float& YHi){
  // set up numerator
  char Tit[100];
  sprintf(Tit,"Num2D");
  strcat(Tit,title);
  mNumerator2D = new StHbt2DHisto(Tit,title,nbinsX,XLo,XHi,nbinsY,YLo,YHi);

  // set up denominator
  sprintf(Tit,"Den2D");
  strcat(Tit,title);
  mDenominator2D = new StHbt2DHisto(Tit,title,nbinsX,XLo,XHi,nbinsY,YLo,YHi);

  // set up ratio
  sprintf(Tit,"Rat2D");
  strcat(Tit,title);
  mRatio2D = new StHbt2DHisto(Tit,title,nbinsX,XLo,XHi,nbinsY,YLo,YHi);
  
  mNumerator2D->Sumw2();
  mDenominator2D->Sumw2();
  mRatio2D->Sumw2();
}
//____________________________
FracMergRowvsQinv::~FracMergRowvsQinv(){
  delete mNumerator2D;
  delete mDenominator2D;
  delete mRatio2D; 
}
//_________________________
void FracMergRowvsQinv::Finish(){
  mRatio2D->Divide(mNumerator2D,mDenominator2D,1.0,1.0);
}


//____________________________
StHbtString FracMergRowvsQinv::Report(){
  string stemp = "Qinv Correlation Function Report:\n";
  char ctemp[100];
  sprintf(ctemp,"Number of entries in numerator:\t%E\n",mNumerator2D->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in denominator:\t%E\n",mDenominator2D->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in ratio:\t%E\n",mRatio2D->GetEntries());
  stemp += ctemp;
  StHbtString returnThis = stemp;
  return returnThis;
}
//____________________________
void FracMergRowvsQinv::AddRealPair(const StHbtPair* pair){
   double Qinv = fabs(pair->qInv()); 
   mNumerator2D->Fill(Qinv,
		      pair->getFracOfMergedRow(),1.0);
}

//____________________________
void FracMergRowvsQinv::AddMixedPair(const StHbtPair* pair){
  double Qinv = fabs(pair->qInv()); 
  mDenominator2D->Fill(Qinv,
		       pair->getFracOfMergedRow(),1.0);
}
