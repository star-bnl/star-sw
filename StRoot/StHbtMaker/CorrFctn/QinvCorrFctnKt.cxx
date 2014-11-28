/***************************************************************************
 *
 * $Id: QinvCorrFctnKt.cxx,v 1.3 2003/09/02 17:58:20 perev Exp $
 *
 * Author: Mercedes Lopez Noriega, OSU, mercedes@pacific.mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   QinvCFs for a given number of CFs (nCFs) between ktLo and ktHi 
 *   where kt is the four-momentum of the pair 
 *
 ***************************************************************************
 *
 * $Log: QinvCorrFctnKt.cxx,v $
 * Revision 1.3  2003/09/02 17:58:20  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  2003/01/31 19:21:09  magestro
 * Cleared up simple compiler warnings on i386_linux24
 *
 * Revision 1.1  2002/05/17 14:26:21  mercedes
 * N Qinv CFs (kt bins) between ktmin and ktmax
 *
 *
 **************************************************************************/

#include "StHbtMaker/CorrFctn/QinvCorrFctnKt.h"
//#include "StHbtMaker/Infrastructure/StHbtHisto.hh"
#include <cstdio>

#ifdef __ROOT__ 
ClassImp(QinvCorrFctnKt)
#endif

//____________________________
QinvCorrFctnKt::QinvCorrFctnKt(char* title, const int& nbins, const float& QinvLo, const float& QinvHi,
			       const int& nCFs, const float& KtLo, const float& KtHi){
  mNumberCFs = nCFs;
  mKtMin = KtLo;
  mKtMax = KtHi;
  mDeltaKt = (mKtMax-mKtMin)/mNumberCFs;

  // set up numerators
  mNumerator = new StHbt1DHisto[mNumberCFs];

  // set up denominators
  mDenominator = new StHbt1DHisto[mNumberCFs];

  // set up ratios
  mRatio = new StHbt1DHisto[mNumberCFs];

  char TitNum[100] = "Num";
  strcat(TitNum,title);
  char TitDen[100] = "Den";
  strcat(TitDen,title);
  char TitRat[100] = "Rat";
  strcat(TitRat,title);

  for (int i=0; i<mNumberCFs; i++){
    
    sprintf(TitNum,"NumQInvCFKtBin%d",i);
    mNumerator[i].SetName(TitNum);
    mNumerator[i].SetTitle(title);
    mNumerator[i].SetBins(nbins,QinvLo,QinvHi);
    mNumerator[i].SetDirectory(0);
    mNumerator[i].Sumw2();
    
    sprintf(TitDen,"DenQInvCFKtBin%d",i);
    mDenominator[i].SetName(TitDen);
    mDenominator[i].SetTitle(title);
    mDenominator[i].SetBins(nbins,QinvLo,QinvHi);
    mDenominator[i].SetDirectory(0);
    mDenominator[i].Sumw2();
    
    sprintf(TitRat,"RatQInvCFKtBin%d",i);
    mRatio[i].SetName(TitRat);
    mRatio[i].SetTitle(title);
    mRatio[i].SetBins(nbins,QinvLo,QinvHi);
    mRatio[i].SetDirectory(0);
    mRatio[i].Sumw2();
  }
}
//____________________________
QinvCorrFctnKt::~QinvCorrFctnKt(){
  delete [] mNumerator;
  delete [] mDenominator;
  delete [] mRatio;
}
//_________________________
void QinvCorrFctnKt::Finish(){
  for (int i=0; i<mNumberCFs; i++){
    mRatio[i].Divide(&mNumerator[i],&mDenominator[i],1.0,1.0);
  }
}
//____________________________
StHbtString QinvCorrFctnKt::Report(){
  int mNumeratorEntries=0;
  int mDenominatorEntries=0;
  int mRatioEntries=0;

  for (int i=0; i<mNumberCFs; i++){
    mNumeratorEntries += (int)mNumerator[i].GetEntries();
    mDenominatorEntries += (int)mDenominator[i].GetEntries();
    mRatioEntries += (int)mRatio[i].GetEntries();
  }
  string stemp = "Qinv Correlation Function Report:\n";
  char ctemp[100];
  sprintf(ctemp,"Number of entries in all numerators:\t%i\n",mNumeratorEntries);
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in all denominators:\t%i\n",mDenominatorEntries);
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in all ratios:\t%i\n",mRatioEntries);
  stemp += ctemp;
  StHbtString returnThis = stemp;
  return returnThis;
}
//____________________________
void QinvCorrFctnKt::AddRealPair(const StHbtPair* pair){
  int mIndex = (int)(((fabs(pair->kT())-mKtMin)/mDeltaKt));
  if ((mIndex>=0)&&(mIndex<mNumberCFs)){
    mNumerator[mIndex].Fill(fabs(pair->qInv()),1.0);
  }
}
//____________________________
void QinvCorrFctnKt::AddMixedPair(const StHbtPair* pair){
  int  mIndex = (int)(((fabs(pair->kT())-mKtMin)/mDeltaKt));
  if ((mIndex>=0)&&(mIndex<mNumberCFs)){
    mDenominator[mIndex].Fill(fabs(pair->qInv()),1.0);
  }
}


