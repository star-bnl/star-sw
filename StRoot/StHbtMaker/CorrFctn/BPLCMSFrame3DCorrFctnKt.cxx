/***************************************************************************
 *
 * $Id: BPLCMSFrame3DCorrFctnKt.cxx,v 1.3 2003/09/02 17:58:20 perev Exp $
 *
 * Author: Mercedes Lopez Noriega, OSU, mercedes@pacific.mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   This one does 3D Bertsch-Pratt decomposition in the LCMS frame,
 *   for kt between ktLo and ktHi max for a given number of bins (nCFs)
 *   kt is the transverse four-momentum of the pair
 *
 ***************************************************************************
 *
 * $Log: BPLCMSFrame3DCorrFctnKt.cxx,v $
 * Revision 1.3  2003/09/02 17:58:20  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  2003/01/31 19:21:09  magestro
 * Cleared up simple compiler warnings on i386_linux24
 *
 * Revision 1.1  2002/05/17 14:25:31  mercedes
 * N 3D Bertsch-Pratt decomposition (kt bins) between ktmin and ktmax (LCMSFrame)
 *
 *
 **************************************************************************/

#include "StHbtMaker/CorrFctn/BPLCMSFrame3DCorrFctnKt.h"
#include <cstdio>

#ifdef __ROOT__ 
ClassImp(BPLCMSFrame3DCorrFctnKt)
#endif

//____________________________
BPLCMSFrame3DCorrFctnKt::BPLCMSFrame3DCorrFctnKt(char* title, const int& nbins, const float& QLo, const float& QHi,
						 const int& nCFs, const float& KtLo, const float& KtHi){

  // set some stuff...

  mNumberCFs = nCFs;
  mKtMin = KtLo;
  mKtMax = KtHi;
  mDeltaKt = (mKtMax-mKtMin)/mNumberCFs;

  mQinvNormLo = 0.15;
  mQinvNormHi = 0.18;
  mNumRealsNorm = 0;
  mNumMixedNorm = 0;
  mCorrection = 0;  // pointer to Coulomb Correction object

  // set up numerator
  mNumerator = new StHbt3DHisto[mNumberCFs];

  // set up denominator
  mDenominator = new StHbt3DHisto[mNumberCFs];

  // set up ratio
  mRatio = new StHbt3DHisto[mNumberCFs];

  // set up ave qInv
  mQinvHisto = new StHbt3DHisto[mNumberCFs];

  char TitNum[100] = "Num";
  strcat(TitNum,title);
  char TitDen[100] = "Den";
  strcat(TitDen,title);
  char TitRat[100] = "Rat";
  strcat(TitRat,title);
  char TitQinv[100] = "Qinv";
  strcat(TitQinv,title);
  
  for (int i=0; i<mNumberCFs; i++){
    
    sprintf(TitNum,"NumBPLCMSFrameCFKtBin%d",i);
    mNumerator[i].SetName(TitNum);
    mNumerator[i].SetTitle(title);
    mNumerator[i].SetBins(nbins,QLo,QHi,nbins,QLo,QHi,nbins,QLo,QHi);
    mNumerator[i].SetDirectory(0);
    mNumerator[i].Sumw2();
    
    sprintf(TitDen,"DenBPLCMSFrameCFKtBin%d",i);
    mDenominator[i].SetName(TitDen);
    mDenominator[i].SetTitle(title);
    mDenominator[i].SetBins(nbins,QLo,QHi,nbins,QLo,QHi,nbins,QLo,QHi);
    mDenominator[i].SetDirectory(0);
    mDenominator[i].Sumw2();
    
    sprintf(TitRat,"RatBPLCMSFrameCFKtBin%d",i);
    mRatio[i].SetName(TitRat);
    mRatio[i].SetTitle(title);
    mRatio[i].SetBins(nbins,QLo,QHi,nbins,QLo,QHi,nbins,QLo,QHi);
    mRatio[i].SetDirectory(0);
    mRatio[i].Sumw2();

    sprintf(TitQinv,"QInvHistoKtBin%d",i);
    mQinvHisto[i].SetName(TitRat);
    mQinvHisto[i].SetTitle(title);
    mQinvHisto[i].SetBins(nbins,QLo,QHi,nbins,QLo,QHi,nbins,QLo,QHi);
    mQinvHisto[i].SetDirectory(0);
    mQinvHisto[i].Sumw2();
  }
}
//____________________________
BPLCMSFrame3DCorrFctnKt::~BPLCMSFrame3DCorrFctnKt(){
  delete [] mNumerator;
  delete [] mDenominator;
  delete [] mRatio;
  delete [] mQinvHisto;
}
//_________________________
void BPLCMSFrame3DCorrFctnKt::Finish(){
  // here is where we should normalize, fit, etc...

  for (int i=0; i<mNumberCFs; i++){
    double NumFact,DenFact;
    if ((mNumRealsNorm !=0) && (mNumMixedNorm !=0)){
      NumFact = double(mNumRealsNorm);
      DenFact = double(mNumMixedNorm);
    }
    // can happen that the mNumRealsNorm and mNumMixedNorm = 0 if you do non-standard
    //   things like making a new CorrFctn and just setting the Numerator and Denominator
    //   from OTHER CorrFctns which you read in (like when doing parallel processing) 
    else{
      cout << "Warning! - no normalization constants defined - I do the best I can..." << endl;
      int nbins = mNumerator[i].GetNbinsX();
      int half_way = nbins/2;
      NumFact = mNumerator[i].Integral(half_way,nbins,half_way,nbins,half_way,nbins);
      DenFact = mDenominator[i].Integral(half_way,nbins,half_way,nbins,half_way,nbins);
    }
    
    mRatio[i].Divide(&mNumerator[i],&mDenominator[i],DenFact,NumFact);
    mQinvHisto[i].Divide(&mDenominator[i]);
  }
}

//____________________________
StHbtString BPLCMSFrame3DCorrFctnKt::Report(){
  int mNumeratorEntries=0;
  int mDenominatorEntries=0;
  int mRatioEntries=0;
  int mQinvHistoEntries=0;

  for (int i=0; i<mNumberCFs; i++){
    mNumeratorEntries += (int)mNumerator[i].GetEntries();
    mDenominatorEntries += (int)mDenominator[i].GetEntries();
    mRatioEntries += (int)mRatio[i].GetEntries();
    mQinvHistoEntries += (int)mQinvHisto[i].GetEntries();
  }

  string stemp = "LCMS Frame Bertsch-Pratt 3D Correlation Function Report:\n";
  char ctemp[100];
  sprintf(ctemp,"Number of entries in numerator:\t%i\n",mNumeratorEntries);
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in denominator:\t%i\n",mDenominatorEntries);
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in ratio:\t%i\n",mRatioEntries);
  stemp += ctemp;
  if (mCorrection)
    {
      float radius = mCorrection->GetRadius();
      sprintf(ctemp,"Coulomb correction used radius of\t%E\n",radius);
    }
  else
    {
      sprintf(ctemp,"No Coulomb Correction applied to this CorrFctn\n");
    }
  stemp += ctemp;
  
  StHbtString returnThis = stemp;
  return returnThis;
}
//____________________________
void BPLCMSFrame3DCorrFctnKt::AddRealPair(const StHbtPair* pair){
  int mIndex = (int)(((fabs(pair->kT())-mKtMin)/mDeltaKt));
  if ((mIndex>=0)&&(mIndex<mNumberCFs)){
    double Qinv = fabs(pair->qInv());   // note - qInv() will be negative for identical pairs...
    if ((Qinv < mQinvNormHi) && (Qinv > mQinvNormLo)) mNumRealsNorm++;
    double qOut = fabs(pair->qOutCMS());
    double qSide = fabs(pair->qSideCMS());
    double qLong = fabs(pair->qLongCMS());

    mNumerator[mIndex].Fill(qOut,qSide,qLong);
  }
}

//____________________________
void BPLCMSFrame3DCorrFctnKt::AddMixedPair(const StHbtPair* pair){
  int  mIndex = (int)(((fabs(pair->kT())-mKtMin)/mDeltaKt));
  if ((mIndex>=0)&&(mIndex<mNumberCFs)){
    double CoulombWeight = (mCorrection ? mCorrection->CoulombCorrect(pair) : 1.0);

    double Qinv = fabs(pair->qInv());   // note - qInv() will be negative for identical pairs...
    if ((Qinv < mQinvNormHi) && (Qinv > mQinvNormLo)) mNumMixedNorm++;
    double qOut = fabs(pair->qOutCMS());
    double qSide = fabs(pair->qSideCMS());
    double qLong = fabs(pair->qLongCMS());

    mDenominator[mIndex].Fill(qOut,qSide,qLong,CoulombWeight);
    mQinvHisto[mIndex].Fill(qOut,qSide,qLong,Qinv);
  }
}

