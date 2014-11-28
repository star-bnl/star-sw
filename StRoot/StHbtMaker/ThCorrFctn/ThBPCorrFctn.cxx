/***************************************************************************
 *
 * $Id: 
 *
 * Author: Laurent Conin, Fabrice Retiere, Subatech, France
 ***************************************************************************
 *
 * Description : Calculate the theoretical QInv correlation function 
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#include "Stsstream.h"
#include "StHbtMaker/ThCorrFctn/ThBPCorrFctn.h"

#ifdef __ROOT__
ClassImp(ThBPCorrFctn)
#endif

//____________________________
void ThBPCorrFctn::AddNum(StHbtThPair* aThPair){

  double tQinv = 2 * fabs(aThPair->GetMeasPair()->KStar());   // note - qInv() will be negative for identical pairs...
  if ((tQinv < mQinvNormHi) && (tQinv > mQinvNormLo)) mNumRealsNorm++;
  double qOut = aThPair->GetMeasPair()->qOutCMS();
  double qSide = aThPair->GetMeasPair()->qSideCMS();
  double qLong = fabs(aThPair->GetMeasPair()->qLongCMS());

  //  cout << "Components Num: inv out side long: " << tQinv << " " << qOut << " " << qSide << " " << qLong << endl;
  mNumerator->Fill(qOut,qSide,qLong,aThPair->GetWeightNum());
}
void ThBPCorrFctn::AddDen(StHbtThPair* aThPair){

  double tQinv= 2 * fabs(aThPair->GetMeasPair()->KStar());
  if ((tQinv < mQinvNormHi) && (tQinv > mQinvNormLo)) mNumMixedNorm++;
  double qOut = aThPair->GetMeasPair()->qOutCMS();
  double qSide = aThPair->GetMeasPair()->qSideCMS();
  double qLong = fabs(aThPair->GetMeasPair()->qLongCMS());

  if ((addedHistos == 1) && (tQinv < 0.1)) {
    add2DHistos[0]->Fill(fabs(qOut), -(fabs(qOut) - fabs(aThPair->RealqOutCMS())), 1.0);
    add2DHistos[1]->Fill(fabs(qSide), -(fabs(qSide) - fabs(aThPair->RealqSideCMS())), 1.0);
    add2DHistos[2]->Fill(fabs(qLong), -(fabs(qLong) - fabs(aThPair->RealqLongCMS())), 1.0);
  }

  //  cout << "Components Den: inv out side long: " << tQinv << " " << qOut << " " << qSide << " " << qLong << endl;
  mDenominator->Fill(qOut,qSide,qLong,aThPair->GetWeightDen());
  mQinvHisto->Fill(qOut,qSide,qLong,tQinv);
}

void ThBPCorrFctn::Finish(){
  // here is where we should normalize, fit, etc...
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
    int nbins = mNumerator->GetNbinsX();
    int half_way = nbins/2;
    NumFact = mNumerator->Integral(half_way,nbins,half_way,nbins,half_way,nbins);
    DenFact = mDenominator->Integral(half_way,nbins,half_way,nbins,half_way,nbins);
  }
  
  mRatio->Divide(mNumerator,mDenominator,DenFact,NumFact);
  mQinvHisto->Divide(mDenominator);
}

ThBPCorrFctn::ThBPCorrFctn(char* aTitle, int aNBins, 
			   double aHLo, double aHHi, int addHistos)
{
  // set some stuff...
  mQinvNormLo = 0.15;
  mQinvNormHi = 0.18;
  mNumRealsNorm = 0;
  mNumMixedNorm = 0;

  // set up numerator
  char TitNum[100] = "Num";
  strcat(TitNum,aTitle);
  mNumerator = new StHbt3DHisto(TitNum,aTitle,aNBins,aHLo,aHHi,aNBins,aHLo,aHHi,aNBins/2,0.0,aHHi);
  // set up denominator
  char TitDen[100] = "Den";
  strcat(TitDen,aTitle);
  mDenominator = new StHbt3DHisto(TitDen,aTitle,aNBins,aHLo,aHHi,aNBins,aHLo,aHHi,aNBins/2,0.0,aHHi);
  // set up ratio
  char TitRat[100] = "Rat";
  strcat(TitRat,aTitle);
  mRatio = new StHbt3DHisto(TitRat,aTitle,aNBins,aHLo,aHHi,aNBins,aHLo,aHHi,aNBins/2,0.0,aHHi);
  // set up ave qInv
  char TitQinv[100] = "Qinv";
  strcat(TitQinv,aTitle);
  mQinvHisto = new StHbt3DHisto(TitQinv,aTitle,aNBins,aHLo,aHHi,aNBins,aHLo,aHHi,aNBins/2,0.0,aHHi);

  // to enable error bar calculation...
  mNumerator->Sumw2();
  mDenominator->Sumw2();
  mRatio->Sumw2();
  
  addedHistos = addHistos;
  if (addHistos == 1) {
    Int_t qdepNBins = 10;
    Int_t qdiffNBins = 50;
    Float_t qdepMin = 0.0, qdepMax=0.1;
    Float_t qdiffMin = -0.025, qdiffMax = 0.025;
    numAdd2DHistos = 3;
    typedef StHbt2DHisto* StHbt2DHistoP;
    add2DHistos = new StHbt2DHistoP[3];
    char Tit1add[100] = "Qout";
    strcat(Tit1add,aTitle);
    add2DHistos[0] = new StHbt2DHisto(Tit1add,aTitle,qdepNBins, qdepMin, qdepMax, qdiffNBins, qdiffMin, qdiffMax);
    char Tit2add[100] = "Qside";
    strcat(Tit2add,aTitle);
    add2DHistos[1] = new StHbt2DHisto(Tit2add,aTitle,qdepNBins, qdepMin, qdepMax, qdiffNBins, qdiffMin, qdiffMax);
    char Tit3add[100] = "Qlong";
    strcat(Tit3add,aTitle);
    add2DHistos[2] = new StHbt2DHisto(Tit3add,aTitle,qdepNBins, qdepMin, qdepMax, qdiffNBins, qdiffMin, qdiffMax);
  }
}

ThBPCorrFctn::ThBPCorrFctn(const ThBPCorrFctn& ThCf)
{
  mQinvNormLo = ThCf.mQinvNormLo;
  mQinvNormHi = ThCf.mQinvNormHi;
  mNumRealsNorm = ThCf.mNumRealsNorm;
  mNumMixedNorm = ThCf.mNumMixedNorm;
  
  mNumerator = new StHbt3DHisto(*ThCf.mNumerator);
  mDenominator = new StHbt3DHisto(*ThCf.mDenominator);
  mRatio = new StHbt3DHisto(*ThCf.mRatio);
  mQinvHisto = new StHbt3DHisto(*ThCf.mQinvHisto);
}

StHbtString ThBPCorrFctn::Report() 
{
  ostrstream *out = new ostrstream();
  (*out) << "Report form the ThBPLCMS Correlation function" << endl;
  return out->str();
}

ThBPCorrFctn::~ThBPCorrFctn() 
{
  delete mNumerator;
  delete mDenominator;
  delete mRatio;
  delete mQinvHisto;
  if (addedHistos > 0) {
    for (int i=0; i<numAdd2DHistos; i++)
      delete add2DHistos[i];
    delete add2DHistos;
  }
}

void ThBPCorrFctn::Write()  {
  mNumerator->Write();mDenominator->Write();mRatio->Write();
  if (addedHistos > 0) {
    for (int i=0; i<numAdd2DHistos; i++)
      add2DHistos[i]->Write();
  }
};

inline StHbt3DHisto* ThBPCorrFctn::Numerator() const {  return mNumerator;};
inline StHbt3DHisto* ThBPCorrFctn::Denominator() const { return mDenominator;};
inline StHbt3DHisto* ThBPCorrFctn::Ratio() const { return mRatio;};
inline StHbtThCorrFctn* ThBPCorrFctn::ThClone() const {return new ThBPCorrFctn(*this);}
