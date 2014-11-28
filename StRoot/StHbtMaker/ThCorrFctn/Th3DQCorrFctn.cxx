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

#include "StHbtMaker/ThCorrFctn/Th3DQCorrFctn.h"

#ifdef __ROOT__
ClassImp(Th3DQCorrFctn)
#endif
  
Th3DQCorrFctn::Th3DQCorrFctn(char* aTitle, int aNBins, 
			     double aHLo, double aHHi)
  : StHbtThCorrFctn() ,StHbtRoot1DCF(aTitle, aNBins, aHLo, aHHi) 
{
  char tit[100] = "";

  Int_t qnbins = 100;
  Float_t qmax = 1.0;
  Float_t qmin = -1.0;
  Float_t dmin = -0.2;
  Float_t dmax = 0.2;
  strcpy(tit,"Outdist");
  strcat(tit,aTitle); 
  qOutdist = new StHbt1DHisto(tit,aTitle,qnbins,qmin,qmax);
  strcpy(tit,"OutSdist");
  strcat(tit,aTitle); 
  qOutSdist = new StHbt1DHisto(tit,aTitle,qnbins,qmin,qmax);
  strcpy(tit,"DeltaOutdist");
  strcat(tit,aTitle); 
  DeltaqOutdist = new StHbt1DHisto(tit,aTitle,qnbins,dmin,dmax);
  strcpy(tit,"Sidedist");
  strcat(tit,aTitle);  
  qSidedist = new StHbt1DHisto(tit,aTitle,qnbins,qmin,qmax);
  strcpy(tit,"SideSdist");
  strcat(tit,aTitle); 
  qSideSdist = new StHbt1DHisto(tit,aTitle,qnbins,qmin,qmax);
  strcpy(tit,"DeltaSidedist");
  strcat(tit,aTitle); 
  DeltaqSidedist = new StHbt1DHisto(tit,aTitle,qnbins,dmin,dmax);
  strcpy(tit,"Longdist");
  strcat(tit,aTitle);  
  qLongdist = new StHbt1DHisto(tit,aTitle,qnbins,qmin,qmax);
  strcpy(tit,"LongSdist");
  strcat(tit,aTitle); 
  qLongSdist = new StHbt1DHisto(tit,aTitle,qnbins,qmin,qmax);
  strcpy(tit,"DeltaLongdist");
  strcat(tit,aTitle); 
  DeltaqLongdist = new StHbt1DHisto(tit,aTitle,qnbins,dmin,dmax);
 
  qOutdist->Sumw2();
  qOutSdist->Sumw2();
  DeltaqOutdist->Sumw2();
  qSidedist->Sumw2();
  qSideSdist->Sumw2();
  DeltaqSidedist->Sumw2();
  qLongdist->Sumw2();
  qLongSdist->Sumw2();
  DeltaqLongdist->Sumw2();
};

Th3DQCorrFctn::Th3DQCorrFctn(const Th3DQCorrFctn& ThCf) 
  : StHbtThCorrFctn(ThCf),StHbtRoot1DCF( ThCf) 
{ /* no-op */ }
  
Th3DQCorrFctn::~Th3DQCorrFctn() 
{ /* no-op */ };

//____________________________
void Th3DQCorrFctn::AddNum(StHbtThPair* aThPair){

  double tQInv=fabs(aThPair->GetMeasPair()->KStar());
  if(tQInv>=mHLo && tQInv<=mHHi){
    mNumerator->Fill(tQInv,aThPair->GetWeightNum());
  }
}

void Th3DQCorrFctn::AddDen(StHbtThPair* aThPair){

  double tQInv=2*fabs(aThPair->GetMeasPair()->KStar());
  if(tQInv>=mHLo && tQInv<=mHHi){
    mDenominator->Fill(tQInv,aThPair->GetWeightDen());

  }
  if (tQInv<0.1) {
    double qOut = aThPair->GetMeasPair()->qOutCMS();
    double qSide = aThPair->GetMeasPair()->qSideCMS();
    double qLong = aThPair->GetMeasPair()->qLongCMS();
    
    double RqOut = aThPair->RealqOutCMS();
    double RqSide = aThPair->RealqSideCMS();
    double RqLong = aThPair->RealqLongCMS();
    
    qOutdist->Fill(qOut);
    qOutSdist->Fill(RqOut);
    DeltaqOutdist->Fill(qOut - RqOut);
    qSidedist->Fill(qSide);
    qSideSdist->Fill(RqSide);
    DeltaqSidedist->Fill(qSide - RqSide);
    qLongdist->Fill(qLong);
    qLongSdist->Fill(RqLong);
    DeltaqLongdist->Fill(qLong - RqLong);
  }
}

void Th3DQCorrFctn::Finish(){
  StHbtRoot1DCF::Finish();
}

inline StHbt1DHisto* Th3DQCorrFctn::Numerator() const { cout << "Return Ratio" << endl; return mNumerator;};
inline StHbt1DHisto* Th3DQCorrFctn::Denominator() const {cout << "Return Denimnatior" << endl; return mDenominator;};
inline StHbt1DHisto* Th3DQCorrFctn::Ratio() const {cout << "Return Numerator" << endl; return mRatio;};
inline void Th3DQCorrFctn::Write()  {
  mNumerator->Write();mDenominator->Write();mRatio->Write();
  qOutdist->Write(); qOutSdist->Write(); DeltaqOutdist->Write();
  qSidedist->Write(); qSideSdist->Write(); DeltaqSidedist->Write();
  qLongdist->Write(); qLongSdist->Write(); DeltaqLongdist->Write();
};

inline StHbtThCorrFctn* Th3DQCorrFctn::ThClone() const {return new Th3DQCorrFctn(*this);}
