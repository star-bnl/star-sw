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

#include "StHbtMaker/ThCorrFctn/ThQinvCorrFctn.h"

#ifdef __ROOT__
ClassImp(ThQinvCorrFctn)
#endif

ThQinvCorrFctn::ThQinvCorrFctn(char* aTitle, int aNBins, 
			       double aHLo, double aHHi)
  : StHbtThCorrFctn() ,StHbtRoot1DCF(aTitle, aNBins, aHLo, aHHi) 
{ /* no-op */ };
ThQinvCorrFctn::ThQinvCorrFctn(const ThQinvCorrFctn& ThCf) : StHbtThCorrFctn(ThCf),StHbtRoot1DCF( ThCf) 
{ /* no-op */ }

ThQinvCorrFctn::~ThQinvCorrFctn() 
{ /* no-op */ };

//____________________________
void ThQinvCorrFctn::AddNum(StHbtThPair* aThPair){

}
void ThQinvCorrFctn::AddDen(StHbtThPair* aThPair){

  double tQInv = 2*fabs(aThPair->GetMeasPair()->KStar());
  //  cout << "Filling ThQinv CF with values: KStar:  " << tQInv << "  wDen:  " << aThPair->GetWeightDen() << " wNum:   " << aThPair->GetWeightNum() << endl;
  if(tQInv>=mHLo && tQInv<=mHHi){
    mDenominator->Fill(tQInv,aThPair->GetWeightDen());
  }
  if(tQInv>=mHLo && tQInv<=mHHi){
    mNumerator->Fill(tQInv,aThPair->GetWeightNum());
  }
}

void ThQinvCorrFctn::Finish(){
  StHbtRoot1DCF::Finish();
}

inline StHbt1DHisto* ThQinvCorrFctn::Numerator() const { cout << "Return Ratio" << endl; return mNumerator;};
inline StHbt1DHisto* ThQinvCorrFctn::Denominator() const {cout << "Return Denimnatior" << endl; return mDenominator;};
inline StHbt1DHisto* ThQinvCorrFctn::Ratio() const {cout << "Return Numerator" << endl; return mRatio;};
inline void ThQinvCorrFctn::Write()  {mNumerator->Write();mDenominator->Write();mRatio->Write();};

inline StHbtThCorrFctn* ThQinvCorrFctn::ThClone() const {return new ThQinvCorrFctn(*this);}
