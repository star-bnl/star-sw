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

#include "StHbtMaker/ThCorrFctn/ThNonId3DCF.h"

#ifdef __ROOT__
ClassImp(ThNonId3DCF)
#endif
  
ThNonId3DCF::ThNonId3DCF(char* title, const int& nbins, const float& QinvLo, const float& QinvHi)
  : StHbtThCorrFctn(), mBtMin(0.0), mBtMax(1.0), mUtMin(0.0), mUtMax(1.0)
{
  // set up numerator
  char TitNumOutP[100] = "NumOutP";
  strcat(TitNumOutP,title);
  mNumOutP = new StHbt1DHisto(TitNumOutP,title,nbins,QinvLo,QinvHi);
  // set up denominator
  char TitDenOutP[100] = "DenOutP";
  strcat(TitDenOutP,title);
  mDenOutP = new StHbt1DHisto(TitDenOutP,title,nbins,QinvLo,QinvHi);
  // set up ratio
  char TitRatOutP[100] = "RatOutP";
  strcat(TitRatOutP,title);
  mRatOutP = new StHbt1DHisto(TitRatOutP,title,nbins,QinvLo,QinvHi);
  // set up numerator
  char TitNumOutN[100] = "NumOutN";
  strcat(TitNumOutN,title);
  mNumOutN = new StHbt1DHisto(TitNumOutN,title,nbins,QinvLo,QinvHi);
  // set up denominator
  char TitDenOutN[100] = "DenOutN";
  strcat(TitDenOutN,title);
  mDenOutN = new StHbt1DHisto(TitDenOutN,title,nbins,QinvLo,QinvHi);
  // set up ratio
  char TitRatOutN[100] = "RatOutN";
  strcat(TitRatOutN,title);
  mRatOutN = new StHbt1DHisto(TitRatOutN,title,nbins,QinvLo,QinvHi);
  // set up ratio
  char TitRatOut[100] = "RatOut";
  strcat(TitRatOut,title);
  mRatOut = new StHbt1DHisto(TitRatOut,title,nbins,QinvLo,QinvHi);
  // set up 
  char TitRatOutNOverP[100] = "RatOutNOverP";
  strcat(TitRatOutNOverP,title);
  mRatOutNOverP = new StHbt1DHisto(TitRatOutNOverP,title,nbins,QinvLo,QinvHi);

  // set up numerator
  char TitNumSideP[100] = "NumSideP";
  strcat(TitNumSideP,title);
  mNumSideP = new StHbt1DHisto(TitNumSideP,title,nbins,QinvLo,QinvHi);
  // set up denominator
  char TitDenSideP[100] = "DenSideP";
  strcat(TitDenSideP,title);
  mDenSideP = new StHbt1DHisto(TitDenSideP,title,nbins,QinvLo,QinvHi);
  // set up ratio
  char TitRatSideP[100] = "RatSideP";
  strcat(TitRatSideP,title);
  mRatSideP = new StHbt1DHisto(TitRatSideP,title,nbins,QinvLo,QinvHi);
  // set up numerator
  char TitNumSideN[100] = "NumSideN";
  strcat(TitNumSideN,title);
  mNumSideN = new StHbt1DHisto(TitNumSideN,title,nbins,QinvLo,QinvHi);
  // set up denominator
  char TitDenSideN[100] = "DenSideN";
  strcat(TitDenSideN,title);
  mDenSideN = new StHbt1DHisto(TitDenSideN,title,nbins,QinvLo,QinvHi);
  // set up ratio
  char TitRatSideN[100] = "RatSideN";
  strcat(TitRatSideN,title);
  mRatSideN = new StHbt1DHisto(TitRatSideN,title,nbins,QinvLo,QinvHi);
  // set up ratio
  char TitRatSide[100] = "RatSide";
  strcat(TitRatSide,title);
  mRatSide = new StHbt1DHisto(TitRatSide,title,nbins,QinvLo,QinvHi);
  // set up 
  char TitRatSideNOverP[100] = "RatSideNOverP";
  strcat(TitRatSideNOverP,title);
  mRatSideNOverP = new StHbt1DHisto(TitRatSideNOverP,
				    title,nbins,QinvLo,QinvHi);

  // set up numerator
  char TitNumLongP[100] = "NumLongP";
  strcat(TitNumLongP,title);
  mNumLongP = new StHbt1DHisto(TitNumLongP,title,nbins,QinvLo,QinvHi);
  // set up denominator
  char TitDenLongP[100] = "DenLongP";
  strcat(TitDenLongP,title);
  mDenLongP = new StHbt1DHisto(TitDenLongP,title,nbins,QinvLo,QinvHi);
  // set up ratio
  char TitRatLongP[100] = "RatLongP";
  strcat(TitRatLongP,title);
  mRatLongP = new StHbt1DHisto(TitRatLongP,title,nbins,QinvLo,QinvHi);
  // set up numerator
  char TitNumLongN[100] = "NumLongN";
  strcat(TitNumLongN,title);
  mNumLongN = new StHbt1DHisto(TitNumLongN,title,nbins,QinvLo,QinvHi);
  // set up denominator
  char TitDenLongN[100] = "DenLongN";
  strcat(TitDenLongN,title);
  mDenLongN = new StHbt1DHisto(TitDenLongN,title,nbins,QinvLo,QinvHi);
  // set up ratio
  char TitRatLongN[100] = "RatLongN";
  strcat(TitRatLongN,title);
  mRatLongN = new StHbt1DHisto(TitRatLongN,title,nbins,QinvLo,QinvHi);
  // set up ratio
  char TitRatLong[100] = "RatLong";
  strcat(TitRatLong,title);
  mRatLong = new StHbt1DHisto(TitRatLong,title,nbins,QinvLo,QinvHi);
  // set up 
  char TitRatLongNOverP[100] = "RatLongNOverP";
  strcat(TitRatLongNOverP,title);
  mRatLongNOverP = new StHbt1DHisto(TitRatLongNOverP,title,nbins,QinvLo,QinvHi);

  char tOP[100] = "ProfOutP";
  strcat(tOP,title);
  mProfOutP = new TProfile(tOP, tOP, nbins, QinvLo, QinvHi);
  char tON[100] = "ProfOutN";
  strcat(tON,title);
  mProfOutN = new TProfile(tON, tON, nbins, QinvLo, QinvHi);
  char tSP[100] = "ProfSideP";
  strcat(tSP,title);
  mProfSideP = new TProfile(tSP, tSP, nbins, QinvLo, QinvHi);
  char tSN[100] = "ProfSideN";
  strcat(tSN,title);
  mProfSideN = new TProfile(tSN, tSN, nbins, QinvLo, QinvHi);
  char tLP[100] = "ProfLongP";
  strcat(tLP,title);
  mProfLongP = new TProfile(tLP, tLP, nbins, QinvLo, QinvHi);
  char tLN[100] = "ProfLongN";
  strcat(tLN,title);
  mProfLongN = new TProfile(tLN, tLN, nbins, QinvLo, QinvHi);

  char tDOP[100] = "ProfDenOutP";
  strcat(tDOP,title);
  mProfDenOutP = new TProfile(tDOP, tDOP, nbins, QinvLo, QinvHi);
  char tDON[100] = "ProfDenOutN";
  strcat(tDON,title);
  mProfDenOutN = new TProfile(tDON, tDON, nbins, QinvLo, QinvHi);
  char tDSP[100] = "ProfDenSideP";
  strcat(tDSP,title);
  mProfDenSideP = new TProfile(tDSP, tDSP, nbins, QinvLo, QinvHi);
  char tDSN[100] = "ProfDenSideN";
  strcat(tDSN,title);
  mProfDenSideN = new TProfile(tDSN, tDSN, nbins, QinvLo, QinvHi);
  char tDLP[100] = "ProfDenLongP";
  strcat(tDLP,title);
  mProfDenLongP = new TProfile(tDLP, tDLP, nbins, QinvLo, QinvHi);
  char tDLN[100] = "ProfDenLongN";
  strcat(tDLN,title);
  mProfDenLongN = new TProfile(tDLN, tDLN, nbins, QinvLo, QinvHi);

  // to enable error bar calculation...
  mNumOutP->Sumw2();
  mDenOutP->Sumw2();  
  mRatOutP->Sumw2();  
  mNumOutN->Sumw2();
  mDenOutN->Sumw2();  
  mRatOutN->Sumw2();
  mRatOut->Sumw2(); 
  mRatOutNOverP->Sumw2();

  mNumSideP->Sumw2();
  mDenSideP->Sumw2();  
  mRatSideP->Sumw2();  
  mNumSideN->Sumw2();
  mDenSideN->Sumw2();  
  mRatSideN->Sumw2();
  mRatSide->Sumw2(); 
  mRatSideNOverP->Sumw2();

  mNumLongP->Sumw2();
  mDenLongP->Sumw2();  
  mRatLongP->Sumw2();  
  mNumLongN->Sumw2();
  mDenLongN->Sumw2();  
  mRatLongN->Sumw2();
  mRatLong->Sumw2(); 
  mRatLongNOverP->Sumw2();

  float klim = 1.2 * QinvHi;
  mHOutKSame = new  StHbt2DHisto("HOutKSame","HOutKSame",
				 100,-klim,klim,nbins,QinvLo,QinvHi);
  mHOutKDiff = new  StHbt2DHisto("HOutKDiff","HOutKDiff",
				 100,-klim,klim,nbins,QinvLo,QinvHi);
  mHSideKSame = new  StHbt2DHisto("HSideKSame","HSideKSame",
				 100,-klim,klim,nbins,QinvLo,QinvHi);
  mHSideKDiff = new  StHbt2DHisto("HSideKDiff","HSideKDiff",
				 100,-klim,klim,nbins,QinvLo,QinvHi);
  mHLongKSame = new  StHbt2DHisto("HLongKSame","HLongKSame",
				 100,-klim,klim,nbins,QinvLo,QinvHi);
  mHLongKDiff = new  StHbt2DHisto("HLongKDiff","HLongKDiff",
				 100,-klim,klim,nbins,QinvLo,QinvHi);

};

ThNonId3DCF::ThNonId3DCF(const ThNonId3DCF& ThCf) : StHbtThCorrFctn(ThCf) 
{
  mNumOutP = new StHbt1DHisto(*ThCf.mNumOutP);
  mDenOutP = new StHbt1DHisto(*ThCf.mDenOutP);  
  mRatOutP = new StHbt1DHisto(*ThCf.mRatOutP);  
  mNumOutN = new StHbt1DHisto(*ThCf.mNumOutN);
  mDenOutN = new StHbt1DHisto(*ThCf.mDenOutN);  
  mRatOutN = new StHbt1DHisto(*ThCf.mRatOutN);
  mRatOut = new StHbt1DHisto(*ThCf.mRatOut); 
  mRatOutNOverP = new StHbt1DHisto(*ThCf.mRatOutNOverP);

  mNumSideP = new StHbt1DHisto(*ThCf.mNumSideP);
  mDenSideP = new StHbt1DHisto(*ThCf.mDenSideP);  
  mRatSideP = new StHbt1DHisto(*ThCf.mRatSideP);  
  mNumSideN = new StHbt1DHisto(*ThCf.mNumSideN);
  mDenSideN = new StHbt1DHisto(*ThCf.mDenSideN);  
  mRatSideN = new StHbt1DHisto(*ThCf.mRatSideN);
  mRatSide = new StHbt1DHisto(*ThCf.mRatSide); 
  mRatSideNOverP = new StHbt1DHisto(*ThCf.mRatSideNOverP);

  mNumLongP = new StHbt1DHisto(*ThCf.mNumLongP);
  mDenLongP = new StHbt1DHisto(*ThCf.mDenLongP);  
  mRatLongP = new StHbt1DHisto(*ThCf.mRatLongP);  
  mNumLongN = new StHbt1DHisto(*ThCf.mNumLongN);
  mDenLongN = new StHbt1DHisto(*ThCf.mDenLongN);  
  mRatLongN = new StHbt1DHisto(*ThCf.mRatLongN);
  mRatLong = new StHbt1DHisto(*ThCf.mRatLong); 
  mRatLongNOverP = new StHbt1DHisto(*ThCf.mRatLongNOverP);

  mHOutKSame = new StHbt2DHisto(*ThCf.mHOutKSame);
  mHOutKDiff = new StHbt2DHisto(*ThCf.mHOutKDiff);
  mHSideKSame = new StHbt2DHisto(*ThCf.mHSideKSame);
  mHSideKDiff = new StHbt2DHisto(*ThCf.mHSideKDiff);
  mHLongKSame = new StHbt2DHisto(*ThCf.mHLongKSame);
  mHLongKDiff = new StHbt2DHisto(*ThCf.mHLongKDiff);
 
  mProfOutP = new TProfile(*ThCf.mProfOutP);
  mProfOutN = new TProfile(*ThCf.mProfOutN);
  mProfSideP = new TProfile(*ThCf.mProfSideP);
  mProfSideN = new TProfile(*ThCf.mProfSideN);
  mProfLongP = new TProfile(*ThCf.mProfLongP);
  mProfLongN = new TProfile(*ThCf.mProfLongN);

  mProfDenOutP = new TProfile(*ThCf.mProfDenOutP);
  mProfDenOutN = new TProfile(*ThCf.mProfDenOutN);
  mProfDenSideP = new TProfile(*ThCf.mProfDenSideP);
  mProfDenSideN = new TProfile(*ThCf.mProfDenSideN);
  mProfDenLongP = new TProfile(*ThCf.mProfDenLongP);
  mProfDenLongN = new TProfile(*ThCf.mProfDenLongN);
}

ThNonId3DCF::~ThNonId3DCF(){
  delete mNumOutP;
  delete mDenOutP;  
  delete mRatOutP;  
  delete mNumOutN;
  delete mDenOutN;  
  delete mRatOutN;
  delete mRatOut; 
  delete mRatOutNOverP;

  delete mNumSideP;
  delete mDenSideP;  
  delete mRatSideP;  
  delete mNumSideN;
  delete mDenSideN;  
  delete mRatSideN;
  delete mRatSide; 
  delete mRatSideNOverP;

  delete mNumLongP;
  delete mDenLongP;  
  delete mRatLongP;  
  delete mNumLongN;
  delete mDenLongN;  
  delete mRatLongN;
  delete mRatLong; 
  delete mRatLongNOverP;

  delete  mHOutKSame;
  delete  mHOutKDiff;
  delete  mHSideKSame;
  delete  mHSideKDiff;
  delete  mHLongKSame;
  delete  mHLongKDiff;

  delete mProfOutP;
  delete mProfOutN;
  delete mProfSideP;
  delete mProfSideN;
  delete mProfLongP;
  delete mProfLongN;

  delete mProfDenOutP;
  delete mProfDenOutN;
  delete mProfDenSideP;
  delete mProfDenSideN;
  delete mProfDenLongP;
  delete mProfDenLongN;

}


//____________________________
void ThNonId3DCF::AddNum(StHbtThPair* aThPair){
  double tKStar = fabs(aThPair->GetMeasPair()->KStar());
  double tKOut = aThPair->GetMeasPair()->dKOut();
  double tKSide = aThPair->GetMeasPair()->dKSide();
  double tKLong = aThPair->GetMeasPair()->dKLong();
  double weight = aThPair->GetWeightNum();

  if ((aThPair->Betat() < mBtMax) && (aThPair->Betat() > mBtMin) &&
      (aThPair->Ut() < mUtMax) && (aThPair->Ut() > mUtMin))
    {
      if(tKOut>0.){
	mNumOutP->Fill(tKStar, weight);
	mProfOutP->Fill(tKStar, weight);
      }
      else{
	mNumOutN->Fill(tKStar, weight); 
	mProfOutN->Fill(tKStar, weight);
      }
      if(tKSide>0.){
	mNumSideP->Fill(tKStar, weight);
	mProfSideP->Fill(tKStar, weight);
      }
      else{
	mNumSideN->Fill(tKStar, weight); 
	mProfSideN->Fill(tKStar, weight);
      }
      if(tKLong>0.){
	mNumLongP->Fill(tKStar, weight);
	mProfLongP->Fill(tKStar, weight);
      }
      else{
	mNumLongN->Fill(tKStar, weight);  
	mProfLongN->Fill(tKStar, weight);
      }

      mHOutKSame->Fill(tKOut, tKStar, weight);
      mHSideKSame->Fill(tKSide, tKStar, weight);
      mHLongKSame->Fill(tKLong, tKStar, weight);
    }
}

void ThNonId3DCF::AddDen(StHbtThPair* aThPair){
  double tKStar = fabs(aThPair->GetMeasPair()->KStar());
  double tKOut = aThPair->GetMeasPair()->dKOut();
  double tKSide = aThPair->GetMeasPair()->dKSide();
  double tKLong = aThPair->GetMeasPair()->dKLong();
  double weight = aThPair->GetWeightDen();

  if ((aThPair->Betat() < mBtMax) && (aThPair->Betat() > mBtMin) &&
      (aThPair->Ut() < mUtMax) && (aThPair->Ut() > mUtMin))
    {
      if(tKOut>0.){
	mDenOutP->Fill(tKStar, weight);
	mProfDenOutP->Fill(tKStar, weight);
      }
      else{
	mDenOutN->Fill(tKStar, weight);    
	mProfDenOutN->Fill(tKStar, weight);
      }
      if(tKSide>0.){
	mDenSideP->Fill(tKStar, weight);
	mProfDenSideP->Fill(tKStar, weight);
      }
      else{
	mDenSideN->Fill(tKStar, weight);    
	mProfDenSideN->Fill(tKStar, weight);
      }
      if(tKLong>0.){
	mDenLongP->Fill(tKStar, weight);
	mProfDenLongP->Fill(tKStar, weight);
      }
      else{
	mDenLongN->Fill(tKStar, weight);    
	mProfDenLongN->Fill(tKStar, weight);
      }

      mHOutKDiff->Fill(tKOut, tKStar, weight);
      mHSideKDiff->Fill(tKSide, tKStar, weight);
      mHLongKDiff->Fill(tKLong, tKStar, weight);
    }
}

void ThNonId3DCF::Finish(){

  double tScale;
  int tLastNormBin = mNumOutP->GetNbinsX();
  int tFirstNormBin = tLastNormBin/2+1;

// >>> Out
  // Make cvk dependant correlation function
  mRatOutP->Divide(mNumOutP,mDenOutP,1.0,1.0);
  tScale = mRatOutP->Integral(tFirstNormBin,tLastNormBin);
  tScale/= (tLastNormBin-tFirstNormBin+1);
  mRatOutP->Scale(1./tScale);
  mRatOutN->Divide(mNumOutN,mDenOutN,1.0,1.0);
  tScale = mRatOutN->Integral(tFirstNormBin,tLastNormBin);
  tScale/= (tLastNormBin-tFirstNormBin+1);
  mRatOutN->Scale(1./tScale);
  mRatOutNOverP->Divide(mRatOutN,mRatOutP,1.0,1.0);
  // Regular correlation function
  TH1D tHNumOut(*mNumOutP);
  tHNumOut.SetName("tHNumOut");
  tHNumOut.Add(mNumOutN);
  TH1D tHDenOut(*mDenOutP);
  tHDenOut.SetName("tHDenOut");
  tHDenOut.Add(mDenOutN); 
  mRatOut->Divide(&tHNumOut,&tHDenOut);
  tScale = mRatOut->Integral(tFirstNormBin,tLastNormBin);
  tScale/= (tLastNormBin-tFirstNormBin+1);
  mRatOut->Scale(1./tScale);

// >>> Side
  // Make cvk dependant correlation function
  mRatSideP->Divide(mNumSideP,mDenSideP,1.0,1.0);
  tScale = mRatSideP->Integral(tFirstNormBin,tLastNormBin);
  tScale/= (tLastNormBin-tFirstNormBin+1);
  mRatSideP->Scale(1./tScale);
  mRatSideN->Divide(mNumSideN,mDenSideN,1.0,1.0);
  tScale = mRatSideN->Integral(tFirstNormBin,tLastNormBin);
  tScale/= (tLastNormBin-tFirstNormBin+1);
  mRatSideN->Scale(1./tScale);
  mRatSideNOverP->Divide(mRatSideN,mRatSideP,1.0,1.0);
  // Regular correlation function
  TH1D tHNumSide(*mNumSideP);
  tHNumSide.SetName("tHNumSide");
  tHNumSide.Add(mNumSideN);
  TH1D tHDenSide(*mDenSideP);
  tHDenSide.SetName("tHDenSide");
  tHDenSide.Add(mDenSideN); 
  mRatSide->Divide(&tHNumSide,&tHDenSide);
  tScale = mRatSide->Integral(tFirstNormBin,tLastNormBin);
  tScale/= (tLastNormBin-tFirstNormBin+1);
  mRatSide->Scale(1./tScale);

// >>> Long
  // Make cvk dependant correlation function
  mRatLongP->Divide(mNumLongP,mDenLongP,1.0,1.0);
  tScale = mRatLongP->Integral(tFirstNormBin,tLastNormBin);
  tScale/= (tLastNormBin-tFirstNormBin+1);
  mRatLongP->Scale(1./tScale);
  mRatLongN->Divide(mNumLongN,mDenLongN,1.0,1.0);
  tScale = mRatLongN->Integral(tFirstNormBin,tLastNormBin);
  tScale/= (tLastNormBin-tFirstNormBin+1);
  mRatLongN->Scale(1./tScale);
  mRatLongNOverP->Divide(mRatLongN,mRatLongP,1.0,1.0);
  // Regular correlation function
  TH1D tHNumLong(*mNumLongP);
  tHNumLong.SetName("tHNumLong");
  tHNumLong.Add(mNumLongN);
  TH1D tHDenLong(*mDenLongP);
  tHDenLong.SetName("tHDenLong");
  tHDenLong.Add(mDenLongN); 
  mRatLong->Divide(&tHNumLong,&tHDenLong);
  tScale = mRatLong->Integral(tFirstNormBin,tLastNormBin);
  tScale/= (tLastNormBin-tFirstNormBin+1);
  mRatLong->Scale(1./tScale);

}

inline void ThNonId3DCF::Write()  {
  mNumOutP->Write();
  mDenOutP->Write();  
  mRatOutP->Write();  
  mNumOutN->Write();
  mDenOutN->Write();  
  mRatOutN->Write();
  mRatOut->Write(); 
  mRatOutNOverP->Write();

  mNumSideP->Write();
  mDenSideP->Write();  
  mRatSideP->Write();  
  mNumSideN->Write();
  mDenSideN->Write();  
  mRatSideN->Write();
  mRatSide->Write(); 
  mRatSideNOverP->Write();

  mNumLongP->Write();
  mDenLongP->Write();  
  mRatLongP->Write();  
  mNumLongN->Write();
  mDenLongN->Write();  
  mRatLongN->Write();
  mRatLong->Write(); 
  mRatLongNOverP->Write();

  mHOutKSame->Write();
  mHOutKDiff->Write();
  mHSideKSame->Write();
  mHSideKDiff->Write();
  mHLongKSame->Write();
  mHLongKDiff->Write();

  mProfOutP->Write();
  mProfOutN->Write();
  mProfSideP->Write();
  mProfSideN->Write();
  mProfLongP->Write();
  mProfLongN->Write();

  mProfDenOutP->Write();
  mProfDenOutN->Write();
  mProfDenSideP->Write();
  mProfDenSideN->Write();
  mProfDenLongP->Write();
  mProfDenLongN->Write();

};

StHbtString ThNonId3DCF::Report(){
  string stemp = "Qinv Correlation Function Report:\n";
  char ctemp[100];
  sprintf(ctemp,"Number of entries in numerator:\t%E\n",mNumOutP->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in denominator:\t%E\n",mDenOutP->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in ratio:\t%E\n",mRatOutP->GetEntries());
  stemp += ctemp;
  StHbtString returnThis = stemp;
  return returnThis;
}

inline StHbtThCorrFctn* ThNonId3DCF::ThClone() const {return new ThNonId3DCF(*this);}
inline void ThNonId3DCF::SetBtRange(double aBtMin, double aBtMax)
{
  mBtMin = aBtMin;
  mBtMax = aBtMax;
}
inline void ThNonId3DCF::SetUtRange(double aUtMin, double aUtMax)
{
  mUtMin = aUtMin;
  mUtMax = aUtMax;
}
