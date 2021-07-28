/***************************************************************************
 *
 * $Id: NonIdPurityCorrFctn.cxx,v 1.2 2003/01/31 19:21:09 magestro Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a simple Q-invariant correlation function           
 *
 ***************************************************************************
 *
 * $Log: NonIdPurityCorrFctn.cxx,v $
 * Revision 1.2  2003/01/31 19:21:09  magestro
 * Cleared up simple compiler warnings on i386_linux24
 *
 * Revision 1.1  2002/12/12 17:02:49  kisiel
 * Use KStar instead of 2*KStar for non-identical particles
 *
 * Revision 1.4  2000/01/25 17:34:45  laue
 * I. In order to run the stand alone version of the StHbtMaker the following
 * changes have been done:
 * a) all ClassDefs and ClassImps have been put into #ifdef __ROOT__ statements
 * b) unnecessary includes of StMaker.h have been removed
 * c) the subdirectory StHbtMaker/doc/Make has been created including everything
 * needed for the stand alone version
 *
 * II. To reduce the amount of compiler warning
 * a) some variables have been type casted
 * b) some destructors have been declared as virtual
 *
 * Revision 1.3  1999/07/29 02:47:09  lisa
 * 1) add OpeningAngle correlation function 2) add StHbtMcEventReader 3) make histos in CorrFctns do errors correctly
 *
 * Revision 1.2  1999/07/06 22:33:20  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#include "StHbtMaker/CorrFctn/NonIdPurityCorrFctn.h"
#include <cstdio>

#ifdef __ROOT__ 
ClassImp(NonIdPurityCorrFctn)
#endif

//____________________________
NonIdPurityCorrFctn::NonIdPurityCorrFctn(char* title, const int& nbins, const float& QinvLo, const float& QinvHi, int p1Type, int p2Type){
  // set up numerator
  char TitNumP[100] = "NumP";
  strcat(TitNumP,title);
  mNumP = new StHbt1DHisto(TitNumP,title,nbins,QinvLo,QinvHi);
  // set up denominator
  char TitDenP[100] = "DenP";
  strcat(TitDenP,title);
  mDenP = new StHbt1DHisto(TitDenP,title,nbins,QinvLo,QinvHi);
  // set up ratio
  char TitRatP[100] = "RatP";
  strcat(TitRatP,title);
  mRatP = new StHbt1DHisto(TitRatP,title,nbins,QinvLo,QinvHi);
  // set up numerator
  char TitNumN[100] = "NumN";
  strcat(TitNumN,title);
  mNumN = new StHbt1DHisto(TitNumN,title,nbins,QinvLo,QinvHi);
  // set up denominator
  char TitDenN[100] = "DenN";
  strcat(TitDenN,title);
  mDenN = new StHbt1DHisto(TitDenN,title,nbins,QinvLo,QinvHi);
  // set up ratio
  char TitRatN[100] = "RatN";
  strcat(TitRatN,title);
  mRatN = new StHbt1DHisto(TitRatN,title,nbins,QinvLo,QinvHi);
  // set up ratio
  char TitRat[100] = "Rat";
  strcat(TitRat,title);
  mRat = new StHbt1DHisto(TitRat,title,nbins,QinvLo,QinvHi);
  // set up 
  char TitRatNOverP[100] = "RatNOverP";
  strcat(TitRatNOverP,title);
  mRatNOverP = new StHbt1DHisto(TitRatNOverP,title,nbins,QinvLo,QinvHi);

//   // set up numerator
//   char TitNumPinvP[100] = "NumPinvP";
//   strcat(TitNumPinvP,title);
//   mNumPinvP = new StHbt1DHisto(TitNumPinvP,title,nbins,QinvLo,QinvHi);
//   // set up denominator
//   char TitDenPinvP[100] = "DenPinvP";
//   strcat(TitDenPinvP,title);
//   mDenPinvP = new StHbt1DHisto(TitDenPinvP,title,nbins,QinvLo,QinvHi);
//   // set up numerator
//   char TitNumPinvN[100] = "NumPinvN";
//   strcat(TitNumPinvN,title);
//   mNumPinvN = new StHbt1DHisto(TitNumPinvN,title,nbins,QinvLo,QinvHi);
//   // set up denominator
//   char TitDenPinvN[100] = "DenPinvN";
//   strcat(TitDenPinvN,title);
//   mDenPinvN = new StHbt1DHisto(TitDenPinvN,title,nbins,QinvLo,QinvHi);
//   // set up ratio
//   char TitRatPinv[100] = "RatPinv";
//   strcat(TitRatPinv,title);
//   mRatPinv = new StHbt1DHisto(TitRatPinv,title,nbins,QinvLo,QinvHi);

//   // set up ratio
//   char TitRatPinvNormP[100] = "RatPinvNormP";
//   strcat(TitRatPinvNormP,title);
//   mRatPinvNormP = new StHbt1DHisto(TitRatPinvNormP,title,nbins,QinvLo,QinvHi);
//   // set up ratio
//   char TitRatPinvNormN[100] = "RatPinvNormN";
//   strcat(TitRatPinvNormN,title);
//   mRatPinvNormN = new StHbt1DHisto(TitRatPinvNormN,title,nbins,QinvLo,QinvHi);
//   // set up ratio
//   char TitRatPinvNorm[100] = "RatPinvNorm";
//   strcat(TitRatPinvNorm,title);
//   mRatPinvNorm = new StHbt1DHisto(TitRatPinvNorm,title,nbins,QinvLo,QinvHi);
//   // set up ratio
//   char TitRatPinvNormNOverP[100] = "RatPinvNormNOverP";
//   strcat(TitRatPinvNormNOverP,title);
//   mRatPinvNormNOverP = new StHbt1DHisto(TitRatPinvNormNOverP,
// 					title,nbins,QinvLo,QinvHi);
  mp1Type = p1Type;
  mp2Type = p2Type;
  char TitPairPurity[100];
  strcpy(TitPairPurity,"PairPurityOut");
  strcat(TitPairPurity,title);
  mPairPurityOut = new TProfile(TitPairPurity,title,2*nbins,-QinvHi,QinvHi);
  strcpy(TitPairPurity,"PairPuritySide");
  strcat(TitPairPurity,title);
  mPairPuritySide = new TProfile(TitPairPurity,title,2*nbins,-QinvHi,QinvHi);
  strcpy(TitPairPurity,"PairPurityLong");
  strcat(TitPairPurity,title);
  mPairPurityLong = new TProfile(TitPairPurity,title,2*nbins,-QinvHi,QinvHi);

  // to enable error bar calculation...
  mNumP->Sumw2();
  mDenP->Sumw2();  
  mRatP->Sumw2();  
  mNumN->Sumw2();
  mDenN->Sumw2();  
  mRatN->Sumw2();
  mRat->Sumw2(); 
  mRatNOverP->Sumw2();

//   mNumPinvP->Sumw2();
//   mDenPinvP->Sumw2();  
//   mNumPinvN->Sumw2();
//   mDenPinvN->Sumw2();  
//   mRatPinv->Sumw2(); 

//   mRatPinvNormP->Sumw2();
//   mRatPinvNormN->Sumw2();
//   mRatPinvNorm->Sumw2();
//   mRatPinvNormNOverP->Sumw2();

//   mQinvPt1 = new  StHbt2DHisto("QinvPt1","QinvPt1",100,0.,1.,
// 			       nbins,QinvLo,QinvHi);
//   mQinvY1 = new  StHbt2DHisto("QinvY1","QinvY1",100,-1.,1.,
// 			       nbins,QinvLo,QinvHi);

//   mHKCVKSame = new  StHbt2DHisto("HKCVKSame","HKCVKSame",
// 				 100,-1.,1.,nbins,QinvLo,QinvHi);
//   mHKCVKDiff = new  StHbt2DHisto("HKCVKDiff","HKCVKDiff",
// 				 100,-1.,1.,nbins,QinvLo,QinvHi);
}

//____________________________
NonIdPurityCorrFctn::~NonIdPurityCorrFctn(){
  delete mNumP;
  delete mDenP;  
  delete mRatP;  
  delete mNumN;
  delete mDenN;  
  delete mRatN;
  delete mRat; 
  delete mRatNOverP;

//   delete mNumPinvP;
//   delete mDenPinvP;  
//   delete mNumPinvN;
//   delete mDenPinvN;  
//   delete mRatPinv; 

//   delete mRatPinvNormP;
//   delete mRatPinvNormN;
//   delete mRatPinvNorm;
//   delete mRatPinvNormNOverP;

//   delete mQinvPt1;
//   delete mQinvY1;

//   delete  mHKCVKSame;
//   delete  mHKCVKDiff;

  delete mPairPurityOut;
  delete mPairPuritySide;
  delete mPairPurityLong;
  
}
//_________________________
void NonIdPurityCorrFctn::Finish(){
  double tScale;
  int tLastNormBin = mNumP->GetNbinsX();
  int tFirstNormBin = tLastNormBin/2+1;
  // Make cvk dependant correlation function
  mRatP->Divide(mNumP,mDenP,1.0,1.0);
  tScale = mRatP->Integral(tFirstNormBin,tLastNormBin);
  tScale/= (tLastNormBin-tFirstNormBin+1);
  mRatP->Scale(1./tScale);
  mRatN->Divide(mNumN,mDenN,1.0,1.0);
  tScale = mRatN->Integral(tFirstNormBin,tLastNormBin);
  tScale/= (tLastNormBin-tFirstNormBin+1);
  mRatN->Scale(1./tScale);
  mRatNOverP->Divide(mRatN,mRatP,1.0,1.0);

//   mRatPinvNormP->Divide(mNumP,mNumPinvP,1.0,1.0);
//   mRatPinvNormN->Divide(mNumN,mNumPinvN,1.0,1.0);
//   mRatPinvNormNOverP->Divide(mRatPinvNormN,mRatPinvNormP);

  // Regular correlation function
  TH1D tHNum(*mNumP);
  tHNum.SetName("tHNum");
  tHNum.Add(mNumN);
  TH1D tHDen(*mDenP);
  tHDen.SetName("tHDen");
  tHDen.Add(mDenN); 
  mRat->Divide(&tHNum,&tHDen);
  tScale = mRat->Integral(tFirstNormBin,tLastNormBin);
  tScale/= (tLastNormBin-tFirstNormBin+1);
  mRat->Scale(1./tScale);

  // Pinv correlation function
//   TH1D tHNumPinv(*mNumPinvP);
//   tHNumPinv.SetName("tHNumPinv");
//   tHNumPinv.Add(mNumPinvN);
//   TH1D tHDenPinv(*mDenPinvP);
//   tHDenPinv.SetName("tHDenPinv");
//   tHDenPinv.Add(mDenPinvN); 
//   mRatPinv->Divide(&tHNumPinv,&tHDenPinv);
//   tScale = mRatPinv->Integral(tFirstNormBin,tLastNormBin);
//   tScale/= (tLastNormBin-tFirstNormBin+1);
//   mRatPinv->Scale(1./tScale);

  // Pinv normalised correlation function
//   mRatPinvNorm->Divide(&tHNum,&tHNumPinv);
}

void NonIdPurityCorrFctn::Write(){
  mNumP->Write();
  mDenP->Write();  
  mRatP->Write();  
  mNumN->Write();
  mDenN->Write();  
  mRatN->Write();
  mRat->Write(); 
  mRatNOverP->Write();

//   mNumPinvP->Write();
//   mDenPinvP->Write();  
//   mNumPinvN->Write();
//   mDenPinvN->Write();  
//   mRatPinv->Write(); 

//   mRatPinvNormP->Write();
//   mRatPinvNormN->Write();
//   mRatPinvNorm->Write();
//   mRatPinvNormNOverP->Write();

//   mHKCVKSame->Write();
//   mHKCVKDiff->Write();
  //mQinvPt1->Write();
  //mQinvY1->Write();

  mPairPurityOut->Write();  
  mPairPuritySide->Write();  
  mPairPurityLong->Write();  
}

//____________________________
StHbtString NonIdPurityCorrFctn::Report(){
  string stemp = "Qinv Correlation Function Report:\n";
  char ctemp[100];
  sprintf(ctemp,"Number of entries in numerator:\t%E\n",mNumP->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in denominator:\t%E\n",mDenP->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in ratio:\t%E\n",mRatP->GetEntries());
  stemp += ctemp;
  StHbtString returnThis = stemp;
  return returnThis;
}
//____________________________
void NonIdPurityCorrFctn::AddRealPair(const StHbtPair* pair){
  double tKStar = fabs(pair->KStar());
  //double tPStar = 2*fabs(pair->KStarFlipped());
  double tCVK = pair->CVK();
  double pPurity = 0.0;
  double tKOut = pair->dKOut();
  double tKSide = pair->dKSide();
  double tKLong = pair->dKLong();
  
  //mQinvPt1->Fill(pair->track1()->FourMomentum().perp(),tKStar,1);
  //mQinvY1->Fill(pair->track1()->FourMomentum().rapidity(),tKStar,1);
  
  //if(pair->track1()->FourMomentum().pz()<0.){
  //  cout << pair->track1()->FourMomentum() << endl;
  //}
//   mHKCVKSame->Fill(tCVK,tKStar,1.);

  if(tCVK>0.){
    mNumP->Fill(tKStar);
  }
  else{
    mNumN->Fill(tKStar);
  }
//   if(pair->CVKFlipped()>0.){
//     mNumPinvP->Fill(tPStar);
//   }
//   else{
//     mNumPinvN->Fill(tPStar);
//   }
  
  switch (mp1Type)
    {
    case 1:
      pPurity =  pair->track1()->GetPionPurity();
      break;
    case 2:
      pPurity =  pair->track1()->GetKaonPurity();
      break;
    case 3:
      pPurity =  pair->track1()->GetProtonPurity();
      break;
    case 4:
      pPurity =  pair->track1()->Track()->PidProbElectron();
      break;
    }

  //  cout << "First particle purity: " << pPurity << endl;
  
  switch (mp2Type)
    {
    case 1:
      pPurity *=  pair->track2()->GetPionPurity();
      break;
    case 2:
      pPurity *=  pair->track2()->GetKaonPurity();
      break;
    case 3:
      pPurity *=  pair->track2()->GetProtonPurity();
      break;
    case 4:
      pPurity *=  pair->track2()->Track()->PidProbElectron();
      break;
    }
  //  cout << "Pair purity: " << pPurity << endl;
  
  mPairPurityOut->Fill(((tKOut>0)*2-1) *tKStar, pPurity);
  mPairPuritySide->Fill(((tKSide>0)*2-1)*tKStar, pPurity);
  mPairPurityLong->Fill(((tKLong>0)*2-1)*tKStar, pPurity);
  
}
//____________________________
void NonIdPurityCorrFctn::AddMixedPair(const StHbtPair* pair){
  double tKStar = 2*fabs(pair->KStar());
  //double tPStar = 2*fabs(pair->KStarFlipped());
  double tCVK = pair->CVK();
//   mHKCVKDiff->Fill(tCVK,tKStar,1.);
  if(tCVK>0.){
    mDenP->Fill(tKStar);
  }
  else{
    mDenN->Fill(tKStar);
  }
//   if(pair->CVKFlipped()>0.){
//     mDenPinvP->Fill(tPStar);
//   }
//   else{
//     mDenPinvN->Fill(tPStar); 
//   }
}


