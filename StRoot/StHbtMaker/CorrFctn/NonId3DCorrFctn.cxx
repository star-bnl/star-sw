/***************************************************************************
 *
 * $Id: NonId3DCorrFctn.cxx,v 1.6 2004/02/17 19:03:15 jeromel Exp $
 *
 * Author: Adam Kisiel, Warsaw University of Technology
 ***************************************************************************
 *
 * Description: part of STAR HBT FRAMEWORK
 *   The correlation function for non-identical particle
 *   correlations. Uses selection on pair kinematics
 *   to perform a "3D-like" analysis.
 *
 ***************************************************************************
 *
 * $Log: NonId3DCorrFctn.cxx,v $
 * Revision 1.6  2004/02/17 19:03:15  jeromel
 * Assignement of float to int is unpredictable
 *
 * Revision 1.5  2004/02/02 20:19:15  kisiel
 * Properly initialize variables in NonId3DCorrFctn
 *
 * Revision 1.4  2003/02/02 21:52:49  magestro
 * small changes to remove compiler warnings
 *
 * Revision 1.3  2002/12/12 17:02:49  kisiel
 * Use KStar instead of 2*KStar for non-identical particles
 *
 * Revision 1.2  2001/04/05 22:05:59  kisiel
 *
 *
 *   Fix for the Insure++ warnings,
 *   and change the name of the developer, so that
 *   it is clear who to blame :)
 *
 * Revision 1.1  2001/04/03 21:02:50  kisiel
 *
 *
 *   The correlation function for non-identical particle
 *   correlations. Uses selection on pair kinematics
 *   to perform a "3D-like" analysis.
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

#include "StHbtMaker/CorrFctn/NonId3DCorrFctn.h"
#include <cstdio>

#ifdef __ROOT__ 
ClassImp(NonId3DCorrFctn)
#endif

//____________________________
NonId3DCorrFctn::NonId3DCorrFctn(char* title, const int& nbins, const float& QinvLo, const float& QinvHi){
  makeHistos(title, nbins, QinvLo, QinvHi);
}

//____________________________
void NonId3DCorrFctn::makeHistos(char* title, const int& nbins, const float& QinvLo, const float& QinvHi){
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

  float kstarlim = 0.5;
  float klim = 0.6 * kstarlim;
  
  char htitle[200];
  strcpy(htitle,"HOutKSame");
  mHOutKSame = new  StHbt2DHisto(strcat(htitle,title),htitle,
				 100,-klim,klim,nbins,QinvLo,kstarlim);
  strcpy(htitle,"HOutKDiff");
  mHOutKDiff = new  StHbt2DHisto(strcat(htitle,title),htitle,
				 100,-klim,klim,nbins,QinvLo,kstarlim);
  strcpy(htitle,"HSideKSame");
  mHSideKSame = new  StHbt2DHisto(strcat(htitle,title),htitle,
				 100,-klim,klim,nbins,QinvLo,kstarlim);
  strcpy(htitle,"HSideKDiff");
  mHSideKDiff = new  StHbt2DHisto(strcat(htitle,title),htitle,
				 100,-klim,klim,nbins,QinvLo,kstarlim);
  strcpy(htitle,"HLongKSame");
  mHLongKSame = new  StHbt2DHisto(strcat(htitle,title),htitle,
				 100,-klim,klim,nbins,QinvLo,kstarlim);
  strcpy(htitle,"HLongKDiff");
  mHLongKDiff = new  StHbt2DHisto(strcat(htitle,title),htitle,
				 100,-klim,klim,nbins,QinvLo,kstarlim);
//  strcpy(htitle,"HQSideExitNum");
//  mHQSideExitNum = new  StHbt2DHisto(strcat(htitle,title),htitle,
//				 100,-klim2,klim2,nbins,0.0,exitlim);
//  strcpy(htitle,"HKStarExitNumSideP");
//  mHKStarExitNumSideP = new  StHbt2DHisto(strcat(htitle,title),htitle,
//				 nbins,QinvLo,QinvHi,100,0.0,exitlim);
//  strcpy(htitle,"HKStarExitNumSideN");
//  mHKStarExitNumSideN = new  StHbt2DHisto(strcat(htitle,title),htitle,
//				 nbins,QinvLo,QinvHi,100,0.0,exitlim);
//  strcpy(htitle,"HQOutExitNum");
//  mHQOutExitNum = new  StHbt2DHisto(strcat(htitle,title),htitle,
//				 100,-klim2,klim2,nbins,0.0,exitlim);
//  strcpy(htitle,"HKStarExitNumOutP");
//  mHKStarExitNumOutP = new  StHbt2DHisto(strcat(htitle,title),htitle,
//				 nbins,QinvLo,QinvHi,100,0.0,exitlim);
//  strcpy(htitle,"HKStarExitNumOutN");
//  mHKStarExitNumOutN = new  StHbt2DHisto(strcat(htitle,title),htitle,
//				 nbins,QinvLo,QinvHi,100,0.0,exitlim);
//  strcpy(htitle,"HQSideExitDen");
//  mHQSideExitDen = new  StHbt2DHisto(strcat(htitle,title),htitle,
//				 100,-klim2,klim2,nbins,0.0,exitlim);
//  strcpy(htitle,"HKStarExitDenSideP");
//  mHKStarExitDenSideP = new  StHbt2DHisto(strcat(htitle,title),htitle,
//				 nbins,QinvLo,QinvHi,100,0.0,exitlim);
//  strcpy(htitle,"HKStarExitDenSideN");
//  mHKStarExitDenSideN = new  StHbt2DHisto(strcat(htitle,title),htitle,
//				 nbins,QinvLo,QinvHi,100,0.0,exitlim);
//  strcpy(htitle,"HQOutExitDen");
//  mHQOutExitDen = new  StHbt2DHisto(strcat(htitle,title),htitle,
//				 100,-klim2,klim2,nbins,0.0,exitlim);
//  strcpy(htitle,"HKStarExitDenOutP");
//  mHKStarExitDenOutP = new  StHbt2DHisto(strcat(htitle,title),htitle,
//				 nbins,QinvLo,QinvHi,100,0.0,exitlim);
//  strcpy(htitle,"HKStarExitDenOutN");
//  mHKStarExitDenOutN = new  StHbt2DHisto(strcat(htitle,title),htitle,
//				 nbins,QinvLo,QinvHi,100,0.0,exitlim);

  mKCompCut = -1000.0;
  mqSideSel = 0;   
}

//____________________________
NonId3DCorrFctn::NonId3DCorrFctn(char* title, const int& nbins, const float& QinvLo, 
				 const float& QinvHi, const int aqSideSel){
  makeHistos(title, nbins, QinvLo, QinvHi);
  mqSideSel = aqSideSel;
  mKCompCut = -1000.0;
}


NonId3DCorrFctn::NonId3DCorrFctn(char* title, const int& nbins, const float& QinvLo, 
		  const float& QinvHi, const float KCompCut)
{
  makeHistos(title, nbins, QinvLo, QinvHi);
  mKCompCut = KCompCut;
  mqSideSel = 0;   
}


//____________________________
NonId3DCorrFctn::~NonId3DCorrFctn(){
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

//   delete mHQSideExitNum;
//   delete mHKStarExitNumSideP;
//   delete mHKStarExitNumSideN;
//   delete mHQOutExitNum;
//   delete mHKStarExitNumOutP;
//   delete mHKStarExitNumOutN;
//   delete mHQSideExitDen;
//   delete mHKStarExitDenSideP;
//   delete mHKStarExitDenSideN;
//   delete mHQOutExitDen;
//   delete mHKStarExitDenOutP;
//   delete mHKStarExitDenOutN;

//  delete mHPt1KStarOutNum;
//  delete mHPt1KStarOutDen;
//  delete mHPt2KStarOutNum;
//  delete mHPt2KStarOutDen;
}
//_________________________
void NonId3DCorrFctn::Finish(){
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

void NonId3DCorrFctn::Write(){
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

//   mHQSideExitNum->Write();
//   mHKStarExitNumSideP->Write();
//   mHKStarExitNumSideN->Write();
//   mHQOutExitNum->Write();
//   mHKStarExitNumOutP->Write();
//   mHKStarExitNumOutN->Write();
//   mHQSideExitDen->Write();
//   mHKStarExitDenSideP->Write();
//   mHKStarExitDenSideN->Write();
//   mHQOutExitDen->Write();
//   mHKStarExitDenOutP->Write();
//   mHKStarExitDenOutN->Write();

//  mHPt1KStarOutNum->Write();
//  mHPt1KStarOutDen->Write();
//  mHPt2KStarOutNum->Write();
//  mHPt2KStarOutDen->Write();
}

//____________________________
StHbtString NonId3DCorrFctn::Report(){
  string stemp = "Non-Identical 3D Correlation Function Report:\n";
  char ctemp[1000];
  sprintf(ctemp,"Number of entries in out numerator:\t%E\n",mHOutKSame->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in out denominator:\t%E\n",mHOutKDiff->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in out ratio:\t%E\n",mRatOut->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in side numerator:\t%E\n",mHSideKSame->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in side denominator:\t%E\n",mHSideKDiff->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in side ratio:\t%E\n",mRatSide->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in long numerator:\t%E\n",mHLongKSame->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in long denominator:\t%E\n",mHLongKDiff->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in long ratio:\t%E\n",mRatLong->GetEntries());
  stemp += ctemp;
  StHbtString returnThis = stemp;
  return returnThis;
}
//____________________________
void NonId3DCorrFctn::AddRealPair(const StHbtPair* pair){
  double tKStar = fabs(pair->KStar());
  double tKOut = pair->dKOut();
  double tKSide = pair->dKSide();
  double tKLong = pair->dKLong();
  //double exitsep = pair->NominalTpcExitSeparation();

   if (mKCompCut <= 0.0)
    {
      if ((!mqSideSel) || (mqSideSel * tKSide > 0)) {
	if(tKOut>0.){
	  mNumOutP->Fill(tKStar);
// 	  mHKStarExitNumOutP->Fill(tKStar,exitsep,1.0);
	}
	else{
	  mNumOutN->Fill(tKStar);    
// 	  mHKStarExitNumOutN->Fill(tKStar,exitsep,1.0);
	}
	if(tKSide>0.){
	  mNumSideP->Fill(tKStar);
// 	  mHKStarExitNumSideP->Fill(tKStar,exitsep,1.0);
	}
	else{
	  mNumSideN->Fill(tKStar);    
// 	  mHKStarExitNumSideN->Fill(tKStar,exitsep,1.0);
	}
	if(tKLong>0.){
	  mNumLongP->Fill(tKStar);
	}
	else{
	  mNumLongN->Fill(tKStar);    
	}
      }
    }
  else  
    {
      if ((fabs(tKLong) < mKCompCut) && (fabs(tKSide) < mKCompCut))
	{
	  if(tKOut>0.){
	    mNumOutP->Fill(tKStar);
// 	    mHKStarExitNumOutP->Fill(tKStar,exitsep,1.0);
	  }
	  else{
	    mNumOutN->Fill(tKStar);    
// 	    mHKStarExitNumOutN->Fill(tKStar,exitsep,1.0);
	  }
	}
      if ((fabs(tKOut) < mKCompCut) && (fabs(tKLong) < mKCompCut))
	{
	  if(tKSide>0.){
	    mNumSideP->Fill(tKStar);
// 	    mHKStarExitNumSideP->Fill(tKStar,exitsep,1.0);
	  }
	  else{
	    mNumSideN->Fill(tKStar);    
// 	    mHKStarExitNumSideN->Fill(tKStar,exitsep,1.0);
	  }
	}
      if ((fabs(tKOut) < mKCompCut) && (fabs(tKSide) < mKCompCut))
	{
	  if(tKLong>0.){
	    mNumLongP->Fill(tKStar);
	  }
	  else{
	    mNumLongN->Fill(tKStar);    
	  }
	}
    }
  mHOutKSame->Fill(tKOut, tKStar, 1.);
  mHSideKSame->Fill(tKSide, tKStar, 1.);
  mHLongKSame->Fill(tKLong, tKStar, 1.);

//  mHPt1KStarOutNum->Fill(pair->track1()->FourMomentum().perp(),tKStar*((tKOut>0)*2-1),1.0);
//  mHPt2KStarOutNum->Fill(pair->track2()->FourMomentum().perp(),tKStar*((tKOut>0)*2-1),1.0);

//   mHQSideExitNum->Fill(tKSide,exitsep,1.0);
//   mHQOutExitNum->Fill(tKOut,exitsep,1.0);
}
//____________________________
void NonId3DCorrFctn::AddMixedPair(const StHbtPair* pair){
  double tKStar = fabs(pair->KStar());
  double tKOut = pair->dKOut();
  double tKSide = pair->dKSide();
  double tKLong = pair->dKLong();
  //double exitsep = pair->NominalTpcExitSeparation();

  if (mKCompCut <= 0.0)
    {
      if ((!mqSideSel) || (mqSideSel * tKSide > 0)) {
	if(tKOut>0.){
	  mDenOutP->Fill(tKStar);
// 	  mHKStarExitDenOutP->Fill(tKStar,exitsep,1.0);
	}
	else{
	  mDenOutN->Fill(tKStar);    
// 	  mHKStarExitDenSideP->Fill(tKStar,exitsep,1.0);
	}
	if(tKSide>0.){
	  mDenSideP->Fill(tKStar);
// 	  mHKStarExitDenSideP->Fill(tKStar,exitsep,1.0);
	}
	else{
	  mDenSideN->Fill(tKStar);    
// 	  mHKStarExitDenSideN->Fill(tKStar,exitsep,1.0);
	}
	if(tKLong>0.){
	  mDenLongP->Fill(tKStar);
	}
	else{
	  mDenLongN->Fill(tKStar);    
	}
      }
    }
  else  
    {
      if ((fabs(tKLong) < mKCompCut) && (fabs(tKSide) < mKCompCut))
	{
	  if(tKOut>0.){
	    mDenOutP->Fill(tKStar);
// 	    mHKStarExitDenOutP->Fill(tKStar,exitsep,1.0);
	  }
	  else{
	    mDenOutN->Fill(tKStar);    
// 	    mHKStarExitDenOutN->Fill(tKStar,exitsep,1.0);
	  }
	}
      if ((fabs(tKOut) < mKCompCut) && (fabs(tKLong) < mKCompCut))
	{
	  if(tKSide>0.){
	    mDenSideP->Fill(tKStar);
// 	    mHKStarExitDenSideP->Fill(tKStar,exitsep,1.0);
	  }
	  else{
	    mDenSideN->Fill(tKStar);    
// 	    mHKStarExitDenSideN->Fill(tKStar,exitsep,1.0);
	  }
	}
      if ((fabs(tKOut) < mKCompCut) && (fabs(tKSide) < mKCompCut))
	{
	  if(tKLong>0.){
	    mDenLongP->Fill(tKStar);
	  }
	  else{
	    mDenLongN->Fill(tKStar);    
	  }
	}
    }

  mHOutKDiff->Fill(tKOut, tKStar, 1.);
  mHSideKDiff->Fill(tKSide, tKStar, 1.);
  mHLongKDiff->Fill(tKLong, tKStar, 1.);

//   mHQSideExitDen->Fill(tKSide,exitsep,1.0);
//   mHQOutExitDen->Fill(tKOut,exitsep,1.0);
//  mHPt1KStarOutDen->Fill(pair->track1()->FourMomentum().perp(),tKStar*((tKOut>0)*2-1),1.0);
//  mHPt2KStarOutDen->Fill(pair->track2()->FourMomentum().perp(),tKStar*((tKOut>0)*2-1),1.0);
}


