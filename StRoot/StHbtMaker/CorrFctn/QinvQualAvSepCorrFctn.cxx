/***************************************************************************
 *
 * Author: Randy Wells, Ohio State, rcwells@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a simple QualityFactor correlation function used for studying 2-track cuts
 *   also has a merging axis built in for entrance separation
 *
 ***************************************************************************
 *
 **************************************************************************/


#include "StHbtMaker/CorrFctn/QinvQualAvSepCorrFctn.h"
#include <cstdio>

#ifdef __ROOT__
ClassImp(QinvQualAvSepCorrFctn)
#endif
//____________________________
QinvQualAvSepCorrFctn::QinvQualAvSepCorrFctn(char* title, const int& nbinsQ, const float& QLo, const float& QHi,
					     const int& nbinsQual, const float& QualLo, const float& QualHi,
					     const int& nbinsSep, const float& SepLo, const float& SepHi) {
  // set up numeratorS
  char Tit[100];
  sprintf(Tit,"Num3D");
  strcat(Tit,title);
  mNumerator3D = new StHbt3DHisto(Tit,title,nbinsQ,QLo,QHi,nbinsQual,QualLo,QualHi,nbinsSep,SepLo,SepHi);

  // set up denominatorS
  sprintf(Tit,"Den3D");
  strcat(Tit,title);
  mDenominator3D = new StHbt3DHisto(Tit,title,nbinsQ,QLo,QHi,nbinsQual,QualLo,QualHi,nbinsSep,SepLo,SepHi);

  // set up ratioS
  sprintf(Tit,"Rat3D");
  strcat(Tit,title);
  mRatio3D = new StHbt3DHisto(Tit,title,nbinsQ,QLo,QHi,nbinsQual,QualLo,QualHi,nbinsSep,SepLo,SepHi);

  // these histograms should have errors associated with them...
  mNumerator3D->Sumw2();
  mDenominator3D->Sumw2();
  mRatio3D->Sumw2();

}

//____________________________
QinvQualAvSepCorrFctn::~QinvQualAvSepCorrFctn(){
  delete mNumerator3D;
  delete mDenominator3D;
  delete mRatio3D;
}
//_________________________
void QinvQualAvSepCorrFctn::Finish(){
  // here is where we should normalize, fit, etc...
  // we should NOT Draw() the histos (as I had done it below),
  // since we want to insulate ourselves from root at this level
  // of the code.  Do it instead at root command line with browser.
  mRatio3D->Divide(mNumerator3D,mDenominator3D,1.0,1.0);
}

//____________________________
StHbtString QinvQualAvSepCorrFctn::Report(){
  string stemp = "Qinv-QualityFactor Correlation Function Report:\n";
  char ctemp[100];
  sprintf(ctemp,"Number of entries in numerator:\t%E\n",
	  mNumerator3D->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in denominator:\t%E\n",
	  mDenominator3D->GetEntries());
  stemp += ctemp;
  StHbtString returnThis = stemp;
  return returnThis;
}
//____________________________
void QinvQualAvSepCorrFctn::AddRealPair(const StHbtPair* pair){

  double Qual = pair->quality();
  double AvSep = pair->NominalTpcAverageSeparation();
  double Q = fabs(pair->qInv());

  mNumerator3D->Fill(Q,Qual,AvSep,1.0);
}
//____________________________
void QinvQualAvSepCorrFctn::AddMixedPair(const StHbtPair* pair){

  double Qual = pair->quality();
  double AvSep = pair->NominalTpcAverageSeparation();
  double Q = fabs(pair->qInv());

  mDenominator3D->Fill(Q,Qual,AvSep,1.0);

}



