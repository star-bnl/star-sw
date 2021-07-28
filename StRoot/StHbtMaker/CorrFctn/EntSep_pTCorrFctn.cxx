/***************************************************************************
 *
 * $Id: EntSep_pTCorrFctn.cxx,v 1.1 2000/09/14 18:36:54 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a simple Qinv-EntranceSeparation correlation function used for studying 2-track cuts
 *
 ***************************************************************************
 *
 * $Log: EntSep_pTCorrFctn.cxx,v $
 * Revision 1.1  2000/09/14 18:36:54  lisa
 * Added Qinv and ExitSep pair cuts and BPLCMSFrame3DCorrFctn_SIM CorrFctn
 * 
 *
 **************************************************************************/

#include "StHbtMaker/CorrFctn/EntSep_pTCorrFctn.h"
#include <cstdio>

#ifdef __ROOT__
ClassImp(EntSep_pTCorrFctn)
#endif


//____________________________
EntSep_pTCorrFctn::EntSep_pTCorrFctn(char* title, const int& nbinsQ, const float& QLo, const float& QHi,
					     const int& nbinsExSep, const float& ExSepLo, const float& ExSepHi){
  // set up numeratorS
  char Tit[100];
  sprintf(Tit,"2D Num");
  strcat(Tit,title);
  mNumerator2D = new StHbt2DHisto(Tit,title,nbinsQ,QLo,QHi,nbinsExSep,ExSepLo,ExSepHi);

  // set up denominatorS
  sprintf(Tit,"2D Den");
  strcat(Tit,title);
  mDenominator2D = new StHbt2DHisto(Tit,title,nbinsQ,QLo,QHi,nbinsExSep,ExSepLo,ExSepHi);

  // set up ratioS
  sprintf(Tit,"2D Rat");
  strcat(Tit,title);
  mRatio2D = new StHbt2DHisto(Tit,title,nbinsQ,QLo,QHi,nbinsExSep,ExSepLo,ExSepHi);

  // these histograms should have errors associated with them...
  mNumerator2D->Sumw2();
  mDenominator2D->Sumw2();
  mRatio2D->Sumw2();

}

//____________________________
EntSep_pTCorrFctn::~EntSep_pTCorrFctn(){
  delete mNumerator2D;
  delete mDenominator2D;
  delete mRatio2D;
}
//_________________________
void EntSep_pTCorrFctn::Finish(){
  mRatio2D->Divide(mNumerator2D,mDenominator2D,1.0,1.0);
}

//____________________________
StHbtString EntSep_pTCorrFctn::Report(){
  string stemp = "EntSep_pT Correlation Function Report:\n";
  char ctemp[100];
  sprintf(ctemp,"Number of entries in numerator:\t%E\n",
	  mNumerator2D->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in denominator:\t%E\n",
	  mDenominator2D->GetEntries());
  stemp += ctemp;
  StHbtString returnThis = stemp;
  return returnThis;
}
//____________________________
void EntSep_pTCorrFctn::AddRealPair(const StHbtPair* pair){


  double entSep = pair->NominalTpcEntranceSeparation();
  //  double Qinv = fabs(pair->qInv());   // note - qInv() will be negative for identical pairs...
  double kT = pair->kT();

  mNumerator2D->Fill(entSep,kT,1.0);
}
//____________________________
void EntSep_pTCorrFctn::AddMixedPair(const StHbtPair* pair){

  double entSep = pair->NominalTpcEntranceSeparation();
  //  double Qinv = fabs(pair->qInv());   // note - qInv() will be negative for identical pairs...
  double kT = pair->kT();

  mDenominator2D->Fill(entSep,kT,1.0);
}

