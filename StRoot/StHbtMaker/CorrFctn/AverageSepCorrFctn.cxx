/***************************************************************************
 *
 * $Id: AverageSepCorrFctn.cxx,v 1.1 2000/10/05 23:08:58 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a simple Qinv-AverageSeparation correlation function used for studying 2-track cuts
 *
 ***************************************************************************
 *
 * $Log: AverageSepCorrFctn.cxx,v $
 * Revision 1.1  2000/10/05 23:08:58  lisa
 * Added kT-dependent radii to mixed-event simulator AND implemented AverageSeparation Cut and CorrFctn
 *
 * 
 **************************************************************************/

#include "StHbtMaker/CorrFctn/AverageSepCorrFctn.h"
#include <cstdio>

#ifdef __ROOT__
ClassImp(AverageSepCorrFctn)
#endif


//____________________________
AverageSepCorrFctn::AverageSepCorrFctn(char* title, const int& nbinsQ, const float& QLo, const float& QHi,
					     const int& nbinsExSep, const float& ExSepLo, const float& ExSepHi){
  // set up numeratorS
  char Tit[100];
  sprintf(Tit,"2DNum");
  strcat(Tit,title);
  mNumerator2D = new StHbt2DHisto(Tit,title,nbinsQ,QLo,QHi,nbinsExSep,ExSepLo,ExSepHi);

  // set up denominatorS
  sprintf(Tit,"2DDen");
  strcat(Tit,title);
  mDenominator2D = new StHbt2DHisto(Tit,title,nbinsQ,QLo,QHi,nbinsExSep,ExSepLo,ExSepHi);

  // set up ratioS
  sprintf(Tit,"2DRat");
  strcat(Tit,title);
  mRatio2D = new StHbt2DHisto(Tit,title,nbinsQ,QLo,QHi,nbinsExSep,ExSepLo,ExSepHi);

  // these histograms should have errors associated with them...
  mNumerator2D->Sumw2();
  mDenominator2D->Sumw2();
  mRatio2D->Sumw2();

}

//____________________________
AverageSepCorrFctn::~AverageSepCorrFctn(){
  delete mNumerator2D;
  delete mDenominator2D;
  delete mRatio2D;
}
//_________________________
void AverageSepCorrFctn::Finish(){
  mRatio2D->Divide(mNumerator2D,mDenominator2D,1.0,1.0);
}

//____________________________
StHbtString AverageSepCorrFctn::Report(){
  string stemp = "Average Seperation Correlation Function Report:\n";
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
void AverageSepCorrFctn::AddRealPair(const StHbtPair* pair){

  // MALisa July2000 - take explicit calculation of exit points and exit separation out of this
  // class and put it into StHbtParticle and StHbtPair where they belong
  //  StHbtThreeVector exitPt1 = FindExitPoint(pair->track1()->Helix());
  //  StHbtThreeVector exitPt2 = FindExitPoint(pair->track2()->Helix());
  //  StHbtThreeVector diff = exitPt1 - exitPt2;
  //  double exitSep = diff.mag();

  double aveSep = pair->NominalTpcAverageSeparation();
  double Qinv = fabs(pair->qInv());   // note - qInv() will be negative for identical pairs...

  mNumerator2D->Fill(Qinv,aveSep,1.0);
}
//____________________________
void AverageSepCorrFctn::AddMixedPair(const StHbtPair* pair){

  double aveSep = pair->NominalTpcAverageSeparation();
  double Qinv = fabs(pair->qInv());   // note - qInv() will be negative for identical pairs...

  mDenominator2D->Fill(Qinv,aveSep,1.0);
}

