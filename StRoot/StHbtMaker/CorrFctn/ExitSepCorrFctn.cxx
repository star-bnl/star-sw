/***************************************************************************
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a simple opening-angle correlation function used for studying 2-track cuts
 *
 ***************************************************************************
 *
 **************************************************************************/

#include "StHbtMaker/CorrFctn/ExitSepCorrFctn.h"
#include "StThreeVectorD.hh"
#include <cstdio>

ClassImp(ExitSepCorrFctn)


StThreeVectorD FindExitPoint(const StPhysicalHelixD& pHelix);

//____________________________
ExitSepCorrFctn::ExitSepCorrFctn(char* title, const int& nbinsQ, const float& QLo, const float& QHi,
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
ExitSepCorrFctn::~ExitSepCorrFctn(){
  delete mNumerator2D;
  delete mDenominator2D;
  delete mRatio2D;
}
//_________________________
void ExitSepCorrFctn::Finish(){
  mRatio2D->Divide(mNumerator2D,mDenominator2D,1.0,1.0);
}

//____________________________
StHbtString ExitSepCorrFctn::Report(){
  string stemp = "Exit Seperation Correlation Function Report:\n";
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
void ExitSepCorrFctn::AddRealPair(const StHbtPair* pair){

  // Let's find the exit seperation for the particles
  // IF THEY WOULD BOTH ORIGINATE AT (0,0,0)

  StThreeVectorD exitPt1 = FindExitPoint(pair->track1()->Helix());
  StThreeVectorD exitPt2 = FindExitPoint(pair->track2()->Helix());
  StThreeVectorD diff = exitPt1 - exitPt2;

  double exitSep = diff.mag();
  double Qinv = fabs(pair->qInv());   // note - qInv() will be negative for identical pairs...

  mNumerator2D->Fill(Qinv,exitSep,1.0);
}
//____________________________
void ExitSepCorrFctn::AddMixedPair(const StHbtPair* pair){

  StThreeVectorD exitPt1 = FindExitPoint(pair->track1()->Helix());
  StThreeVectorD exitPt2 = FindExitPoint(pair->track2()->Helix());
  StThreeVectorD diff = exitPt1 - exitPt2;

  double exitSep = diff.mag();
  double Qinv = fabs(pair->qInv());   // note - qInv() will be negative for identical pairs...

  mDenominator2D->Fill(Qinv,exitSep,1.0);
}



//_______________ seperate function for calculating the exit point...
StThreeVectorD FindExitPoint(const StPhysicalHelixD& pHelix){
  static StThreeVectorD ZeroVec(0.,0.,0.);
  double dip, curv, phase;
  int h;
  curv = pHelix.curvature();
  dip  = pHelix.dipAngle();
  phase= pHelix.phase();
  h    = pHelix.h();
  StHelixD hel(curv,dip,phase,ZeroVec,h);

  pairD candidates;
  double sideLength;  // this is how much length to go to leave through sides of TPC
  double endLength;  // this is how much length to go to leave through endcap of TPC
  // figure out how far to go to leave through side...
  candidates = hel.pathLength(2.0);
  sideLength = (candidates.first > 0) ? candidates.first : candidates.second;

  static StThreeVectorD WestEnd(0.,0.,2.);
  static StThreeVectorD EastEnd(0.,0.,-2.);
  static StThreeVectorD EndCapNormal(0.,0.,1.0);

  endLength = hel.pathLength(WestEnd,EndCapNormal);
  if (endLength < 0.0) endLength = hel.pathLength(EastEnd,EndCapNormal);

  if (endLength < 0.0) cout << "FindExitPoint: Hey-- I cannot find an exit point out endcaps" << endl;

  // OK, firstExitLength will be the shortest way out of the detector...
  double firstExitLength = (endLength < sideLength) ? endLength : sideLength;

  // now then, let's return the POSITION at which particle leaves TPC...
  return hel.at(firstExitLength);
}
