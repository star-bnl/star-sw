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

#include "StHbtMaker/CorrFctn/OpeningAngleCorrFctn.h"
#include <cstdio>

#ifdef __ROOT__
ClassImp(OpeningAngleCorrFctn)
#endif
//____________________________
OpeningAngleCorrFctn::  OpeningAngleCorrFctn(char* title, const int& nbinsQ, const float& QLo, const float& QHi,
					     const int& nbinsAng, const float& AngLo, const float& AngHi){
  // set up numeratorS
  char Tit[100];
  sprintf(Tit,"2D Num");
  strcat(Tit,title);
  mNumerator2D = new StHbt2DHisto(Tit,title,nbinsQ,QLo,QHi,nbinsAng,AngLo,AngHi);

  // set up denominatorS
  sprintf(Tit,"2D Den");
  strcat(Tit,title);
  mDenominator2D = new StHbt2DHisto(Tit,title,nbinsQ,QLo,QHi,nbinsAng,AngLo,AngHi);

  // set up ratioS
  sprintf(Tit,"2D Rat");
  strcat(Tit,title);
  mRatio2D = new StHbt2DHisto(Tit,title,nbinsQ,QLo,QHi,nbinsAng,AngLo,AngHi);

  // these histograms should have errors associated with them...
  mNumerator2D->Sumw2();
  mDenominator2D->Sumw2();
  mRatio2D->Sumw2();

}

//____________________________
OpeningAngleCorrFctn::~OpeningAngleCorrFctn(){
  delete mNumerator2D;
  delete mDenominator2D;
  delete mRatio2D;
}
//_________________________
void OpeningAngleCorrFctn::Finish(){
  // here is where we should normalize, fit, etc...
  // we should NOT Draw() the histos (as I had done it below),
  // since we want to insulate ourselves from root at this level
  // of the code.  Do it instead at root command line with browser.
  mRatio2D->Divide(mNumerator2D,mDenominator2D,1.0,1.0);
}

//____________________________
StHbtString OpeningAngleCorrFctn::Report(){
  string stemp = "Opening Angle Correlation Function Report:\n";
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
void OpeningAngleCorrFctn::AddRealPair(const StHbtPair* pair){
  StHbtThreeVector p1 = pair->track1()->FourMomentum().vect();
  StHbtThreeVector p2 = pair->track2()->FourMomentum().vect();

  // get relative angle in plane transverse to z:
  //  StHbtThreeVector p1Tran = p1;  p1Tran.setZ(0.0);
  //StHbtThreeVector p2Tran = p2;  p2Tran.setZ(0.0);
  //double dAngTran = 57.296*acos((p1Tran.dot(p2Tran))/(p1Tran.mag()*p2Tran.mag()));
  //mNumeratorTran->Fill(dAngTran);

  // get "absolute" relative angle
  double dAngInv = 57.296*acos((p1.dot(p2))/(p1.mag()*p2.mag()));

  double Qinv = fabs(pair->qInv());   // note - qInv() will be negative for identical pairs...

  mNumerator2D->Fill(Qinv,dAngInv,1.0);
}
//____________________________
void OpeningAngleCorrFctn::AddMixedPair(const StHbtPair* pair){
  StHbtThreeVector p1 = pair->track1()->FourMomentum().vect();
  StHbtThreeVector p2 = pair->track2()->FourMomentum().vect();

  // get relative angle in plane transverse to z:
  // StHbtThreeVector p1Tran = p1;  p1Tran.setZ(0.0);
//   StHbtThreeVector p2Tran = p2;  p2Tran.setZ(0.0);
//   double dAngTran = 57.296*acos((p1Tran.dot(p2Tran))/(p1Tran.mag()*p2Tran.mag()));
//   mDenominatorTran->Fill(dAngTran);

  // get "absolute" relative angle
  double dAngInv = 57.296*acos((p1.dot(p2))/(p1.mag()*p2.mag()));
  double Qinv = fabs(pair->qInv());   // note - qInv() will be negative for identical pairs...

  mDenominator2D->Fill(Qinv,dAngInv,1.0);
}



