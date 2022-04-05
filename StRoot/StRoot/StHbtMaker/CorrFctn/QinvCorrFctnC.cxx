/***************************************************************************
 *
 * $Id: QinvCorrFctnC.cxx,v 1.3 2000/03/20 16:42:48 laue Exp $
 *
 * Author: Randy Wells, Ohio State, rcwells@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   a simple Q-invariant correlation function with Coulomb correction
 *   does HBT for pi- pi-
 * Can be changed -> double charge = -1.0 to do pi+ pi- HBT           
 *
 ***************************************************************************
 *
 * $Log: QinvCorrFctnC.cxx,v $
 * Revision 1.3  2000/03/20 16:42:48  laue
 * Bug fix by Randy
 *
 * Revision 1.2  2000/01/25 17:34:45  laue
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
 * Revision 1.1  1999/09/23 23:28:01  lisa
 * add helensV0Cut  AND  rename mikes and franks ParticleCuts to TrackCuts  AND  update documentation
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

#include "StHbtMaker/CorrFctn/QinvCorrFctnC.h"
//#include "StHbtMaker/Infrastructure/StHbtHisto.hh"
#include <cstdio>

#ifdef __ROOT__ 
ClassImp(QinvCorrFctnC)
#endif

//____________________________
QinvCorrFctnC::QinvCorrFctnC(char* title, const int& nbins, const float& QinvLo, const float& QinvHi){
  // set up numerator
  //  title = "Num Qinv (MeV/c)";
  char TitNum[100] = "Num";
  strcat(TitNum,title);
  mNumerator = new StHbt1DHisto(TitNum,title,nbins,QinvLo,QinvHi);
  // set up denominator
  //title = "Den Qinv (MeV/c)";
  char TitDen[100] = "Den";
  strcat(TitDen,title);
  mDenominator = new StHbt1DHisto(TitDen,title,nbins,QinvLo,QinvHi);
  // set up ratio
  //title = "Ratio Qinv (MeV/c)";
  char TitRat[100] = "Rat";
  strcat(TitRat,title);
  mRatio = new StHbt1DHisto(TitRat,title,nbins,QinvLo,QinvHi);
  // this next bit is unfortunately needed so that we can have many histos of same "title"
  // it is neccessary if we typedef StHbt1DHisto to TH1d (which we do)
  //mNumerator->SetDirectory(0);
  //mDenominator->SetDirectory(0);
  //mRatio->SetDirectory(0);

  // to enable error bar calculation...
  mNumerator->Sumw2();
  mDenominator->Sumw2();
  mRatio->Sumw2();

}

//____________________________
QinvCorrFctnC::~QinvCorrFctnC(){
  delete mNumerator;
  delete mDenominator;
  delete mRatio;
}
//_________________________
void QinvCorrFctnC::Finish(){
  // here is where we should normalize, fit, etc...
  // we should NOT Draw() the histos (as I had done it below),
  // since we want to insulate ourselves from root at this level
  // of the code.  Do it instead at root command line with browser.
  //  mNumerator->Draw();
  //mDenominator->Draw();
  //mRatio->Draw();
  double qInvTop = mNumerator->GetBinContent(50);
  double qInvBottom = mDenominator->GetBinContent(50);
  mRatio->Divide(mNumerator,mDenominator,qInvBottom,qInvTop);
}

//____________________________
StHbtString QinvCorrFctnC::Report(){
  string stemp = "Qinv Coulomb corrected Correlation Function Report:\n";
  char ctemp[100];
  sprintf(ctemp,"Number of entries in numerator:\t%E\n",mNumerator->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in denominator:\t%E\n",mDenominator->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in ratio:\t%E\n",mRatio->GetEntries());
  stemp += ctemp;
  //  stemp += mCoulombWeight->Report();
  StHbtString returnThis = stemp;
  return returnThis;
}
//____________________________
void QinvCorrFctnC::AddRealPair(const StHbtPair* pair){
  double Qinv = fabs(pair->qInv());   // note - qInv() will be negative for identical pairs...
  mNumerator->Fill(Qinv);
  //  cout << "QinvCorrFctnC::AddRealPair : " << pair->qInv() << " " << Qinv <<
  //" " << pair->track1().FourMomentum() << " " << pair->track2().FourMomentum() << endl;
}
//____________________________
void QinvCorrFctnC::AddMixedPair(const StHbtPair* pair){
  double Qinv = fabs(pair->qInv());   // note - qInv() will be negative for identical pairs...
  // I call SetRadius(7.0) in my macro so don't need to use it here.
  //  double weight = mCorrection.CoulombCorrect(pair,radius);
  double weight = mCorrection.CoulombCorrect(pair);
  mDenominator->Fill(Qinv,weight);
}

void QinvCorrFctnC::AddCorrection(const StHbtCoulomb* coulomb) {
  mCorrection = *coulomb;
}


