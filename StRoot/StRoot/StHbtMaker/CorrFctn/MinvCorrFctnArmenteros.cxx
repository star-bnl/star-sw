/***************************************************************************
 * 
 * $Id: MinvCorrFctnArmenteros.cxx,v 1.4 2003/09/02 17:58:20 perev Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *     A simple invariant-mass correlation function
 *
 ***************************************************************************
 *
 * $Log: MinvCorrFctnArmenteros.cxx,v $
 * Revision 1.4  2003/09/02 17:58:20  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2000/06/15 18:52:42  willson
 * HbtAnalysis() method must be cast to specific analysis
 * rotateEventCut installed
 *
 * Revision 1.2  2000/03/16 01:56:36  laue
 * Copy constructor added to some correlation functions
 *
 * Revision 1.1  2000/02/28 14:31:51  laue
 * Correlation function to make the Armenteros-Podolanski plot.
 *
 * Revision 1.2  1999/07/06 22:33:19  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/


#include "StHbtMaker/CorrFctn/MinvCorrFctnArmenteros.h"
//#include "StHbtMaker/Infrastructure/StHbtHisto.hh"
#include <cstdio>

#ifdef __ROOT__
  ClassImp(MinvCorrFctnArmenteros)
#endif



pairD armenteros(const StHbtPair*);
double ptArm(const StHbtPair*);
double alphaArm(const StHbtPair*);

//___________________________
MinvCorrFctnArmenteros::MinvCorrFctnArmenteros(char* title, 
					 const int& nbins1, const float& MinvLo1, const float& MinvHi1,
					 const int& nbins2, const float& MinvLo2, const float& MinvHi2){
  char theTitle[100];
  // set up numerator
  char TitNum[100] = "NumArmenteros1 ";
  sprintf(theTitle,"Num %s",title);
  mNumerator = new StHbt2DHisto(TitNum,theTitle,nbins1,MinvLo1,MinvHi1,nbins2,MinvLo2,MinvHi2);
  // set up denominator
  char TitDen[100] = "DenArmenteros1";
  sprintf(theTitle,"Den %s",title);
  mDenominator = new StHbt2DHisto(TitDen,theTitle,nbins1,MinvLo1,MinvHi1,nbins2,MinvLo2,MinvHi2);
  // set up difference
  char TitDif[100] = "DifArmenteros1";
  sprintf(theTitle,"Dif %s",title);
  mDifference = new StHbt2DHisto(TitDif,theTitle,nbins1,MinvLo1,MinvHi1,nbins2,MinvLo2,MinvHi2);
  // this next bit is unfortunately needed so that we can have many histos of same "title"
  // it is neccessary if we typedef StHbt1DHisto to TH1d (which we do)
  mNumerator->SetDirectory(0);
  mDenominator->SetDirectory(0);
  mDifference->SetDirectory(0);
  // default for mass window, can be changed via SetMassWindow(double, double);
  mRealPairs = 0;
  mMixedPairs = 0;
}

//____________________________
MinvCorrFctnArmenteros::~MinvCorrFctnArmenteros(){
  delete mNumerator;
  delete mDenominator;
  delete mDifference;
}
//_________________________
void MinvCorrFctnArmenteros::Finish(){
  int NEvents = 1;
  if (   dynamic_cast<StHbtAnalysis*>( HbtAnalysis() )   ) {
    if (   dynamic_cast<mikesEventCut*>( ((StHbtAnalysis*)HbtAnalysis())->EventCut() )   )
      NEvents = ((mikesEventCut*)((StHbtAnalysis*)HbtAnalysis())->EventCut())->NEventsPassed();
  }

  mNumerator->Scale(1./NEvents);
  mDenominator->Scale(1./NEvents);
  mDifference->Scale(1./NEvents);

  //double NumeratorInt = mNumerator->Integral();
  //double DenominatorInt = mDenominator->Integral();
  
  mDifference->Add(mNumerator,mDenominator,1.0,-1.*mRealPairs/mMixedPairs);

}

//____________________________
StHbtString MinvCorrFctnArmenteros::Report(){
  string stemp = "Minv Correlation Function Report:\n";
  char ctemp[100];
  sprintf(ctemp,"Number of entries in numerator:\t%E\n",mNumerator->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in denominator:\t%E\n",mDenominator->GetEntries());
  stemp += ctemp;
  sprintf(ctemp,"Number of entries in difference:\t%E\n",mDifference->GetEntries());
  stemp += ctemp;
  StHbtString returnThis = stemp;
  return returnThis;
}
//____________________________
inline void MinvCorrFctnArmenteros::AddRealPair(const StHbtPair* pair){
  if ( pair->mInv()>mLo && pair->mInv()<mHi || mHi == mLo) {
    pairD ptArmAlpha = armenteros(pair);
    mNumerator->Fill( ptArmAlpha.first, ptArmAlpha.second, 1.);
    mRealPairs++;
  }  
}
//____________________________
inline void MinvCorrFctnArmenteros::AddMixedPair(const StHbtPair* pair){ 
  if ( pair->mInv()>mLo && pair->mInv()<mHi || mHi == mLo) {
    pairD ptArmAlpha = armenteros(pair);
    mDenominator->Fill( ptArmAlpha.first, ptArmAlpha.second, 1.);
    mMixedPairs++;
  }
}
//____________________________
inline pairD armenteros(const StHbtPair* pair ) {
  StHbtThreeVector pp = pair->track1()->FourMomentum().vect();
  StHbtThreeVector pn = pair->track2()->FourMomentum().vect();
  float pdotn = pp.dot(pn);
  float ptotp2 = pp.mag2();
  float ptotn2 = pn.mag2();
  
  float ptot  = pair->fourMomentumSum().vect().mag();
  
  float ppp = ( ptotp2 + pdotn )/ptot;
  float ppn = ( ptotn2 + pdotn )/ptot;
  
  return pairD( (ppp - ppn)/(ppp + ppn), ::sqrt(fabs(ptotp2 - ppp*ppp)) );
}
