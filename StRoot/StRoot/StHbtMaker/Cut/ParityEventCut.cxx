/***************************************************************************
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *  This cut is for the Parity Violation studies.
 *  It is different from the other Event Cuts in that it interacts with
 *  the correlation function.  In the parity violation study, we want a
 *  number associated with EVENTS, not PAIRS.  Thus, this cut has a pair of
 *  histograms associated with it, one for real and one for mixed events.
 *  The AddRealPair() and AddMixedPair() methods of the correlation function
 *  have access to two data members of this EventCut class.
 *
 ***************************************************************************
 *
 **************************************************************************/

#include "StHbtMaker/Cut/ParityEventCut.h"
#include <cstdio>

#ifdef __ROOT__
ClassImp(ParityEventCut)
#endif

ParityEventCut::ParityEventCut(const char* title, const int& nbins, const float& Lo, const float& Hi){
  mNEventsPassed =  mNEventsFailed = 0;

  RealQuantity = MixedQuantity = 0.0;
  nReals = nMixed = 0;

  char Tit[100];
  sprintf(Tit,"Real Events");
  strcat(Tit,title);
  mReals = new StHbt1DHisto(Tit,title,nbins,Lo,Hi);
  sprintf(Tit,"Mixed Events");
  strcat(Tit,title);
  mMixed = new StHbt1DHisto(Tit,title,nbins,Lo,Hi);
}
//------------------------------
ParityEventCut::~ParityEventCut(){
  delete mReals;
  delete mMixed;
}
//------------------------------
bool ParityEventCut::Pass(const StHbtEvent* event){
  if (nReals>0){  // fill the Reals histo
    double temp = RealQuantity / nReals;
    mReals->Fill(temp);
    cout << "Real value was " << temp << " with " << nReals << " pairs" << endl;
    RealQuantity = 0.0;
    nReals = 0;
  }
  if (nMixed>0){  // fill the Mixed histo
    double temp = MixedQuantity / nMixed;
    mMixed->Fill(temp);
    cout << "Mixed value was " << temp << " with " << nMixed << " pairs" << endl;
    MixedQuantity = 0.0;
    nMixed = 0;
  }

  int mult =  event->NumberOfTracks();
  double VertexZPos = event->PrimVertPos().z();
  cout << "ParityEventCut::Pass -- mult, VertexZPos : " << mult << " " << VertexZPos << endl;
  bool goodEvent =
    ((mult > mEventMult[0]) && 
     (mult < mEventMult[1]) && 
     (VertexZPos > mVertZPos[0]) &&
     (VertexZPos < mVertZPos[1]));
  goodEvent ? mNEventsPassed++ : mNEventsFailed++ ;
  return (goodEvent);
}
//------------------------------
StHbtString ParityEventCut::Report(){
  string Stemp;
  char Ctemp[100];
  sprintf(Ctemp,"Parity EventCut\nMultiplicity:\t %d-%d\n",mEventMult[0],mEventMult[1]);
  Stemp = Ctemp;
  sprintf(Ctemp,"Vertex Z-position:\t %E-%E\n",mVertZPos[0],mVertZPos[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"Number of events which passed:\t%ld  Number which failed:\t%ld\n",mNEventsPassed,mNEventsFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}
