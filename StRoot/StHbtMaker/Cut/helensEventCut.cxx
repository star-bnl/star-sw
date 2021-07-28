/***************************************************************************
 *
 * $Id: helensEventCut.cxx,v 1.1 2000/10/09 21:56:15 laue Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   A simple event-wise cut that selects on multiplicity and z-position
 *   of primary vertex           
 *
 ***************************************************************************
 *
 * $Log: helensEventCut.cxx,v $
 * Revision 1.1  2000/10/09 21:56:15  laue
 * Helens new cuts
 *
 * Revision 1.7  2000/02/18 21:27:10  laue
 * franksTrackCut changed. If mCharge is set to '0' there will be no cut
 * on charge. This is important for front-loaded cuts.
 *
 * Revision 1.6  2000/01/25 17:35:02  laue
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
 * Revision 1.5  1999/10/15 01:57:03  lisa
 * Important enhancement of StHbtMaker - implement Franks CutMonitors
 * ----------------------------------------------------------
 * This means 3 new files in Infrastructure area (CutMonitor),
 * several specific CutMonitor classes in the Cut area
 * and a new base class in the Base area (StHbtCutMonitor).
 * This means also changing all Cut Base class header files from .hh to .h
 * so we have access to CutMonitor methods from Cint command line.
 * This last means
 * 1) files which include these header files are slightly modified
 * 2) a side benefit: the TrackCuts and V0Cuts no longer need
 * a SetMass() implementation in each Cut class, which was stupid.
 * Also:
 * -----
 * Include Franks StHbtAssociationReader
 * ** None of these changes should affect any user **
 *
 * Revision 1.4  1999/07/24 16:24:20  lisa
 * adapt StHbtMaker to dev version of library - solaris still gives problems with strings
 *
 * Revision 1.3  1999/07/19 14:24:04  hardtke
 * modifications to implement uDST
 *
 * Revision 1.2  1999/07/06 22:33:21  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#include "StHbtMaker/Cut/helensEventCut.h"
#include <cstdio>

#ifdef __ROOT__
ClassImp(helensEventCut)
#endif

helensEventCut::helensEventCut(){
  mNEventsPassed =  mNEventsFailed = 0;
  mV0Mult[0] =-10;
  mV0Mult[1] = 100000;
  mTrkV0MatchCollection = new StHbtTrkV0MatchCollection;
} 
//------------------------------
//helensEventCut::~helensEventCut(){
//  /* noop */
//}
//------------------------------
bool helensEventCut::Pass(const StHbtEvent* event){

  int v0Mult=0;
  int mult =  event->NumberOfTracks();
  if( event->V0Collection()){
    v0Mult= event->V0Collection()->size();
  }
  double VertexZPos = event->PrimVertPos().z();
  cout << "helensEventCut:: mult:       " << mEventMult[0] << " < " << mult << " < " << mEventMult[1] << endl;
  cout << "helensEventCut:: VertexZPos: " << mVertZPos[0] << " < " << VertexZPos << " < " << mVertZPos[1] << endl;
  cout << "helensEventCut:: V0Mult: " << mV0Mult[0] << " < " << v0Mult << " < " << mV0Mult[1] << endl;
  bool goodEvent =
    ((mult > mEventMult[0]) && 
     (mult < mEventMult[1]) && 
     (VertexZPos > mVertZPos[0]) &&
     (VertexZPos < mVertZPos[1]) &&
     (v0Mult > mV0Mult[0]) &&
     (v0Mult < mV0Mult[1]));
  goodEvent ? mNEventsPassed++ : mNEventsFailed++ ;


  // Delete list from previous event


  StHbtTrkV0Iterator pIter;
  if( TrkV0MatchCollection()->size() > 0){
    for( pIter=TrkV0MatchCollection()->begin(); pIter!=TrkV0MatchCollection()->end();
	 pIter++){
      delete *pIter;
    }
  
    TrkV0MatchCollection()->clear();
  }
  
  // Now build list for this event. There is one entry in list per track
  
  StHbtTrackIterator TrkIter;
  StHbtTrack* pParticle;  

  for( TrkIter=event->TrackCollection()->begin(); TrkIter!=event->TrackCollection()->end(); TrkIter++){
    
    pParticle = *TrkIter;
    StHbtTrkV0Match* TrackV0Match = new StHbtTrkV0Match;
    TrackV0Match->SetTrkId( pParticle->TrackId());
    TrackV0Match->SetdEdx( pParticle->dEdx());
    TrackV0Match->SetUsed(0);
    TrkV0MatchCollection()->push_back(TrackV0Match);
    
  }
  
  cout << "TrkV0MatchCollection Size= " << TrkV0MatchCollection()->size() << endl;
  cout << "helensEventCut:: return : " << goodEvent << endl;
  return (goodEvent);
}
//------------------------------
StHbtString helensEventCut::Report(){
  string Stemp;
  char Ctemp[100];
  sprintf(Ctemp,"\nMultiplicity:\t %d-%d",mEventMult[0],mEventMult[1]);
  Stemp = Ctemp;
  sprintf(Ctemp,"\nVertex Z-position:\t %E-%E",mVertZPos[0],mVertZPos[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"\nV0Multiplicity:\t %d-%d",mV0Mult[0],mV0Mult[1]);
  Stemp = Ctemp;
  sprintf(Ctemp,"\nNumber of events which passed:\t%ld  Number which failed:\t%ld",mNEventsPassed,mNEventsFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}
