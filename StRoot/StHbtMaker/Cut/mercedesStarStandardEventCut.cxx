/***************************************************************************
 *
 * $Id: mercedesStarStandardEventCut.cxx,v 1.2 2003/01/17 16:47:18 mercedes Exp $
 *
 * Author: Mercedes Lopez Noriega , Ohio State, mercedes@pacific.mps.ohio-state.edu
 *
 ***************************************************************************
 *
 * Description:part of STAR HBT Framework: StHbtMaker package
 * A event-wise cut taht cuts on multiplicity ("new" STAR standard multipliity 
 * definition for charge hadrons, 2001 data) and:
 * primary tracks only
 * flag > 0
 * fit points >= 10
 * abs(eta) < 0.5
 * dca < 3 cm
 * 
 ***************************************************************************
 *
 * $Log: mercedesStarStandardEventCut.cxx,v $
 * Revision 1.2  2003/01/17 16:47:18  mercedes
 * Updated to get reference multiplicity
 *
 * Revision 1.1  2002/02/22 15:50:13  mercedes
 * added StarStandard multiplicity cut for charge hadrons (2001 data)
 *
 * Revision 1.1  2002/02/22 16:27:15  mercedes
 * added StarStandard multiplicity cut for charge hadrons
 *
 **************************************************************************/


#include "StHbtMaker/Cut/mercedesStarStandardEventCut.h"
#include <cstdio>

#ifdef __ROOT__
ClassImp(mercedesStarStandardEventCut)
#endif

mercedesStarStandardEventCut::mercedesStarStandardEventCut(){
  mNEventsPassed =  mNEventsFailed = 0;
} 

//------------------------------
bool mercedesStarStandardEventCut::Pass(const StHbtEvent* event){

  double VertexZPos = event->PrimVertPos().z();
  cout << "mercedesStarStandardEventCut:: VertexZPos: " << mVertZPos[0] << " < " << VertexZPos << " < " << mVertZPos[1] << endl;
  bool goodEvent =
    ((VertexZPos > mVertZPos[0]) &&
     (VertexZPos < mVertZPos[1]));
  if (goodEvent){
    // this is the StMuEvent::refMult():
    int mult = event->UncorrectedNumberOfPrimaries();

    //    int mult = 0;
    //     StHbtTrack* track;
    //     for (StHbtTrackIterator pIter=event->TrackCollection()->begin();pIter!=event->TrackCollection()->end();pIter++){
    //       track = *pIter;
    //       if ((track->NHits()>=10)&&(track->DCAxy()<3.0)&&(fabs(track->P().pseudoRapidity())<0.5)){
    //         mult++;
    //       }
    //     }

    cout << "mercedesStarStandardEventCut:: mult:       " << mEventMult[0] << " < " << mult << " < " << mEventMult[1] << endl;
    goodEvent = (goodEvent&& (mult > mEventMult[0]) && (mult < mEventMult[1]));
  }
  
  goodEvent ? mNEventsPassed++ : mNEventsFailed++ ;
  cout << "mercedesStarStandardEventCut:: return : " << goodEvent << endl;
  
  return (goodEvent);
}

//------------------------------
StHbtString mercedesStarStandardEventCut::Report(){
  string Stemp;
  char Ctemp[100];
  sprintf(Ctemp,"\nMultiplicity:\t %d-%d",mEventMult[0],mEventMult[1]);
  Stemp = Ctemp;
  sprintf(Ctemp,"\nVertex Z-position:\t %E-%E",mVertZPos[0],mVertZPos[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"\nNumber of events which passed:\t%ld  Number which failed:\t%ld",mNEventsPassed,mNEventsFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}
