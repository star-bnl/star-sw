/***************************************************************************
 *
 * $Id: mikesStarStandardEventCut.cxx,v 1.1 2000/09/04 16:27:15 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   A simple event-wise cut that selects on multiplicity and z-position
 *   of primary vertex.  This one calculates and then cuts on the number of
 *   negative primary tracks with -0.5<eta<0.5, which is the STAR standard.
 *   The cuts are (copied from StEventUtilities/StuRefMult.hh
 * primary tracks only
 * flag > 0
 * charge < 0
 * fit points >= 10
 * abs(eta) < 0.5
 * dca < 3 cm
 *
 * Also, this cut was used to check the correlation between the "old"
 * HBT multiplicity and the "new" STAR standard multiplicity definition,
 * by way of an output ASCII file.  I have commented this part out, but
 * just uncomment it in the constructor (where the file is openned) and
 * in the Pass() method (where the file is written to) and you will get
 * the file.
 *
 ***************************************************************************
 *
 * $Log: mikesStarStandardEventCut.cxx,v $
 * Revision 1.1  2000/09/04 16:27:15  lisa
 * added StarStandard multiplicity cut and modified mikesTrackCut to allow NOT cutting on charge sign
 *
 *
 **************************************************************************/

#include "StHbtMaker/Cut/mikesStarStandardEventCut.h"
#include <cstdio>



#ifdef __ROOT__
ClassImp(mikesStarStandardEventCut)
#endif

mikesStarStandardEventCut::mikesStarStandardEventCut(){
  mNEventsPassed =  mNEventsFailed = 0;
  //  mOutFile = new ofstream("Mult_vs_mult.dat");
} 
//------------------------------
//mikesStarStandardEventCut::~mikesStarStandardEventCut(){
//  mOutFile->close();
//  delete mOutFile;
//  /* noop */
//}
//------------------------------
bool mikesStarStandardEventCut::Pass(const StHbtEvent* event){


  double VertexZPos = event->PrimVertPos().z();
  cout << "mikesStarStandardEventCut:: VertexZPos: " << mVertZPos[0] << " < " << VertexZPos << " < " << mVertZPos[1] << endl;
  bool goodEvent =
    ((VertexZPos > mVertZPos[0]) &&
     (VertexZPos < mVertZPos[1]));
  if (goodEvent){
    //  int mult =  event->NumberOfTracks();
    int mult = 0;
    StHbtTrack* track;
    for (StHbtTrackIterator pIter=event->TrackCollection()->begin();pIter!=event->TrackCollection()->end();pIter++){
      track = *pIter;
      if ((track->Charge()<0)&&(track->NHits()>=10)&&(track->DCAxy()<3.0)&&(fabs(track->P().pseudoRapidity())<0.5)){
	mult++;
      }
    }

    //    (*mOutFile) << event->NumberOfTracks() << " " << mult << " " << VertexZPos << endl;
    cout << "mikesStarStandardEventCut:: mult:       " << mEventMult[0] << " < " << mult << " < " << mEventMult[1] << endl;
    goodEvent = (goodEvent&& (mult > mEventMult[0]) && (mult < mEventMult[1]));
  }
  
  goodEvent ? mNEventsPassed++ : mNEventsFailed++ ;
  cout << "mikesStarStandardEventCut:: return : " << goodEvent << endl;


  return (goodEvent);
}
//------------------------------
StHbtString mikesStarStandardEventCut::Report(){
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
