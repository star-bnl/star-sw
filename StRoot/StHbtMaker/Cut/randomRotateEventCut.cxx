/***************************************************************************
 *
 * $Id: randomRotateEventCut.cxx,v 1.2 2001/04/02 16:16:34 jeromel Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   A simple event-wise cut that selects on multiplicity and z-position
 *   of primary vertex and rotates the event around the z-axis           
 *
 ***************************************************************************
 *
 * $Log: randomRotateEventCut.cxx,v $
 * Revision 1.2  2001/04/02 16:16:34  jeromel
 * Type cast in sprintf() statement for bool conversion (insure++ issue)
 *
 * Revision 1.1  2000/05/25 21:47:27  laue
 * new event cut which can be used to rotate an event around the z-axis
 *
 *
 **************************************************************************/

#include "StHbtMaker/Cut/randomRotateEventCut.h"
#include "Randomize.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"

#include <cstdio>

#ifdef __ROOT__
ClassImp(randomRotateEventCut)
#endif

randomRotateEventCut::randomRotateEventCut() : mRotation(true) {
  mNEventsPassed =  mNEventsFailed = 0;
  engine = new HepRandom();
} 
//------------------------------
//randomRotateEventCut::~randomRotateEventCut(){
//  /* noop */
//}
//------------------------------
bool randomRotateEventCut::Pass(const StHbtEvent* event){
  int mult;
  double VertexZPos;
  double angle;
  bool goodEvent = true;

  if ( event->TrackCollection()->size() + event->V0Collection()->size() > 0 ) {
    mult =  event->NumberOfTracks();
    VertexZPos = event->PrimVertPos().z();
    goodEvent =
      ((mult > mEventMult[0]) && 
       (mult < mEventMult[1]) && 
       (VertexZPos > mVertZPos[0]) &&
       (VertexZPos < mVertZPos[1]));
    
    if (goodEvent && mRotation ) {
      angle = engine->flat()*2.*pi;
      ((StHbtEvent*)event)->RotateZ(angle);
    }
    goodEvent ? mNEventsPassed++ : mNEventsFailed++ ;
  }
  return (goodEvent);
}
//------------------------------
StHbtString randomRotateEventCut::Report(){
  string Stemp="";
  char Ctemp[100];
  sprintf(Ctemp,"\n randomRotateEventCut:");
  Stemp += Ctemp;
  sprintf(Ctemp,"\n Rotation :\t %d",(int) mRotation);
  Stemp += Ctemp;
  sprintf(Ctemp,"\n Multiplicity:\t %d-%d",mEventMult[0],mEventMult[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"\n Vertex Z-position:\t %E-%E",mVertZPos[0],mVertZPos[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"\n Number of events which passed:\t%ld  Number which failed:\t%ld",mNEventsPassed,mNEventsFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}
