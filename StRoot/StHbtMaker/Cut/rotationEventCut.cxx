/***************************************************************************
 *
 * $Id: rotationEventCut.cxx,v 1.1 2000/06/14 18:22:33 laue Exp $
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
 * $Log: rotationEventCut.cxx,v $
 * Revision 1.1  2000/06/14 18:22:33  laue
 * New event cut
 *
 * Revision 1.1  2000/05/25 21:47:28  laue
 * new event cut which can be used to rotate an event around the z-axis
 *
 *
 **************************************************************************/

#include "StHbtMaker/Cut/rotationEventCut.h"
#include "Randomize.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"

#include <cstdio>

#ifdef __ROOT__
ClassImp(rotationEventCut)
#endif

rotationEventCut::rotationEventCut() : mRotation(false), mRandom(false), mSmear(0) {
  mNEventsPassed =  mNEventsFailed = 0;
  mNumberOfTracks[0] = 0;  mNumberOfTracks[1] = 100000;
  mNumberOfV0s[0]    = 0;  mNumberOfV0s[1]    = 100000;
  engine = new HepJamesRandom();
  gauss = new RandGauss(engine);
} 
//------------------------------
//rotationEventCut::~rotationEventCut(){
//  /* noop */
//}
//------------------------------
bool rotationEventCut::Pass(const StHbtEvent* event){
  int mult;
  double VertexZPos;
  double angle;
  double smear;
  bool goodEvent = true;
  
  if ( event->TrackCollection()->size() + event->V0Collection()->size() > 0 ) {
    mult =  event->NumberOfTracks();
    VertexZPos = event->PrimVertPos().z();
    goodEvent =
      ((mult >= mEventMult[0]) && 
       (mult <= mEventMult[1]) && 
       (VertexZPos >= mVertZPos[0]) &&
       (VertexZPos <= mVertZPos[1]) &&
       ((int)event->TrackCollection()->size() >= mNumberOfTracks[0]) &&
       ((int)event->TrackCollection()->size() <= mNumberOfTracks[1]) &&
       ((int)event->V0Collection()->size() >= mNumberOfV0s[0]) &&
       ((int)event->V0Collection()->size() <= mNumberOfV0s[1])
       );

    if (goodEvent && mRotation ) {
      angle = gauss->shoot()*2.*pi;  // gaussian
      ((StHbtEvent*)event)->RotateZ(-1.* event->ReactionPlane() + angle);
    }

    if (goodEvent && mRandom ) {
      angle = engine->flat()*2.*pi;
      ((StHbtEvent*)event)->RotateZ(angle);
    }

    if (goodEvent && mSmear!=0 ) {
      smear = gauss->shoot()*mSmear/360.*2*pi;  // gaussian
      ((StHbtEvent*)event)->RotateZ(smear);
    }

    goodEvent ? mNEventsPassed++ : mNEventsFailed++ ;
  }
  
  return (goodEvent);
}
//------------------------------
StHbtString rotationEventCut::Report(){
  string Stemp="";
  char Ctemp[100];
  sprintf(Ctemp,"rotationEventCut: ");
  Stemp += Ctemp;
  sprintf(Ctemp,"\n Rotation :\t %d",mRotation);
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
