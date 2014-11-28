/***************************************************************************
 *
 * $Id: rotateToReactionPlaneEventCut.cxx,v 1.2 2001/04/02 16:15:48 jeromel Exp $
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
 * $Log: rotateToReactionPlaneEventCut.cxx,v $
 * Revision 1.2  2001/04/02 16:15:48  jeromel
 * Type cast in sprintf() statement for bool conversion (insure++ issue)
 *
 * Revision 1.1  2000/05/25 21:47:28  laue
 * new event cut which can be used to rotate an event around the z-axis
 *
 *
 **************************************************************************/

#include "StHbtMaker/Cut/rotateToReactionPlaneEventCut.h"
#include "Randomize.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"

#include <cstdio>

#ifdef __ROOT__
ClassImp(rotateToReactionPlaneEventCut)
#endif

rotateToReactionPlaneEventCut::rotateToReactionPlaneEventCut() : mRotation(true), mSmear(0) {
  mNEventsPassed =  mNEventsFailed = 0;
  engine = new HepJamesRandom();
  gauss = new RandGauss(engine);
} 
//------------------------------
//rotateToReactionPlaneEventCut::~rotateToReactionPlaneEventCut(){
//  /* noop */
//}
//------------------------------
bool rotateToReactionPlaneEventCut::Pass(const StHbtEvent* event){
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
      angle = gauss->shoot()*mSmear*2.*pi;  // gaussian
      ((StHbtEvent*)event)->RotateZ(-1.* event->ReactionPlane() + angle);
    }
    goodEvent ? mNEventsPassed++ : mNEventsFailed++ ;
  }
  
  return (goodEvent);
}
//------------------------------
StHbtString rotateToReactionPlaneEventCut::Report(){
  string Stemp="";
  char Ctemp[100];
  sprintf(Ctemp,"rotateToReactionPlaneEventCut: ");
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
