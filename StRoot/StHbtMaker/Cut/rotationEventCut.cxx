/***************************************************************************
 *
 * $Id: rotationEventCut.cxx,v 1.4 2001/12/05 15:13:22 laue Exp $
 *
 * Author: Frank Laue, Ohio State, laue@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   A simple event-wise cut that selects on multiplicity and z-position
 *   of primary vertex and rotates the event around the z-axis           
 *
 ***************************************************************************
 *
 * $Log: rotationEventCut.cxx,v $
 * Revision 1.4  2001/12/05 15:13:22  laue
 * rotationEventCut.h: cut on l3 trigger algorithm
 * franksXiCut.cxx: updates
 *
 * Revision 1.3  2001/06/21 19:09:44  laue
 * updated to match changed base classes
 *
 * Revision 1.1  2000/06/14 18:22:33  laue
 * New event cut
 *
 * Revision 1.1  2000/05/25 21:47:28  laue
 * new event cut which can be used to rotate an event around the z-axis
 *
 **************************************************************************/

#include "StHbtMaker/Cut/rotationEventCut.h"
#include "Randomize.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StHbtParticle.hh"
#include "StHbtPair.hh"
#include "StHbtMaker/Infrastructure/StHbtEvent.hh"

#include <cstdio>

#ifdef __ROOT__
ClassImp(rotationEventCut)
#endif

rotationEventCut::rotationEventCut() : mRotation(false), mRandom(false), mSmear(0), mL3TriggerAlgorithm(0) {
  mNEventsPassed =  mNEventsFailed = 0;
  mEventRefMult[0]      = 0;  mEventRefMult[1]   = 100000;
  mEventMult[0]         = 0;  mEventMult[1]      = 100000;
  mEventMultGood[0]     = 0;  mEventMultGood[1]  = 100000;
  mNumberOfTracks[0]    = 0;  mNumberOfTracks[1] = 100000;
  mNumberOfV0s[0]       = 0;  mNumberOfV0s[1]    = 100000;
  mReactionPlaneError[0]=-10.; mReactionPlaneError[1]=+10.;
  
  engine = new HepJamesRandom();
  gauss = new RandGauss(engine);
} 
//------------------------------
//rotationEventCut::~rotationEventCut(){
//  /* noop */
//}
//------------------------------
bool rotationEventCut::Pass(const StHbtEvent* event){
  int refMult,mult,multGood;
  double VertexZPos;
  double angle;
  double smear;
  bool goodEvent = true;

    refMult =  event->UncorrectedNumberOfNegativePrimaries();
    mult =  event->NumberOfTracks();
    multGood =  event->NumberOfGoodTracks();
//   cout << " rotationEventCut::Pass(...)  refMult=" << refMult << "  mult=" << mult << "  multGood=" <<  multGood << endl;
//   cout << " rotationEventCut::Pass(...) mNumberOfV0s[0] event->V0Collection()->size() mNumberOfV0s[0] ";
//   cout << mNumberOfV0s[0] << " " << event->V0Collection()->size() << " " << mNumberOfV0s[1] << endl;

    VertexZPos = event->PrimVertPos().z();
    goodEvent = 
      (
       (refMult >= mEventRefMult[0]) && 
       (refMult <= mEventRefMult[1]) && 
       (mult >= mEventMult[0]) && 
       (mult <= mEventMult[1]) && 
       (multGood >= mEventMultGood[0]) && 
       (multGood <= mEventMultGood[1]) && 
       (event->ReactionPlaneError() >= mReactionPlaneError[0]) &&
       (event->ReactionPlaneError() <= mReactionPlaneError[1]) &&
       ((int)event->TrackCollection()->size() >= mNumberOfTracks[0]) &&
       ((int)event->TrackCollection()->size() <= mNumberOfTracks[1]) &&
       ((int)event->V0Collection()->size() >= mNumberOfV0s[0]) &&
       ((int)event->V0Collection()->size() <= mNumberOfV0s[1])
       );

    if (mL3TriggerAlgorithm) goodEvent = goodEvent && (mL3TriggerAlgorithm & event->L3TriggerAlgorithm(0));

    if ( mVertZPos[0] < mVertZPos[1]) 
      goodEvent = goodEvent && ( (VertexZPos >= mVertZPos[0]) && (VertexZPos <= mVertZPos[1]) );
    else {
      goodEvent = goodEvent && !( (VertexZPos >= mVertZPos[0]) && (VertexZPos <= mVertZPos[1]) );
      cout << " rotationEventCut::Pass(const StHbtEvent* event) : upper limit < lower limit  - inverting the cut" << endl; 
    }    

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


    if (event->PrimVertPos().x() == event->PrimVertPos().y() == event->PrimVertPos().z() ) {
      for ( int i=0; i<50; i++) cout << " rotationEventCut::Pass(...) - flagged bad from embedding " << endl;
      goodEvent = false;
    }

    //    if ( event->TrackCollection()->size() + event->V0Collection()->size() > 0 )  goodEvent=false;


    goodEvent ? mNEventsPassed++ : mNEventsFailed++ ;


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
  sprintf(Ctemp,"\n Random   :\t %d",mRandom);
  Stemp += Ctemp;
  sprintf(Ctemp,"\n Smear    :\t %E",mSmear);
  Stemp += Ctemp;
  sprintf(Ctemp,"\n Reference Multiplicity:\t %d-%d",mEventRefMult[0],mEventRefMult[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"\n Multiplicity:\t %d-%d",mEventMult[0],mEventMult[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"\n Multiplicity good tracks:\t %d-%d",mEventMultGood[0],mEventMultGood[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"\n NumberOfTracks:\t %d-%d",mNumberOfTracks[0],mNumberOfTracks[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"\n NumberOfV0s:\t %d-%d",mNumberOfV0s[0],mNumberOfV0s[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"\n Vertex Z-position:\t %E-%E",mVertZPos[0],mVertZPos[1]);
  Stemp += Ctemp;
  sprintf(Ctemp,"\n Number of events which passed:\t%ld  Number which failed:\t%ld",mNEventsPassed,mNEventsFailed);
  Stemp += Ctemp;
  StHbtString returnThis = Stemp;
  return returnThis;
}
