/***************************************************************************
 *
 * $Id: StHbtEvent.cc,v 1.12 2001/06/23 21:55:17 laue Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   HbtEvent is the "transient microDST"  Objects of this class are
 *   generated from the input data by a Reader, and then presented to
 *   the Cuts of the various active Analyses.
 *
 ***************************************************************************
 *
 * $Log: StHbtEvent.cc,v $
 * Revision 1.12  2001/06/23 21:55:17  laue
 * StHbtCheckPdgIdList can take can not check for mother,particle,daughter
 * Some output turned off
 *
 * Revision 1.11  2001/06/21 19:15:45  laue
 * Modified fiels:
 *   CTH.hh : new constructor added
 *   StHbtEvent, StHbtKink, StHbtTrack : constructors from the persistent
 *                                   (TTree) classes added
 *   StHbtLikeSignAnalysis : minor changes, for debugging
 *   StHbtTypes: split into different files
 * Added files: for the new TTree muDst's
 *   StExceptions.cxx StExceptions.hh StHbtEnumeration.hh
 *   StHbtHelix.hh StHbtHisto.hh StHbtString.hh StHbtTFile.hh
 *   StHbtTTreeEvent.cxx StHbtTTreeEvent.h StHbtTTreeKink.cxx
 *   StHbtTTreeKink.h StHbtTTreeTrack.cxx StHbtTTreeTrack.h
 *   StHbtTTreeV0.cxx StHbtTTreeV0.h StHbtVector.hh
 *
 * Revision 1.10  2001/05/15 15:30:16  rcwells
 * Added magnetic field to StHbtEvent
 *
 * Revision 1.9  2000/08/31 22:31:31  laue
 * StHbtAnalysis: output changed (a little bit less)
 * StHbtEvent: new version, members for reference mult added
 * StHbtIOBinary: new IO for new StHbtEvent version
 * StHbtTypes: TTree typedef to StHbtTTree added
 * StHbtVertexAnalysis: overflow and underflow added
 *
 * Revision 1.8  2000/07/16 21:38:22  laue
 * StHbtCoulomb.cxx StHbtSectoredAnalysis.cxx : updated for standalone version
 * StHbtV0.cc StHbtV0.hh : some cast to prevent compiling warnings
 * StHbtParticle.cc StHbtParticle.hh : pointers mTrack,mV0 initialized to 0
 * StHbtIOBinary.cc : some printouts in #ifdef STHBTDEBUG
 * StHbtEvent.cc : B-Field set to 0.25Tesla, we have to think about a better
 *                 solution
 *
 * Revision 1.7  2000/05/25 21:54:16  laue
 * RotateZ implemented. Rotates momentum and helix around the z axis
 *
 * Revision 1.5  2000/02/18 21:32:23  laue
 * franksTrackCut changed. If mCharge is set to '0' there will be no cut
 * on charge. This is important for front-loaded cuts.
 *
 * copy constructor implemented for StHbtEvent, StHbtTrack and StHbtV0.
 *
 * franks1HistoD.cxx franks1HistoD.h franks2HistoD.cxx franks2HistoD.h
 * removed. We can now (CC5 on Solaris) use the versions (no D)
 *
 * Revision 1.4  1999/09/16 18:47:59  lisa
 * replace placeholder HbtV0Track stuff with Helens StHbtV0 classes
 *
 * Revision 1.3  1999/07/27 10:47:04  lisa
 * now works in dev on linux and solaris - mistake in deleting picoEvents fixed
 *
 * Revision 1.2  1999/07/19 14:24:05  hardtke
 * modifications to implement uDST
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Infrastructure/StHbtV0.hh"
#include "StHbtMaker/Infrastructure/StHbtKink.hh"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Base/StHbtV0Cut.h"
#include "StHbtMaker/Base/StHbtKinkCut.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"

#ifdef __ROOT__
#include "StHbtMaker/Infrastructure/StHbtTTreeEvent.h"
#include "StHbtMaker/Infrastructure/StHbtTTreeTrack.h"
#include "StHbtMaker/Infrastructure/StHbtTTreeV0.h"
#include "StHbtMaker/Infrastructure/StHbtTTreeKink.h"

StHbtEvent::StHbtEvent(const StHbtTTreeEvent* ev) {
#ifdef STHBTDEBUG
  cout << "StHbtEvent::StHbtEvent(const StHbtTTreeEvent* ev)" << endl;
#endif
  mEventNumber = ev->mEventNumber;
  mRunNumber = ev->mRunNumber;
  mTpcNhits = ev->mTpcNhits;
  mNumberOfTracks = ev->mNumberOfTracks;
  mNumberOfGoodTracks = ev->mNumberOfGoodTracks;
  mCtbMultiplicity = ev->mCtbMultiplicity;
  mZdcAdc[0] = ev->mZdcAdc[0];
  mZdcAdc[1] = ev->mZdcAdc[1];
  mUncorrectedNumberOfPositivePrimaries = ev->mUncorrectedNumberOfPositivePrimaries;
  mUncorrectedNumberOfNegativePrimaries = ev->mUncorrectedNumberOfNegativePrimaries;
  mReactionPlane[0] = ev->mReactionPlane[0];
  mReactionPlane[1] = ev->mReactionPlane[1];
  mPrimVertPos = StHbtThreeVector(ev->mVertexX,ev->mVertexY,ev->mVertexZ);
  mMagneticField = ev->mMagneticField;
  if (mMagneticField==0) {
#if STHBTDEBUG
    cout << "StHbtEvent::StHbtEvent(const StHbtTTreeEvent* ev) - mMagneticField=" << mMagneticField << endl;
#endif
    mMagneticField = 2.5;
    ev->SetMagneticField(mMagneticField);
    cout << "StHbtEvent::StHbtEvent(const StHbtTTreeEvent* ev) - mMagneticField set to " << mMagneticField << endl;
  }

  // create collections
  mV0Collection = new StHbtV0Collection();
  mKinkCollection = new StHbtKinkCollection();
  mTrackCollection = new StHbtTrackCollection();

  // copy track collection  
  for ( unsigned int i=0; i <ev->mNtracks; i++) {
    StHbtTrack* trackCopy = new StHbtTrack(ev, (StHbtTTreeTrack*)ev->tracks()->UncheckedAt(i));
    mTrackCollection->push_back(trackCopy);
  }
  // copy v0 collection  
  for ( unsigned int i=0; i<ev->mNv0s; i++) {
//     StHbtV0* v0Copy = new StHbtV0(ev, (StHbtTTreeV0*)ev->v0s()->UncheckedAt(i));
//     mV0Collection->push_back(v0Copy);
  }
  // copy kink collection  
  for ( unsigned int i=0; i <ev->mNkinks; i++) {
    StHbtKink* kinkCopy = new StHbtKink(ev, (StHbtTTreeKink*)ev->kinks()->UncheckedAt(i));
    mKinkCollection->push_back(kinkCopy);
  }
}
#endif

//___________________
StHbtEvent::StHbtEvent(){
  mPrimVertPos[0]=-999.0;
  mPrimVertPos[1]=-999.0;
  mPrimVertPos[2]=-999.0;
  mTrackCollection = new StHbtTrackCollection;
  mV0Collection = new StHbtV0Collection;
  mKinkCollection = new StHbtKinkCollection;
  mMagneticField=0.0;
}
//___________________
StHbtEvent::StHbtEvent(const StHbtEvent& ev, StHbtTrackCut* tCut, StHbtV0Cut* vCut, StHbtKinkCut* kCut){ // copy constructor with track and v0 cuts
  //cout << "StHbtEvent::StHbtEvent(const StHbtEvent& ev, StHbtTrackCut* tCut, StHbtV0Cut* vCut, StHbtV0Cut* kCut)" << endl;
  mEventNumber = ev.mEventNumber;
  mRunNumber = ev.mRunNumber;
  mCtbMultiplicity = ev.mCtbMultiplicity;
  mZdcAdc[0] = ev.mZdcAdc[0];
  mZdcAdc[1] = ev.mZdcAdc[1];
  mTpcNhits = ev.mTpcNhits;
  mNumberOfTracks = ev.mNumberOfTracks;
  mNumberOfGoodTracks = ev.mNumberOfGoodTracks;
  mUncorrectedNumberOfPositivePrimaries = ev.mUncorrectedNumberOfPositivePrimaries;
  mUncorrectedNumberOfNegativePrimaries = ev.mUncorrectedNumberOfNegativePrimaries;
  mReactionPlane[0] = ev.mReactionPlane[0];
  mReactionPlane[1] = ev.mReactionPlane[1];
  mPrimVertPos = ev.mPrimVertPos;
  mMagneticField= ev.mMagneticField;

  // create collections
  mTrackCollection = new StHbtTrackCollection;
  mV0Collection = new StHbtV0Collection;
  mKinkCollection = new StHbtKinkCollection;
  // copy track collection  
  for ( StHbtTrackIterator tIter=ev.mTrackCollection->begin(); tIter!=ev.mTrackCollection->end(); tIter++) {
    if ( !tCut || tCut->Pass(*tIter) ) {
      //cout << " trackCut passed " << endl;
      StHbtTrack* trackCopy = new StHbtTrack(**tIter);
      mTrackCollection->push_back(trackCopy);
    }
  }
  // copy v0 collection
  for ( StHbtV0Iterator vIter=ev.mV0Collection->begin(); vIter!=ev.mV0Collection->end(); vIter++) {
    if ( !vCut || vCut->Pass(*vIter) ) {
      StHbtV0* v0Copy = new StHbtV0(**vIter);
      mV0Collection->push_back(v0Copy);
    }
  }
  // copy kink collection  
  for ( StHbtKinkIterator kIter=ev.mKinkCollection->begin(); kIter!=ev.mKinkCollection->end(); kIter++) {
    if ( !kCut || kCut->Pass(*kIter) ) {
      //cout << " kinkCut passed " << endl;
      StHbtKink* kinkCopy = new StHbtKink(**kIter);
      mKinkCollection->push_back(kinkCopy);
    }
  }
}
//___________________
StHbtEvent::~StHbtEvent(){
#ifdef STHBTDEBUG
  cout << " StHbtEvent::~StHbtEvent() " << endl;
#endif
  for (StHbtTrackIterator iter=mTrackCollection->begin();iter!=mTrackCollection->end();iter++){
    delete *iter;
  }
  mTrackCollection->clear();
  delete mTrackCollection;
  //must do the same for the V0 collection
  for (StHbtV0Iterator V0iter=mV0Collection->begin();V0iter!=mV0Collection->end();V0iter++){
    delete *V0iter;
  }
  mV0Collection->clear();
  delete mV0Collection;
  //must do the same for the Kink collection
  for (StHbtKinkIterator kinkIter=mKinkCollection->begin();kinkIter!=mKinkCollection->end();kinkIter++){
    delete *kinkIter;
  }
  mKinkCollection->clear();
  delete mKinkCollection;
}
//___________________
void StHbtEvent::RotateZ(const double angle){

  StHbtTrackIterator iter;
  StHbtV0Iterator V0iter;

  StPhysicalHelixD helix;
  StHbtThreeVector p;
  StHbtThreeVector o;
  double c;

  mReactionPlane[0] += angle;
  cout << " StHbtEvent::RotateZ(const double angle) - angle=" << angle << " rad    ";
  cout << angle / degree << " deg " << endl; 
  for (iter=mTrackCollection->begin();iter!=mTrackCollection->end();iter++){
      p = (*iter)->P();    p.rotateZ(angle);  (*iter)->SetP(p);
      p= (*iter)->Helix().momentum(mMagneticField*kilogauss);
      o= (*iter)->Helix().origin();
      p.rotateZ(angle);
      o.rotateZ(angle);
      c= (*iter)->Helix().charge(mMagneticField*kilogauss);
      helix = StPhysicalHelixD(p,o,mMagneticField*kilogauss,c);
      (*iter)->SetHelix(helix);
  }
  for (V0iter=mV0Collection->begin();V0iter!=mV0Collection->end();V0iter++){
    p=(*V0iter)->decayVertexV0();  p.rotateX(angle);   (*V0iter)->SetdecayVertexV0(p);
    p=(*V0iter)->momV0();          p.rotateX(angle);   (*V0iter)->SetmomV0(p);    
    p=(*V0iter)->momPos();         p.rotateX(angle);   (*V0iter)->SetmomPos(p);    
    p=(*V0iter)->momNeg();         p.rotateX(angle);   (*V0iter)->SetmomNeg(p);
  }
}


void StHbtEvent::SetEventNumber(const unsigned short& event){mEventNumber = event;}
void StHbtEvent::SetRunNumber(const int& runNum){mRunNumber = runNum;}
void StHbtEvent::SetCtbMult(const unsigned short& mult){mCtbMultiplicity = mult;}
void StHbtEvent::SetZdcAdcEast(const unsigned short& adc){mZdcAdc[0]= adc;}
void StHbtEvent::SetZdcAdcWest(const unsigned short& adc){mZdcAdc[1]=adc;}
void StHbtEvent::SetNumberOfTpcHits(const int& nhits){mTpcNhits = nhits;}
void StHbtEvent::SetNumberOfTracks(const unsigned short& tracks){mNumberOfTracks = tracks;}
void StHbtEvent::SetNumberOfGoodTracks(const unsigned short& tracks){mNumberOfGoodTracks = tracks;}
void StHbtEvent::SetUncorrectedNumberOfPositivePrimaries(const unsigned int& tracks){mUncorrectedNumberOfPositivePrimaries = tracks;}
void StHbtEvent::SetUncorrectedNumberOfNegativePrimaries(const unsigned int& tracks){mUncorrectedNumberOfNegativePrimaries = tracks;}
void StHbtEvent::SetReactionPlane(const float& rp){mReactionPlane[0] = rp ;}
void StHbtEvent::SetReactionPlaneError(const float& rp ){ SetReactionPlaneSubEventDifference(rp); }
void StHbtEvent::SetReactionPlaneSubEventDifference(const float& rp ){mReactionPlane[1]=rp;}
void StHbtEvent::SetPrimVertPos(const StHbtThreeVector& vp){mPrimVertPos = vp;}
void StHbtEvent::SetMagneticField(const double& magF){mMagneticField = magF;}

unsigned short StHbtEvent::EventNumber() const {return mEventNumber;}
int            StHbtEvent::RunNumber() const {return mRunNumber;}
unsigned short StHbtEvent::CtbMult() const {return mCtbMultiplicity;}
unsigned short StHbtEvent::ZdcAdcEast() const {return mZdcAdc[0];}
unsigned short StHbtEvent::ZdcAdcWest() const {return mZdcAdc[1];}
int            StHbtEvent::NumberOfTpcHits() const {return mTpcNhits;}
unsigned short StHbtEvent::NumberOfTracks() const {return mNumberOfTracks;}
unsigned short StHbtEvent::NumberOfGoodTracks() const {return mNumberOfGoodTracks;}
unsigned int StHbtEvent::UncorrectedNumberOfPositivePrimaries() const {return mUncorrectedNumberOfPositivePrimaries;}
unsigned int StHbtEvent::UncorrectedNumberOfNegativePrimaries() const {return mUncorrectedNumberOfNegativePrimaries;}
float          StHbtEvent::ReactionPlane() const {return mReactionPlane[0];}
float          StHbtEvent::ReactionPlaneError() const {return ReactionPlaneSubEventDifference();}
float          StHbtEvent::ReactionPlaneSubEventDifference() const {return mReactionPlane[1];}
StHbtV0Collection* StHbtEvent::V0Collection() const {return mV0Collection;}
StHbtKinkCollection* StHbtEvent::KinkCollection() const {return mKinkCollection;}
StHbtTrackCollection* StHbtEvent::TrackCollection() const {return mTrackCollection;}
StHbtThreeVector StHbtEvent::PrimVertPos() const {return mPrimVertPos;}
double StHbtEvent::MagneticField() const {return mMagneticField;}
