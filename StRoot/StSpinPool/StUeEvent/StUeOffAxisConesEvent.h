#ifndef ST_UE_OFFAXISCONES_EVENT_H
#define ST_UE_OFFAXISCONES_EVENT_H

#include "TObject.h"
#include "TClonesArray.h"

#include "StUeVertex.h"
//#include "StUeJet.h"
#include "StUeOffAxisConesJet.h"
#include "StUeOffAxisCones.h"

#include "StSpinPool/StJetEvent/StJetTrack.h"
#include "StSpinPool/StJetEvent/StJetTower.h"
#include "StSpinPool/StJetEvent/StJetParticle.h"

class StUeOffAxisConesEvent : public TObject{

 public:
  StUeOffAxisConesEvent(){
    mEventId = -1;
    mVertices = new TClonesArray("StUeVertex",20);
    mUeOffAxisConesJets = new TClonesArray("StUeOffAxisConesJet",50);
    mUeOffAxisCones = new TClonesArray("StUeOffAxisCones",2);
    mTracks = new TClonesArray("StJetTrack",100);
    mTowers = new TClonesArray("StJetTower",100);
    mParticles = new TClonesArray("StJetParticle",100);
  }
  ~StUeOffAxisConesEvent(){}

  void Clear(Option_t* option)
  {
    mVertices->Clear(option);
    mUeOffAxisConesJets->Clear(option);
    mUeOffAxisCones->Clear(option);
    mTracks->Clear(option);
    mTowers->Clear(option);
    mParticles->Clear(option);
  }

  void setEventId(int eventId){ mEventId = eventId;}
  int eventId() const{ return mEventId;}

  int numberOfVertices () const { return mVertices->GetEntriesFast(); }
  TClonesArray* vertices () const { return mVertices;  }
  StUeVertex*  vertex(int i = 0) const{return (StUeVertex*)mVertices->At(i);}
  StUeVertex* lastVertex() const{ return (StUeVertex*)mVertices->Last(); }
  StUeVertex*  newVertex(){return new ((*mVertices)[mVertices->GetEntriesFast()]) StUeVertex; }
  StUeOffAxisConesJet*  newUeOffAxisConesJet(){return new ((*mUeOffAxisConesJets)[mUeOffAxisConesJets->GetEntriesFast()]) StUeOffAxisConesJet; }
  StUeOffAxisCones*  newUeOffAxisCones(){return new ((*mUeOffAxisCones)[mUeOffAxisCones->GetEntriesFast()]) StUeOffAxisCones; }
  StJetTrack*  newTrack(){return new ((*mTracks)[mTracks->GetEntriesFast()]) StJetTrack; }
  StJetTower*  newTower(){return new ((*mTowers)[mTowers->GetEntriesFast()]) StJetTower; }
  StJetParticle*  newParticle(){return new ((*mParticles)[mParticles->GetEntriesFast()]) StJetParticle; }
 private:
  int mEventId;
  TClonesArray *mVertices;
  TClonesArray *mUeOffAxisConesJets;
  TClonesArray *mUeOffAxisCones;
  TClonesArray *mTracks;
  TClonesArray *mTowers;
  TClonesArray *mParticles;
  ClassDef(StUeOffAxisConesEvent, 1);
};
#endif
