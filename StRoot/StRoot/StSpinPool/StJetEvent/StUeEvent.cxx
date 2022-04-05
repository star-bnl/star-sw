// -*- mode: c++ -*-

//
// Grant Webb <gdwebb@bnl.gov>
// Brookhaven National Laboratory
// 30 July 2015
//
#include "TClonesArray.h"
#include "TVector3.h"
#include "TLorentzVector.h"
#include "StJetEventTypes.h"

ClassImp(StUeEvent);

StUeEvent::StUeEvent()
  : mRunId(0)
  , mEventId(0)
  , mVertices(new TClonesArray("StJetVertex",10))
  , mTracks(new TClonesArray("StJetTrack",50))
  , mTowers(new TClonesArray("StJetTower",50))
  , mParticles(new TClonesArray("StJetParticle",50))
{
}

StUeEvent::~StUeEvent()
{
  mVertices->Delete(); delete mVertices; mVertices = 0;
  mTracks->Delete(); delete mTracks; mTracks = 0;
  mTowers->Delete(); delete mTowers; mTowers = 0;
  mParticles->Delete(); delete mParticles; mParticles = 0;
}
void StUeEvent::Clear(Option_t* option)
{
  mVertices->Clear(option);
  mTracks->Clear(option);
  mTowers->Clear(option);
  mParticles->Clear(option);
}

int StUeEvent::numberOfVertices () const { return mVertices->GetEntriesFast(); }
int StUeEvent::numberOfTracks   () const { return mTracks->GetEntriesFast(); }
int StUeEvent::numberOfTowers   () const { return mTowers->GetEntriesFast(); }
int StUeEvent::numberOfParticles() const { return mParticles->GetEntriesFast(); }

StJetVertex*    StUeEvent::vertex  (int i) const { return (StJetVertex*)mVertices->At(i); }
StJetTrack*     StUeEvent::track   (int i) const { return (StJetTrack*)mTracks->At(i); }
StJetTower*     StUeEvent::tower   (int i) const { return (StJetTower*)mTowers->At(i); }
StJetParticle*  StUeEvent::particle(int i) const { return (StJetParticle*)mParticles->At(i); }

StJetVertex* StUeEvent::newVertex() { return new ((*mVertices)[mVertices->GetEntriesFast()]) StJetVertex; }
StJetTrack* StUeEvent::newTrack() { return new ((*mTracks)[mTracks->GetEntriesFast()]) StJetTrack; }
StJetTower* StUeEvent::newTower() { return new ((*mTowers)[mTowers->GetEntriesFast()]) StJetTower; }
StJetParticle* StUeEvent::newParticle() { return new ((*mParticles)[mParticles->GetEntriesFast()]) StJetParticle; }

float StUeEvent::sumTrackPt() const
{
  float s = 0;
  for (int i = 0; i < numberOfTracks(); ++i) s += track(i)->pt();
  return s;
}

float StUeEvent::sumTowerPt() const
{
  float s = 0;
  for (int i = 0; i < numberOfTowers(); ++i) s += tower(i)->pt();
  return s;
}

float StUeEvent::sumParticlePt() const
{
  float s = 0;
  for (int i = 0; i < numberOfParticles(); ++i) s += particle(i)->pt();
  return s;
}
