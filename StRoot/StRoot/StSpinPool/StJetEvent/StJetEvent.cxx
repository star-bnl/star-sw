//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 1 september 2009
//

#include "TClonesArray.h"
#include "TVector3.h"
#include "TLorentzVector.h"
#include "StJetEventTypes.h"

ClassImp(StJetEvent);

StJetEvent::StJetEvent()
  : mRunId(0)
  , mEventId(0)
  , mVertices(new TClonesArray("StJetVertex",10))
  , mJets(new TClonesArray("StJetCandidate",10))
  , mTracks(new TClonesArray("StJetTrack",50))
  , mTowers(new TClonesArray("StJetTower",50))
  , mParticles(new TClonesArray("StJetParticle",50))
{
}

StJetEvent::~StJetEvent()
{
  mVertices->Delete(); delete mVertices; mVertices = 0;
  mJets->Delete(); delete mJets; mJets = 0;
  mTracks->Delete(); delete mTracks; mTracks = 0;
  mTowers->Delete(); delete mTowers; mTowers = 0;
  mParticles->Delete(); delete mParticles; mParticles = 0;
}

void StJetEvent::Clear(Option_t* option)
{
  mVertices->Clear(option);
  mJets->Clear(option);
  mTracks->Clear(option);
  mTowers->Clear(option);
  mParticles->Clear(option);
}

int StJetEvent::numberOfVertices () const { return mVertices->GetEntriesFast(); }
int StJetEvent::numberOfJets     () const { return mJets->GetEntriesFast(); }
int StJetEvent::numberOfTracks   () const { return mTracks->GetEntriesFast(); }
int StJetEvent::numberOfTowers   () const { return mTowers->GetEntriesFast(); }
int StJetEvent::numberOfParticles() const { return mParticles->GetEntriesFast(); }

StJetVertex*    StJetEvent::vertex  (int i) const { return (StJetVertex*)mVertices->At(i); }
StJetCandidate* StJetEvent::jet     (int i) const { return (StJetCandidate*)mJets->At(i); }
StJetTrack*     StJetEvent::track   (int i) const { return (StJetTrack*)mTracks->At(i); }
StJetTower*     StJetEvent::tower   (int i) const { return (StJetTower*)mTowers->At(i); }
StJetParticle*  StJetEvent::particle(int i) const { return (StJetParticle*)mParticles->At(i); }

StJetVertex* StJetEvent::newVertex() { return new ((*mVertices)[mVertices->GetEntriesFast()]) StJetVertex; }
StJetCandidate* StJetEvent::newJet(const TVector3& vertex, const TLorentzVector& fourMomentum, float area, float area_error) { return new ((*mJets)[mJets->GetEntriesFast()]) StJetCandidate(vertex,fourMomentum, area, area_error); }
StJetTrack* StJetEvent::newTrack() { return new ((*mTracks)[mTracks->GetEntriesFast()]) StJetTrack; }
StJetTower* StJetEvent::newTower() { return new ((*mTowers)[mTowers->GetEntriesFast()]) StJetTower; }
StJetParticle* StJetEvent::newParticle() { return new ((*mParticles)[mParticles->GetEntriesFast()]) StJetParticle; }
