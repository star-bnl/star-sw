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
}

void StJetEvent::Clear(Option_t* option)
{
  mVertices->Clear(option);
  mJets->Clear(option);
  mTracks->Clear(option);
  mTowers->Clear(option);
}

int StJetEvent::numberOfVertices() const { return mVertices->GetEntriesFast(); }
const StJetVertex* StJetEvent::vertex(int i) const { return (StJetVertex*)mVertices->At(i); }
StJetVertex* StJetEvent::newVertex() { return new ((*mVertices)[mVertices->GetEntriesFast()]) StJetVertex; }
StJetCandidate* StJetEvent::newJet(const TVector3& vertex, const TLorentzVector& fourMomentum) { return new ((*mJets)[mJets->GetEntriesFast()]) StJetCandidate(vertex,fourMomentum); }
StJetTrack* StJetEvent::newTrack() { return new ((*mTracks)[mTracks->GetEntriesFast()]) StJetTrack; }
StJetTower* StJetEvent::newTower() { return new ((*mTowers)[mTowers->GetEntriesFast()]) StJetTower; }
StJetParticle* StJetEvent::newParticle() { return new ((*mParticles)[mParticles->GetEntriesFast()]) StJetParticle; }
