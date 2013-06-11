//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 1 september 2009
//

#include "TClonesArray.h"
#include "StJetCandidate.h"
#include "StJetEvent.h"
#include "StJetTrack.h"
#include "StJetTower.h"

ClassImp(StJetEvent);

StJetEvent::StJetEvent()
  : mRunId(0), mEventId(0)
  , mJets(new TClonesArray("StJetCandidate", 100))
  , mTracks(new TClonesArray("StJetTrack", 100))
  , mTowers(new TClonesArray("StJetTower", 100))
{
}

StJetEvent::~StJetEvent()
{
  mJets->Delete();
  mTracks->Delete();
  mTowers->Delete();

  delete mJets;
  delete mTracks;
  delete mTowers;

  mJets = 0;
  mTracks = 0;
  mTowers = 0;
}

void StJetEvent::Clear(Option_t* option)
{
  mJets->Clear(option);
  mTracks->Clear(option);
  mTowers->Clear(option);
}

int StJetEvent::numberOfJets() const { return mJets->GetEntriesFast(); }
int StJetEvent::numberOfTracks() const { return mTracks->GetEntriesFast(); }
int StJetEvent::numberOfTowers() const { return mTowers->GetEntriesFast(); }

StJetCandidate* StJetEvent::jet(int i) const { return (StJetCandidate*)mJets->At(i); }
StJetTrack* StJetEvent::track(int i) const { return (StJetTrack*)mTracks->At(i); }
StJetTower* StJetEvent::tower(int i) const { return (StJetTower*)mTowers->At(i); }

StJetCandidate* StJetEvent::addJet(const StJetCandidate* jet) { return new ((*mJets)[mJets->GetEntriesFast()]) StJetCandidate(*jet); }
StJetTrack* StJetEvent::newTrack() { return new ((*mTracks)[mTracks->GetEntriesFast()]) StJetTrack; }
StJetTower* StJetEvent::newTower() { return new ((*mTowers)[mTowers->GetEntriesFast()]) StJetTower; }
