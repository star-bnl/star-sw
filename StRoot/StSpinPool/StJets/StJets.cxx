// $Id: StJets.cxx,v 1.4 2010/06/30 17:51:58 pibero Exp $
#include "StJet.h"
#include "StJets.h"

#include "TrackToJetIndex.h"
#include "TowerToJetIndex.h"

ClassImp(StJets);

StJets::StJets()
  : mDylanPoints(0)
  , mSumEmcE(0)
  , mEventId(0)
  , mEventNumber(0)
  , mRunId(0)
  , mRunNumber(0)
  , mCorrupt(false)
  , mJets(new TClonesArray("StJet",100))
  , mTrackToJetIndices(new TClonesArray("TrackToJetIndex",100)) 
  , mTowerToJetIndices(new TClonesArray("TowerToJetIndex",100))
{
}

StJets::~StJets()
{
  mJets->Delete();
  mTrackToJetIndices->Delete();
  mTowerToJetIndices->Delete();

  delete mJets;
  delete mTrackToJetIndices;
  delete mTowerToJetIndices;

  mJets = 0;
  mTrackToJetIndices = 0;
  mTowerToJetIndices = 0;
}

void StJets::Clear(bool clearAll)
{
  mJets->Clear();
  mTrackToJetIndices->Clear();
  mTowerToJetIndices->Clear();
  mDylanPoints = 0;
  mSumEmcE = 0.;
}

void StJets::addTrackToIndex(TrackToJetIndex& t2j)
{
  new ((*mTrackToJetIndices)[mTrackToJetIndices->GetEntriesFast()]) TrackToJetIndex(t2j);
}

void StJets::addTowerToIndex(TowerToJetIndex& t2j)
{
  new ((*mTowerToJetIndices)[mTowerToJetIndices->GetEntriesFast()]) TowerToJetIndex(t2j);
}

void StJets::addJet(StJet& jet)
{
  new((*mJets)[nJets()]) StJet(jet);
}

TObjArray StJets::tracks(int jetIndex) const
{
  TObjArray a;
  for (int i = 0; i < mTrackToJetIndices->GetEntriesFast(); ++i) {
    TrackToJetIndex* track = (TrackToJetIndex*)mTrackToJetIndices->At(i);
    if (track->jetIndex() == jetIndex) a.Add(track);
  }
  return a;
}

TObjArray StJets::towers(int jetIndex) const
{
  TObjArray a;
  for (int i = 0; i < mTowerToJetIndices->GetEntriesFast(); ++i) {
    TowerToJetIndex* tower = (TowerToJetIndex*)mTowerToJetIndices->At(i);
    if (tower->jetIndex() == jetIndex) a.Add(tower);
  }
  return a;
}

vector<TLorentzVector*> StJets::particles(int jetIndex) const
{
  vector<TLorentzVector*> v;
  for (int i = 0; i < mTrackToJetIndices->GetEntriesFast(); ++i) {
    TrackToJetIndex* track = (TrackToJetIndex*)mTrackToJetIndices->At(i);
    if (track->jetIndex() == jetIndex) v.push_back(track);
  }
  for (int i = 0; i < mTowerToJetIndices->GetEntriesFast(); ++i) {
    TowerToJetIndex* tower = (TowerToJetIndex*)mTowerToJetIndices->At(i);
    if (tower->jetIndex() == jetIndex) v.push_back(tower);
  }
  return v;
}

bool StJets::inBounds(int i)
{
  return (i>0 && i<nJets());
}

double StJets::e(int i)  const
{
  StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
  return (j) ? j->E() : -999.;
}

double StJets::et(int i) const
{
  StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
  return (j) ? j->et() : -999.;
}

double StJets::p(int i) const
{
  StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
  return (j) ? j->P() : -999.;
}

double StJets::pt(int i) const
{
  StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
  return (j) ? j->Pt() : -999.;
}

double StJets::phi(int i) const
{
  StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
  return (j) ? j->Phi() : -999.;
}

double StJets::eta(int i) const
{
  StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
  return (j) ? j->Eta() : -999.;
}

int StJets::nCell(int i) const
{
  StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
  return (j) ? j->nCell : -999;
}

int StJets::charge(int i) const
{
  StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
  return (j) ? j->charge : -999;
}
