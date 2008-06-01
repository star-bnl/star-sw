// $Id: StJets.cxx,v 1.1 2008/06/01 03:41:45 tai Exp $
#include "StJet.h"
#include "StJets.h"

#include "TrackToJetIndex.h"

ClassImp(StJets)

using namespace std;

StJets::StJets()
  : mDylanPoints(0)
  , mSumEmcE(0.0)
  , mEventId(0)
  , mEventNumber(0)
  , mRunId(0)
  , mRunNumber(0)
  , mCorrupt(false)
  , mJets(new TClonesArray("StJet",100))
  , mTrackToJetIndices(new TClonesArray("TrackToJetIndex",200)) 
{
	
}

StJets::~StJets()
{
    mJets->Delete();
    delete mJets;
    mJets = 0;
	
    mTrackToJetIndices->Delete();
    delete mTrackToJetIndices;
    mTrackToJetIndices = 0;
}

void StJets::Clear(bool clearAll)
{
    mJets->Clear();
    mTrackToJetIndices->Clear();
    mDylanPoints = 0;
    mSumEmcE = 0.;
}

void StJets::addTrackToIndex(TrackToJetIndex& t2j)
{
  new ((*mTrackToJetIndices)[mTrackToJetIndices->GetLast() + 1]) TrackToJetIndex(t2j);
}

void StJets::addJet(StJet& jet)
{
  new((*mJets)[nJets()]) StJet(jet);
}

vector<TrackToJetIndex*> StJets::particles(int jetIndex)
{
    int size = mTrackToJetIndices->GetLast()+1;
    vector<TrackToJetIndex*> vec;
    
    for (int i=0; i<size; ++i) {
		TrackToJetIndex* id = static_cast<TrackToJetIndex*>( (*mTrackToJetIndices)[i] );
		if (id->jetIndex()==jetIndex) {
			vec.push_back(id);
		}
    }
    return vec;
}

TObjArray StJets::particles_(int jetIndex)
{
  TObjArray ret;

  vector<TrackToJetIndex*> vec = particles(jetIndex);
  for (vector<TrackToJetIndex*>::iterator iter = vec.begin(); iter != vec.end(); ++iter)
    ret.Add(*iter);

  return ret;
} 

bool StJets::inBounds(int i)
{
    return (i>0 && i<nJets());
}

double StJets::e(int i) 
{
    StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
    return (j) ? j->E() : -999.;
}

double StJets::et(int i) 
{
    StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
    return (j) ? j->et() : -999.;
}

double StJets::p(int i) 
{
    StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
    return (j) ? j->P() : -999.;
}

double StJets::pt(int i) 
{
    StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
    return (j) ? j->Pt() : -999.;
}

double StJets::phi(int i)
{
    StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
    return (j) ? j->Phi() : -999.;
}

double StJets::eta(int i) 
{
    StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
    return (j) ? j->Eta() : -999.;
}

int StJets::nCell(int i) 
{
    StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
    return (j) ? j->nCell : -999;
}

int StJets::charge(int i) 
{
    StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
    return (j) ? j->charge : -999;
}

