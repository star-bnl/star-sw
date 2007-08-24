#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StGammaPythiaEvent.h"

#include "StGammaEvent.h"

ClassImp(StGammaEvent);

// ------------------------------------------------------- class constructor --
StGammaEvent::StGammaEvent()
{
  mPythia = 0;
  InitArrays(); // initialize tclones arrays
  Clear();
}
// ------------------------------------------------------------- reset event --
void StGammaEvent::Clear(Option_t *opts)
{
  mRunNumber=0;
  mEventNumber=0;
  mTriggerIds.clear();
  mVertex=TVector3(0.,0.,0.);
  mTracks->Clear();
  mTowers->Clear();
  mPreshower1->Clear();
  mPreshower2->Clear();
  mPostshower->Clear();
  mStrips->Clear();
  mCandidates->Clear();

  mFlags=0x0000;

  if (mPythia) mPythia->Clear(opts);
}

// ------------------------------------------------------------ init tclones --
Int_t StGammaEvent::InitArrays()
{
  mTracks     = new TClonesArray("StGammaTrack",1000);
  mTowers     = new TClonesArray("StGammaTower",4800+720);
  mPreshower1 = new TClonesArray("StGammaTower",4800+720);
  mPreshower2 = new TClonesArray("StGammaTower",720);
  mPostshower = new TClonesArray("StGammaTower",720);
  mStrips     = new TClonesArray("StGammaStrip",10000);
  mCandidates = new TClonesArray("StGammaCandidate",10);
  return 1;
}

// ------------------------------------- methods to add tracks/towers/strips --
StGammaTrack *StGammaEvent::newTrack( StMuTrack *mutr )
{
  TClonesArray &tracks = *mTracks;
  StGammaTrack *track = 0;
  if ( mutr )
    track = new( tracks[tracks.GetEntriesFast()] ) StGammaTrack(mutr);
  else
    track = new( tracks[tracks.GetEntriesFast()] ) StGammaTrack;
  return track;
}

StGammaTower *StGammaEvent::newTower()
{
  TClonesArray &towers = *mTowers;
  StGammaTower *tower = new( towers[towers.GetEntriesFast()] ) StGammaTower;
  return tower;
}

StGammaTower *StGammaEvent::newPreshower1()
{
  TClonesArray &preshower1 = *mPreshower1;
  StGammaTower *tower = new( preshower1[preshower1.GetEntriesFast()] ) StGammaTower;
  return tower;
}

StGammaTower *StGammaEvent::newPreshower2()
{
  TClonesArray &preshower2 = *mPreshower2;
  StGammaTower *tower = new( preshower2[preshower2.GetEntriesFast()] ) StGammaTower;
  return tower;
}

StGammaTower *StGammaEvent::newPostshower()
{
  TClonesArray &postshower = *mPostshower;
  StGammaTower *tower = new( postshower[postshower.GetEntriesFast()] ) StGammaTower;
  return tower;
}

StGammaStrip *StGammaEvent::newStrip()
{
  TClonesArray &strips = *mStrips;
  StGammaStrip *strip = new( strips[strips.GetEntriesFast()] ) StGammaStrip;
  return strip;
}

StGammaCandidate *StGammaEvent::newCandidate()
{
  TClonesArray &candidates = *mCandidates;
  StGammaCandidate *candidate = new( candidates[candidates.GetEntriesFast()] ) StGammaCandidate;
  return candidate;
}

// ----------------------------------------------------------------------------

Float_t StGammaEvent::sumPt( Float_t eta_min, Float_t eta_max ) const
{
  return sumTrackPt(eta_min,eta_max) + sumTowerPt(eta_min,eta_max);
}

Float_t StGammaEvent::sumTrackPt(Float_t eta_min, Float_t eta_max) const
{  
  Float_t sum = 0.;
  for ( Int_t i=0;i<numberOfTracks();i++ )
    {
      StGammaTrack *t=track(i);
      if ( t->eta() < eta_min || t->eta() > eta_max ) continue;
      sum += t->pt();
    }
  return sum;
}

Float_t StGammaEvent::sumTowerPt(Float_t eta_min, Float_t eta_max) const
{  
  Float_t sum = 0.;
  for ( Int_t i=0;i<numberOfTowers();i++ )
    {
      StGammaTower *t=tower(i);
      if ( t->fail ) continue;      
      if ( t->eta < eta_min || t->eta > eta_max ) continue;
      sum += t->pt();
    }
  return sum;
}
