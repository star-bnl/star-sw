#include "StGammaEvent.h"

#include "StMuDSTMaker/COMMON/StMuTrack.h"

ClassImp(StGammaEvent);

// ------------------------------------------------------- class constructor --
StGammaEvent::StGammaEvent()
{
  InitArrays(); // initialize tclones arrays
  Clear();
}
// ------------------------------------------------------------- reset event --
void StGammaEvent::Clear(Option_t *opts)
{
  mRunNumber=0;
  mEventNumber=0;

  mVertex=TVector3(0.,0.,0.);

  mTracks->Clear();
  mTowers->Clear();
  mPreshower1->Clear();
  mPreshower2->Clear();
  mPostshower->Clear();
  mStrips->Clear();
  mCandidates->Clear();

  mFlags=0x0000;

  nTracks=0;
  nTowers=0;
  nPreshower1=0;
  nPreshower2=0;
  nPostshower=0;
  nStrips=0;
  nCandidates=0;
  mPythia = 0;
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
    track = new( tracks[nTracks++] ) StGammaTrack(mutr);
  else
    track = new( tracks[nTracks++] ) StGammaTrack();
  return track;
}

StGammaTower *StGammaEvent::newTower()
{
  TClonesArray &towers = *mTowers;
  StGammaTower *tower = new( towers[nTowers++] ) StGammaTower();
  return tower;
}

StGammaTower *StGammaEvent::newPreshower1()
{
  TClonesArray &preshower1 = *mPreshower1;
  StGammaTower *tower = new( preshower1[nPreshower1++] ) StGammaTower();
  return tower;
}

StGammaTower *StGammaEvent::newPreshower2()
{
  TClonesArray &preshower2 = *mPreshower2;
  StGammaTower *tower = new( preshower2[nPreshower2++] ) StGammaTower();
  return tower;
}

StGammaTower *StGammaEvent::newPostshower()
{
  TClonesArray &postshower = *mPostshower;
  StGammaTower *tower = new( postshower[nPostshower++] ) StGammaTower();
  return tower;
}

StGammaStrip *StGammaEvent::newStrip()
{
  TClonesArray &strips = *mStrips;
  StGammaStrip *strip = new( strips[nStrips++] ) StGammaStrip();
  return strip;
}

StGammaCandidate *StGammaEvent::newCandidate()
{
  TClonesArray &candidates = *mCandidates;
  StGammaCandidate *candidate = new( candidates[nCandidates++] ) StGammaCandidate();
  return candidate;
}

// ----------------------------------------------------------------------------

Float_t StGammaEvent::sumPt( Float_t min, Float_t max )
{
  return sumTrackPt(min,max) + sumTowerPt(min,max);
}
Float_t StGammaEvent::sumTrackPt(Float_t min, Float_t max)
{  
  Float_t sum = 0.;
  for ( Int_t i=0;i<numberOfTracks();i++ )
    {
      StGammaTrack *t=track(i);
      if ( t->eta() < min || t->eta() > max ) continue;
      sum += t->pt();
    }
  return sum;
}

Float_t StGammaEvent::sumTowerPt(Float_t min, Float_t max)
{  
  Float_t sum = 0.;
  for ( Int_t i=0;i<numberOfTowers();i++ )
    {
      StGammaTower *t=tower(i);
      if ( t->fail ) continue;      
      if ( t->eta < min || t->eta > max ) continue;
      sum += t->pt();
    }
  return sum;
}

