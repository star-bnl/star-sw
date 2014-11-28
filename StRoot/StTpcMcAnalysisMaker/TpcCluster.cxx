#include "TpcCluster.h"
#include "StTpcHit.h"
#include "StMcEvent/StMcTpcHit.hh"
#include "StMcEvent/StMcTrack.hh"
TClonesArray *TpcCluster::fgPixels = 0;
TClonesArray *TpcCluster::fgMcHit = 0;
TClonesArray *TpcCluster::fgRcHit = 0;
TClonesArray *TpcCluster::fgRcTrack = 0;
TClonesArray *TpcCluster::fgRcTHit = 0;

ClassImp(TpcCluster)
ClassImp(TpcTrack)
//______________________________________________________________________________
TpcCluster::TpcCluster() : TObject() {
   if (!fgPixels) fgPixels = new TClonesArray("StTpcPixel", 1000);
   if (!fgMcHit)  fgMcHit  = new TClonesArray("StMcTpcHit",  1000);
   if (!fgRcHit)  fgRcHit  = new TClonesArray("StTpcHit", 1000);
   if (!fgRcTrack)  fgRcTrack  = new TClonesArray("TpcTrack", 1000);
   if (!fgRcTHit)  fgRcTHit  = new TClonesArray("StTpcHit", 1000);
   fPixels = fgPixels;
   fMcHit  = fgMcHit;
   fRcHit  = fgRcHit;
   fRcTrack  = fgRcTrack;
   fRcTHit  = fgRcTHit;
   DVelWest =   DVelEast =   fNofPV =  fNoTracksAtBestPV = -1;
   fxV =    fyV=   fzV = 0;
   
   Clear();
}
//______________________________________________________________________________
void TpcCluster::Clear(Option_t *option)
{
  fAdcSum = 0;
  fPixels->Clear(); fNoPixels   = 0;
  fMcHit->Clear();  fNoMcHit    = 0;
  fRcHit->Clear();  fNoRcHit    = 0;
  fRcTrack->Clear();  fNoRcTrack    = 0;
  fRcTHit->Clear();  fNoRcTHit    = 0;
  fSector = fRow = 0;
}

//______________________________________________________________________________
void TpcCluster::Reset(Option_t *option)
{
   SafeDelete(fgPixels);
   SafeDelete(fgMcHit);
   SafeDelete(fgRcHit);
   SafeDelete(fgRcTrack);
   SafeDelete(fgRcTHit);
}
//______________________________________________________________________________
void TpcCluster::AddMcHit(const StMcTpcHit* hit) {
   TClonesArray &hits = *fMcHit;
   StMcTpcHit *mchit = new(hits[fNoMcHit++]) StMcTpcHit(*hit);
   StMcTrack *track = mchit->parentTrack();
   Int_t key = -1;
   if (track) key =  track->key();
   mchit->setKey(key);
   mchit->setParentTrack(0);
}
//______________________________________________________________________________
void TpcCluster::AddRcHit(const StTpcHit* hit) {
   TClonesArray &hits = *fRcHit;
   new(hits[fNoRcHit++]) StTpcHit(*hit);
}
//______________________________________________________________________________
void TpcCluster::AddRcTHit(const StTpcHit* hit) {
   TClonesArray &hits = *fRcTHit;
   new(hits[fNoRcTHit++]) StTpcHit(*hit);
}
//______________________________________________________________________________
void TpcCluster::AddPixel(const StTpcPixel* Signal) {
   TClonesArray &pixels = *fPixels;
   new(pixels[fNoPixels++]) StTpcPixel(*Signal);
}
//______________________________________________________________________________
void TpcCluster::AddRcTrack(const TpcTrack* track) {
   TClonesArray &tracks = *fRcTrack;
   new(tracks[fNoRcTrack++]) TpcTrack(*track);
}
