//-------------------------------------------------
// For StTpcEvalMaker
//-------------------------------------------------
// author: milton toy
// additions: manuel cbs
//-------------------------------------------------
// class definition of StTpcEvalHistograms
//-------------------------------------------------
#include <iostream.h>
#include <stdlib.h>
#include <string>
#include <vector>
#include <math.h>

#include "TH1.h"
#include "TH2.h"
#include "TNtuple.h"
#include "TClonesArray.h"

#include "StTpcEvalEvent.h"
#include "StTpcEvalHistograms.h"

ClassImp(StTpcEvalHistograms)

//-------------------------------------------------

StTpcEvalHistograms::StTpcEvalHistograms() {
    mTrackIndex = 0;
}
StTpcEvalHistograms::~StTpcEvalHistograms() {
}

//-------------------------------------------------
// for ntuple definition...
#define TRKNT_ENUM gId,gPx,gPy,gPz,gY,gHits,gHitMatch,g2rTrackMatch,rId,rPx,rPy,rPz,rHits,rEta,rHitMatch,r2gTrackMatch,commonHits,xRes,xResRMS,zRes,zResRMS,enumLast
#define TRKNT_VAR "gId:gPx:gPy:gPz:gY:gHits:gHitMatch:g2rTrackMatch:rId:rPx:rPy:rPz:rHits:rEta:rHitMatch:r2gTrackMatch:commonHits:xRes:xResRMS:zRes:zResRMS"
//-------------------------------------------------

void StTpcEvalHistograms::fillTrackNtuple(MatchedTrackPair* trackPair) {

  // ntuple can't have entries of different
  // types...
  // that's what the TTree is for.
 
  enum {TRKNT_ENUM};
  float ntEntries[enumLast];

  ntEntries[gId] = trackPair->mcInfo()->trackId();
  ntEntries[gPx] = trackPair->mcInfo()->fourMomentum().px();
  ntEntries[gPy] = trackPair->mcInfo()->fourMomentum().py();
  ntEntries[gPz] = trackPair->mcInfo()->fourMomentum().pz();
  ntEntries[gY]  = trackPair->mcInfo()->fourMomentum().rapidity();
  ntEntries[gHits] = trackPair->mcInfo()->numberOfHits();
  ntEntries[gHitMatch] = trackPair->mcInfo()->numberOfMatchedHits();
  ntEntries[g2rTrackMatch] = trackPair->mcInfo()->numberOfMatchedTracks();

  ntEntries[rId] = trackPair->rcInfo()->trackId();
  ntEntries[rPx] = trackPair->rcInfo()->momentum().x();
  ntEntries[rPy] = trackPair->rcInfo()->momentum().y();
  ntEntries[rPz] = trackPair->rcInfo()->momentum().z();
  ntEntries[rEta]= trackPair->rcInfo()->momentum().pseudoRapidity();
  ntEntries[rHits] = trackPair->rcInfo()->numberOfHits();
  ntEntries[rHitMatch] = trackPair->rcInfo()->numberOfMatchedHits();
  ntEntries[r2gTrackMatch] = trackPair->rcInfo()->numberOfMatchedTracks();

  ntEntries[commonHits] = trackPair->commonHits();

  ntEntries[xRes] = trackPair->spatialResolution().x();
  ntEntries[xResRMS] = trackPair->spatialResolutionRMS().x();
  ntEntries[zRes] = trackPair->spatialResolution().z();
  ntEntries[zResRMS] = trackPair->spatialResolutionRMS().z();

  trackNtuple->Fill(ntEntries);
  
}

//-------------------------------------------------

void StTpcEvalHistograms::Book() {
  int lwidth = 2;
  tpcHitResX = new TH1F("tpcHitResX","",100,-1.0,1.0);
  tpcHitResX->SetXTitle("hit resolution, localX (cm)");
  tpcHitResX->SetLineWidth(lwidth);
  
  tpcHitResY = new TH1F("tpcHitResY","",100,-1.0,1.0);
  tpcHitResY->SetXTitle("hit resolution, localY (cm)");
  tpcHitResY->SetLineWidth(lwidth);

  tpcHitResZ = new TH1F("tpcHitResZ","",100,-1.0,1.0);
  tpcHitResZ->SetXTitle("hit resolution, localZ (cm)");
  tpcHitResZ->SetLineWidth(lwidth);

  matchesToRcHits = new TH1F("mc2rcMatchedHits","",10,0.0,10.0);
  matchesToRcHits->SetXTitle("number of StTpcHit matched to a StMcTpcHit");
  matchesToRcHits->SetLineWidth(lwidth);

  matchesToMcHits = new TH1F("rc2mcMatchedHits","",10,0.0,10.0);
  matchesToMcHits->SetXTitle("number of StMcTpcHit matched to a StTpcHit");
  matchesToMcHits->SetLineWidth(lwidth);

  mcHitPositionRad = new TH1F("mcHitPositionRad","",45,1.0,46.0);
  mcHitPositionRad->SetXTitle("radial position (padrow)");
  mcHitPositionRad->SetLineWidth(lwidth);

  mcHitPositionZ = new TH1F("mcHitPositionZ","",100,-210.0,210.0);
  mcHitPositionZ->SetXTitle("z position (cm)");
  mcHitPositionZ->SetLineWidth(lwidth);

  mcUnmatchedHitPositionRad = new TH1F("mcUnmatchedHitPositionRad","",45,1.0,46.0);
  mcUnmatchedHitPositionRad->SetXTitle("radial position (padrow)");
  mcUnmatchedHitPositionRad->SetLineWidth(lwidth);

  mcUnmatchedHitPositionSector = new TH1F("mcUnmatchedHitPositionSector","",24,1.0,25.0);
  mcUnmatchedHitPositionSector->SetXTitle("Sector");
  mcUnmatchedHitPositionSector->SetLineWidth(lwidth);
  
  mcUnmatchedHitPositionZ = new TH1F("mcUnmatchedHitPositionZ","",100,-210.0,210.0);
  mcUnmatchedHitPositionZ->SetXTitle("z position (cm)");
  mcUnmatchedHitPositionZ->SetLineWidth(lwidth);

  mc1to1HitPositionRad = new TH1F("mc1to1HitPositionRad","",45,1.0,46.0);
  mc1to1HitPositionRad->SetXTitle("radial position (padrow)");
  mc1to1HitPositionRad->SetLineWidth(lwidth);

  mc1to1HitPositionZ = new TH1F("mc1to1HitPositionZ","",100,-210.0,210.0);
  mc1to1HitPositionZ->SetXTitle("z position (cm)");
  mc1to1HitPositionZ->SetLineWidth(lwidth);

  mcMergedHitPositionRad = new TH1F("mcMergedHitPositionRad","",45,1.0,46.0);
  mcMergedHitPositionRad->SetXTitle("radial position (padrow)");
  mcMergedHitPositionRad->SetLineWidth(lwidth);

  mcMergedHitPositionZ = new TH1F("mcMergedHitPositionZ","",100,-210.0,210.0);
  mcMergedHitPositionZ->SetXTitle("z position (cm)");
  mcMergedHitPositionZ->SetLineWidth(lwidth);

  rcHitPositionRad = new TH1F("rcHitPositionRad","",45,1.0,46.0);
  rcHitPositionRad->SetXTitle("radial position (padrow)");
  rcHitPositionRad->SetLineWidth(lwidth);

  rcHitPositionZ = new TH1F("rcHitPositionZ","",100,-210.0,210.0);
  rcHitPositionZ->SetXTitle("z position (cm)");
  rcHitPositionZ->SetLineWidth(lwidth);

  rcUnmatchedHitPositionRad = new TH1F("rcUnmatchedHitPositionRad","",45,1.0,46.0);
  rcUnmatchedHitPositionRad->SetXTitle("radial position (padrow)");
  rcUnmatchedHitPositionRad->SetLineWidth(lwidth);

  rcUnmatchedHitPositionSector = new TH1F("rcUnmatchedHitPositionSector","",24,1.0,25.0);
  rcUnmatchedHitPositionSector->SetXTitle("Sector");
  rcUnmatchedHitPositionSector->SetLineWidth(lwidth);

  rcUnmatchedHitPositionZ = new TH1F("rcUnmatchedHitPositionZ","",100,-210.0,210.0);
  rcUnmatchedHitPositionZ->SetXTitle("z position (cm)");
  rcUnmatchedHitPositionZ->SetLineWidth(lwidth);

  char* ntVars = TRKNT_VAR;
  trackNtuple = new TNtuple("trackNtuple","Track Pair Ntuple",ntVars);

  
}

//-------------------------------------------------
