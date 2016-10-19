/*************************************************
 *
 * $Id: StTpcMcAnalysisMaker.cxx,v 1.11 2015/01/28 21:20:02 fisyak Exp $
 * $Log: StTpcMcAnalysisMaker.cxx,v $
 * Revision 1.11  2015/01/28 21:20:02  fisyak
 * Freeze
 *
 * Revision 1.10  2012/09/25 13:38:13  fisyak
 * Freeze
 *
 * Revision 1.9  2010/11/09 19:37:00  fisyak
 * Add option to dump multiple clusters (for Tonko)
 *
 * Revision 1.8  2010/11/09 16:32:45  fisyak
 * rename TTree and add RcHit
 *
 * Revision 1.7  2009/01/26 15:08:09  fisyak
 * Expand TNTuple
 *
 * Revision 1.6  2008/04/25 15:32:04  fisyak
 * Freeze
 *
 * Revision 1.5  2007/12/03 23:57:12  fisyak
 * Move macros for TpcT analysis here
 *
 * Revision 1.4  2005/09/28 21:34:25  fisyak
 * Persistent StMcEvent
 *
 * Revision 1.3  2004/06/04 17:08:18  fisyak
 * move Track to Clone Array
 *
 * Revision 1.2  2004/05/29 21:15:15  fisyak
 * Add track info
 *
 * Revision 1.1.1.1  2004/05/13 22:57:50  fisyak
 * TPC pixel/cluster analysis
 *
*************************************************/
#include <assert.h>
#include <Stiostream.h> 
#include <stdlib.h>
#include <string>
#include <vector>
#include <set>
#include <map>
#include <cmath>
#include "TNtuple.h"
#include "TFile.h"
#ifndef HEP_SYSTEM_OF_UNITS_H
#include "SystemOfUnits.h"
#endif
#include "StMagF.h"
#include "StTpcMcAnalysisMaker.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StMessMgr.h"
#include "StAssociationMaker/StAssociationMaker.h"
#include "StAssociationMaker/StTrackPairInfo.hh"
#include "StThreeVectorF.hh"
#include "StEventTypes.h"
#include "StMcEventTypes.hh"
#include "StEvent.h"
#include "StTpcHit.h"
#include "StMcEvent.hh"
#include "StMcTpcHit.hh"
#include "TpcCluster.h"
// StDb
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StCoordinates.hh" 
#include "StTpcDb/StTpcDb.h"
#include "StTpcRawData.h"
#include "StDetectorDbMaker/St_tpcPadPlanesC.h"
ClassImp(StTpcMcAnalysisMaker)

//_________________________________________________
Int_t StTpcMcAnalysisMaker::Init() {
  TFile *f = GetTFile();
  assert(f);
  f->cd();
  if (  m_Mode) {gMessMgr->Info() << "Multi  Cluster mode" << endm;}
  else          {gMessMgr->Info() << "Single Cluster mode" << endm;}
  fCluster = new TpcCluster();
  mTpcT = new TTree("TpcT","the TPC hit pairs and pixel Info");
  Int_t bufsize= 64000;
  Int_t split = 99;
  if (split)  bufsize /= 4;
  mTpcT->Branch("TpcCluster", "TpcCluster",&fCluster, bufsize, split);
  return StMaker::Init();
}
//_________________________________________________
Int_t StTpcMcAnalysisMaker::Make() {
  if (m_Mode) {
    return MultiCluster();
  } else {
    return SingleCluster();
  }
}
//_________________________________________________
Int_t StTpcMcAnalysisMaker::MultiCluster() {
  StEvent*   rEvent      = (StEvent*)    GetInputDS("StEvent");
  StMcEvent* mEvent      = (StMcEvent *) GetInputDS("StMcEvent");
  TDataSet*  tpcRawEvent =               GetInputDS("Event");
  StTpcRawData *tpcRawData = 0;
  if (tpcRawEvent) tpcRawData = (StTpcRawData *) tpcRawEvent->GetObject();
  StAssociationMaker* assoc = (StAssociationMaker*) GetMaker("StAssociationMaker");
  rcTpcHitMapType* theHitMap   = 0;
  if (assoc)       theHitMap   = assoc->rcTpcHitMap();
  if (!theHitMap) 
    gMessMgr->Warning() << "----------WARNING----------\n"
			<< "No Hit Map found for this event!" << endm;
  
  static StTpcCoordinateTransform transform(gStTpcDb);
  static StGlobalCoordinate                gCoord; 
  static StGlobalDirection                 dirG;
  static StTpcLocalSectorCoordinate        local, local1, local2, localMc;
  static StTpcLocalDirection               dirL;      
  static StTpcLocalSectorAlignedCoordinate lsCoordA, localMcA,localA;
  static StTpcLocalSectorAlignedDirection  dirLSA;
  static StTpcPadCoordinate                PadOnPlane;      
  static StVectPixel                       Pixels;
  StTpcHitCollection* rcHits = rEvent->tpcHitCollection();
  if (! rcHits) {
    gMessMgr->Warning() << "No TPC Hits found for this event!" << endm;
    return kStWarn;
  }
  StMcTpcHitCollection* mcHits = 0;
  if (mEvent)  mcHits = mEvent->tpcHitCollection();
  StSPtrVecTrackNode& trackNode = rEvent->trackNodes();
  UInt_t nTracks = trackNode.size();
  StPrimaryVertex *primVtx = rEvent->primaryVertex();
  UInt_t NoTracks = 0;
  if (primVtx) NoTracks = primVtx->numberOfDaughters();
  StTrackNode *node=0;
  
  for (UInt_t sector = 1; sector <= 24; sector++) {
    for (UInt_t row = 1; row <= 45; row++) {
      fCluster->Clear();
      fCluster->SetEventNo(GetEventNumber());
      fCluster->SetDriftVelocities(gStTpcDb->DriftVelocity(1),gStTpcDb->DriftVelocity(13));
      fCluster->SetFrequency(gStTpcDb->Electronics()->samplingFrequency());
      fCluster->SetNofPV(rEvent->numberOfPrimaryVertices());
      fCluster->SetNoTracksAtBestPV(NoTracks);
      fCluster->SetSecRow(sector,row);
      if (primVtx)
	fCluster->SetXyzPV(primVtx->position().x(),primVtx->position().y(),primVtx->position().z());
      Int_t nPixels = 0;
      if (tpcRawData) nPixels = tpcRawData->getVecOfPixels(Pixels,sector,row);
      for (Int_t k = 0; k < nPixels; k++) fCluster->AddPixel(&Pixels[k]);
      if (mcHits) {
	StMcTpcSectorHitCollection* tpcSectHitColl = mcHits->sector(sector-1);
	if (! tpcSectHitColl) continue;
	StMcTpcPadrowHitCollection* tpcPadRowHitColl = tpcSectHitColl->padrow(row-1);
	if (! tpcPadRowHitColl) continue;
	for (UInt_t iHit=0; iHit<tpcPadRowHitColl->hits().size(); iHit++){
	  const StMcTpcHit *mcHit = dynamic_cast<const StMcTpcHit *> (tpcPadRowHitColl->hits()[iHit]);
	  assert (mcHit);
	  if (mcHit->isDet()) continue;
	  fCluster->AddMcHit(mcHit);
	}
      }
      if (rcHits) {
	StTpcSectorHitCollection* tpcSectHitColl = rcHits->sector(sector-1);
	if (! tpcSectHitColl) continue;
	StTpcPadrowHitCollection* tpcPadRowHitColl = tpcSectHitColl->padrow(row-1);
	if (! tpcPadRowHitColl) continue;
	for (UInt_t iHit=0; iHit<tpcPadRowHitColl->hits().size(); iHit++){
	  const StTpcHit *rcHit = dynamic_cast<const StTpcHit *> (tpcPadRowHitColl->hits()[iHit]);
	  assert (rcHit);
	  fCluster->AddRcHit(rcHit);
	}
      }
      for (UInt_t i=0; i < nTracks; i++) {
	node = trackNode[i]; if (!node) continue;
	StGlobalTrack* gTrack = static_cast<StGlobalTrack*>(node->track(global));
	if (! gTrack ||  gTrack->flag() <= 0) continue;
	Int_t ifPrim = 0;
	StPrimaryTrack* pTrack = static_cast<StPrimaryTrack*>(node->track(primary));
	if (NoTracks >= 10 && pTrack &&  pTrack->flag() > 0) {
	  if (pTrack->vertex() == primVtx) ifPrim = 1;
	}
	StPtrVecHit hvec = gTrack->detectorInfo()->hits(kTpcId);
	if (! hvec.size()) continue;
	Int_t npoints = gTrack->detectorInfo()->numberOfPoints(kTpcId);
	Int_t nfitpoints = gTrack->fitTraits().numberOfFitPoints(kTpcId);
	//    if (npoints < 20) continue;
	StDedxPidTraits *pid = 0;
	StDedxPidTraits *pid70 = 0;
	StSPtrVecTrackPidTraits &traits = gTrack->pidTraits();
	Double_t TrackLength70 = 0, I70 = 0;
	for (UInt_t i = 0; i < traits.size(); i++) {
	  if (! traits[i]) continue;
	  if ( traits[i]->IsZombie()) continue;
	  pid = dynamic_cast<StDedxPidTraits*>(traits[i]);
	  if (pid) {
	    if (pid->method() == kTruncatedMeanId) {
	      pid70 = pid; I70 = pid70->mean(); 
	      TrackLength70 = pid70->length(); 
	    }
	  }
	}	  
	for (UInt_t j=0; j<hvec.size(); j++) {// hit loop
	  const StTpcHit *rHit = dynamic_cast<StTpcHit *> (hvec[j]);
	  if (! rHit) continue;
	  if (! rHit->usedInFit()) continue;
	  if (sector != rHit->sector()) continue;
	  if (row    != rHit->padrow()) continue;
	  StTpcLocalSectorDirection  dirLS(0.,1.,0.,sector,row);
	  transform(dirLS,dirLSA);
	  transform(dirLSA,dirL);
	  transform(dirL,dirG);
	  StThreeVectorD normal(dirG.position().x(),dirG.position().y(),dirG.position().z());
	  Double_t y = transform.yFromRow(row);
	  StTpcLocalSectorCoordinate  lsCoord(0., y, 10.,sector,row);
	  transform(lsCoord,lsCoordA);
	  transform(lsCoordA, gCoord);
	  StThreeVectorD middle(gCoord.position().x(),gCoord.position().y(),gCoord.position().z());
	  //      RawHelper.SetSector(sector);
	  Double_t s = gTrack->geometry()->helix().pathLength(middle, normal);
	  if (s > 1.e4) continue;
	  StThreeVectorD xyzOnPlane = gTrack->geometry()->helix().at(s);
	  StGlobalCoordinate globalOnPlane(xyzOnPlane.x(),xyzOnPlane.y(),xyzOnPlane.z());
	  transform(globalOnPlane,localA,sector,row);
	  transform(localA,local);
	  transform(local,PadOnPlane);
	  Float_t xyz[3] = {(Float_t) xyzOnPlane.x(),(Float_t) xyzOnPlane.y(),(Float_t) xyzOnPlane.z()};
	  Float_t BField[3];
	  StMagF::Agufld(xyz,BField);
	  //      StGlobalDirection pxyz(gTrack->geometry()->momentum());
	  dirG = StGlobalDirection(gTrack->geometry()->helix().momentumAt(s,BField[2]*kilogauss));
	  transform(dirG,dirL,sector,row);
	  transform(dirL,dirLSA);
	  transform(dirLSA,dirLS);
	  // Track prediction 
	  TpcTrack track(PadOnPlane.sector(), PadOnPlane.row(), 
			 PadOnPlane.pad(), PadOnPlane.timeBucket(), 
			 npoints, nfitpoints, ifPrim, 
			 dirLS.position().x(),  dirLS.position().y(), dirLS.position().z(), 
			 I70, TrackLength70);
	  fCluster->AddRcTrack(&track);
	  fCluster->AddRcTHit(rHit);
	}
      }
      mTpcT->Fill();
    }
  }
  return kStOK;
}
//_________________________________________________
Int_t StTpcMcAnalysisMaker::SingleCluster() {
  StEvent*   rEvent      = (StEvent*)    GetInputDS("StEvent");
  StMcEvent* mEvent      = (StMcEvent *) GetInputDS("StMcEvent");
  TDataSet*  tpcRawEvent =               GetInputDS("Event");
  StTpcRawData *tpcRawData = 0;
  if (tpcRawEvent) tpcRawData = (StTpcRawData *) tpcRawEvent->GetObject();
  StAssociationMaker* assoc = (StAssociationMaker*) GetMaker("StAssociationMaker");
  rcTpcHitMapType* theHitMap   = 0;
  if (assoc)       theHitMap   = assoc->rcTpcHitMap();
  if (!theHitMap) 
    gMessMgr->Warning() << "----------WARNING----------\n"
			<< "No Hit Map found for this event!" << endm;
  
  static StTpcCoordinateTransform transform(gStTpcDb);
  static StGlobalCoordinate                gCoord; 
  static StGlobalDirection                 dirG;
  static StTpcLocalSectorCoordinate        local, local1, local2, localMc;
  static StTpcLocalDirection               dirL;      
  static StTpcLocalSectorAlignedCoordinate lsCoordA, localMcA,localA;
  static StTpcLocalSectorAlignedDirection  dirLSA;
  static StTpcPadCoordinate                PadOnPlane;      
  static StVectPixel                       Pixels;
  StTpcHitCollection* rcHits = rEvent->tpcHitCollection();
  if (! rcHits) {
    gMessMgr->Warning() << "No TPC Hits found for this event!" << endm;
    return kStWarn;
  }
  StMcTpcHitCollection* mcHits = 0;
  if (mEvent)  mcHits = mEvent->tpcHitCollection();
  StSPtrVecTrackNode& trackNode = rEvent->trackNodes();
  UInt_t nTracks = trackNode.size();
  StPrimaryVertex *primVtx = rEvent->primaryVertex();
  UInt_t NoTracks = 0;
  if (primVtx) NoTracks = primVtx->numberOfDaughters();
  StTrackNode *node=0;
  for (UInt_t i=0; i < nTracks; i++) {
    node = trackNode[i]; if (!node) continue;
    StGlobalTrack* gTrack = static_cast<StGlobalTrack*>(node->track(global));
    if (! gTrack ||  gTrack->flag() <= 0) continue;
    Int_t ifPrim = 0;
    StPrimaryTrack* pTrack = static_cast<StPrimaryTrack*>(node->track(primary));
    if (NoTracks >= 10 && pTrack &&  pTrack->flag() > 0) {
      if (pTrack->vertex() == primVtx) ifPrim = 1;
    }
    StPtrVecHit hvec = gTrack->detectorInfo()->hits(kTpcId);
    if (! hvec.size()) continue;
    Int_t npoints = gTrack->detectorInfo()->numberOfPoints(kTpcId);
    Int_t nfitpoints = gTrack->fitTraits().numberOfFitPoints(kTpcId);
    //    if (npoints < 20) continue;
    StDedxPidTraits *pid = 0;
    StDedxPidTraits *pid70 = 0;
    StSPtrVecTrackPidTraits &traits = gTrack->pidTraits();
    Double_t TrackLength70 = 0, I70 = 0;
    for (UInt_t i = 0; i < traits.size(); i++) {
      if (! traits[i]) continue;
      if ( traits[i]->IsZombie()) continue;
      pid = dynamic_cast<StDedxPidTraits*>(traits[i]);
      if (pid) {
	if (pid->method() == kTruncatedMeanId) {
	  pid70 = pid; I70 = pid70->mean(); 
	  TrackLength70 = pid70->length(); 
	}
      }
    }	  
    for (UInt_t j=0; j<hvec.size(); j++) {// hit loop
      const StTpcHit *rHit = dynamic_cast<StTpcHit *> (hvec[j]);
      if (! rHit) continue;
      if (! rHit->usedInFit() || rHit->flag()) continue;
      Int_t sector = rHit->sector();
      Int_t row    = rHit->padrow();
      StTpcLocalSectorDirection  dirLS(0.,1.,0.,sector,row);
      transform(dirLS,dirLSA);
      transform(dirLSA,dirL);
      transform(dirL,dirG);
      StThreeVectorD normal(dirG.position().x(),dirG.position().y(),dirG.position().z());
      Double_t y = transform.yFromRow(row);
      StTpcLocalSectorCoordinate  lsCoord(0., y, 10.,sector,row);
      transform(lsCoord,lsCoordA);
      transform(lsCoordA, gCoord);
      StThreeVectorD middle(gCoord.position().x(),gCoord.position().y(),gCoord.position().z());
      //      RawHelper.SetSector(sector);
      Double_t s = gTrack->geometry()->helix().pathLength(middle, normal);
      if (s > 1.e4) continue;
      StThreeVectorD xyzOnPlane = gTrack->geometry()->helix().at(s);
      StGlobalCoordinate globalOnPlane(xyzOnPlane.x(),xyzOnPlane.y(),xyzOnPlane.z());
      transform(globalOnPlane,localA,sector,row);
      transform(localA,local);
      transform(local,PadOnPlane);
      Float_t xyz[3] = {(Float_t) xyzOnPlane.x(),(Float_t) xyzOnPlane.y(),(Float_t) xyzOnPlane.z()};
      Float_t BField[3];
      StMagF::Agufld(xyz,BField);
      //      StGlobalDirection pxyz(gTrack->geometry()->momentum());
      dirG = StGlobalDirection(gTrack->geometry()->helix().momentumAt(s,BField[2]*kilogauss));
      transform(dirG,dirL,sector,row);
      transform(dirL,dirLSA);
      transform(dirLSA,dirLS);
      // Track prediction 
      TpcTrack track(PadOnPlane.sector(), PadOnPlane.row(), 
		     PadOnPlane.pad(), PadOnPlane.timeBucket(), 
		     npoints, nfitpoints, ifPrim, 
		     dirLS.position().x(),  dirLS.position().y(), dirLS.position().z(), 
		     I70, TrackLength70);
      //	if (rHit->flag()) continue;
      fCluster->Clear();
      fCluster->SetEventNo(GetEventNumber());
      fCluster->AddRcTrack(&track);
      fCluster->AddRcHit(rHit);
      fCluster->SetDriftVelocities(gStTpcDb->DriftVelocity(1),gStTpcDb->DriftVelocity(13));
      fCluster->SetFrequency(gStTpcDb->Electronics()->samplingFrequency());
      fCluster->SetNofPV(rEvent->numberOfPrimaryVertices());
      fCluster->SetNoTracksAtBestPV(NoTracks);
      if (primVtx)
	fCluster->SetXyzPV(primVtx->position().x(),primVtx->position().y(),primVtx->position().z());
      if (tpcRawData) {
	Int_t kPadMin = rHit->minPad();
	Int_t kPadMax = rHit->maxPad();
	Int_t kTbMin  = rHit->minTmbk();
	Int_t kTbMax  = rHit->maxTmbk();
	if (kTbMax - rHit->timeBucket() >= 15) kTbMax += 10;
	Int_t nPixels = 0;
	if (tpcRawData) nPixels = tpcRawData->getVecOfPixels(Pixels,sector,row, kPadMin, kPadMax, kTbMin, kTbMax);
	if (! nPixels) continue;
	Int_t AdcSum = 0;
	Int_t Id = rHit->idTruth();
	for (Int_t k = 0; k < nPixels; k++) {
	  if (Pixels[k].idTruth() == Id) {
	    fCluster->AddPixel(&Pixels[k]);
	    AdcSum += Pixels[k].adc();
	  }
	}
	if (Debug() > 1) cout << "\taccepted\t" <<  AdcSum << "\t" 
			      << kPadMin << "/" << kPadMax << " "
			      << (Int_t) rHit->minPad() << "/" << (Int_t) rHit->maxPad()
			      << "\t" << kTbMin << "/" << kTbMax << " "
			      << rHit->minTmbk() << "/" << rHit->maxTmbk()
			      << endl;
	fCluster->SetAdcSum(AdcSum);
	TClonesArray *pixels = fCluster->Pixels();
	Int_t N = pixels->GetEntriesFast();
	if (N > 0) { // add projections 
	  Double_t *SumOverPadsD = new Double_t[kTbMax-kTbMin+1];
	  memset (SumOverPadsD, 0, (kTbMax-kTbMin+1)*sizeof(Double_t));
	  Double_t *SumOverTBinD = new Double_t[kPadMax-kPadMin+1];
	  memset (SumOverTBinD, 0, (kPadMax-kPadMin+1)*sizeof(Double_t));
	  for (Int_t k = 0; k < N; k++) {
	    StTpcPixel *S = (StTpcPixel *) pixels->At(k);// cout << *S << endl;
	    Int_t pad = S->pad();
	    Int_t tbin = S->timebin();
	    SumOverPadsD[tbin-kTbMin] += S->adc();
	    SumOverTBinD[pad-kPadMin] += S->adc();
	  }
	  for (Int_t pad = kPadMin; pad <= kPadMax; pad++) {
	    if (SumOverTBinD[pad-kPadMin] > 0) {
	      StTpcPixel pixelT(1,sector,row,pad,1024,(UShort_t) SumOverTBinD[pad-kPadMin],Id);
	      fCluster->AddPixel(&pixelT);
	    }
	  }
	  delete [] SumOverTBinD;
	  for (Int_t tbin = kTbMin; tbin <= kTbMax; tbin++) {
	    if (SumOverPadsD[tbin-kTbMin] > 0) {
	      StTpcPixel pixelT(1,sector,row,255,tbin,(UShort_t) SumOverPadsD[tbin-kTbMin],Id);
	      fCluster->AddPixel(&pixelT);
	    }
	  }
	  delete [] SumOverPadsD;
	}
      }
      if (theHitMap) {
	if (rHit->TestBit(StMcHit::kMatched)) {
	  pair<rcTpcHitMapIter,rcTpcHitMapIter>
	    recBounds = theHitMap->equal_range(rHit);
	  for (rcTpcHitMapIter it2=recBounds.first; it2!=recBounds.second; ++it2){
	    const StMcTpcHit *mHit = dynamic_cast<const StMcTpcHit *> ((*it2).second);
	    assert ( mHit);
	    if (mHit->isDet()) continue;
	    fCluster->AddMcHit(mHit);
	  }
	}
      }
      mTpcT->Fill();
    }
  }
#if 1
  // non matched hits
  if (rcHits) {
    UInt_t numberOfSectors = rcHits->numberOfSectors();
    for (UInt_t i = 0; i< numberOfSectors; i++) {
      StTpcSectorHitCollection* sectorCollection = rcHits->sector(i);
      if (sectorCollection) {
	Int_t numberOfPadrows = sectorCollection->numberOfPadrows();
	for (int j = 0; j< numberOfPadrows; j++) {
	  StTpcPadrowHitCollection *rowCollection = sectorCollection->padrow(j);
	  if (rowCollection) {
	    StSPtrVecTpcHit &hits = rowCollection->hits();
	    UInt_t NoHits = hits.size();
	    if (NoHits) {
	      for (UInt_t k = 0; k < NoHits; k++) {
		StTpcHit *rHit = static_cast<StTpcHit *> (hits[k]);
		if (! rHit) continue;
		if ( rHit->usedInFit() ) continue;
		Int_t sector = rHit->sector();
		Int_t row    = rHit->padrow();
		StTpcLocalSectorDirection  dirLS(0.,1.,0.,sector,row);
		transform(dirLS,dirLSA);
		transform(dirLSA,dirL);
		transform(dirL,dirG);
		StThreeVectorD normal(dirG.position().x(),dirG.position().y(),dirG.position().z());
		Double_t y = transform.yFromRow(row);
		StTpcLocalSectorCoordinate  lsCoord(0., y, 10.,sector,row);
		transform(lsCoord,lsCoordA);
		transform(lsCoordA, gCoord);
		transform(dirG,dirL,sector,row);
		transform(dirL,dirLSA);
		transform(dirLSA,dirLS);
		// Track prediction 
		fCluster->Clear();
		fCluster->SetEventNo(GetEventNumber());
		fCluster->AddRcHit(rHit);
		fCluster->SetDriftVelocities(gStTpcDb->DriftVelocity(1),gStTpcDb->DriftVelocity(13));
		fCluster->SetFrequency(gStTpcDb->Electronics()->samplingFrequency());
		fCluster->SetNofPV(rEvent->numberOfPrimaryVertices());
		fCluster->SetNoTracksAtBestPV(NoTracks);
		if (tpcRawData) {
		  Int_t kPadMin = rHit->minPad();
		  Int_t kPadMax = rHit->maxPad();
		  Int_t kTbMin  = rHit->minTmbk();
		  Int_t kTbMax  = rHit->maxTmbk();
		  if (kTbMax - rHit->timeBucket() >= 15) kTbMax += 10;
		  Int_t nPixels = 0;
		  if (tpcRawData) nPixels = tpcRawData->getVecOfPixels(Pixels,sector,row, kPadMin, kPadMax, kTbMin, kTbMax);
		  if (! nPixels) continue;
		  Int_t AdcSum = 0;
		  Int_t Id = rHit->idTruth();
		  for (Int_t k = 0; k < nPixels; k++) {
		    if (Pixels[k].idTruth() == Id) {
		      fCluster->AddPixel(&Pixels[k]);
		      AdcSum += Pixels[k].adc();
		    }
		  }
		  if (Debug() > 1) cout << "\taccepted\t" <<  AdcSum << "\t" 
					<< kPadMin << "/" << kPadMax << " "
					<< (Int_t) rHit->minPad() << "/" << (Int_t) rHit->maxPad()
					<< "\t" << kTbMin << "/" << kTbMax << " "
					<< rHit->minTmbk() << "/" << rHit->maxTmbk()
					<< endl;
		  fCluster->SetAdcSum(AdcSum);
		  TClonesArray *pixels = fCluster->Pixels();
		  Int_t N = pixels->GetEntriesFast();
		  if (N > 0) { // add projections 
		    Double_t *SumOverPadsD = new Double_t[kTbMax-kTbMin+1];
		    memset (SumOverPadsD, 0, (kTbMax-kTbMin+1)*sizeof(Double_t));
		    Double_t *SumOverTBinD = new Double_t[kPadMax-kPadMin+1];
		    memset (SumOverTBinD, 0, (kPadMax-kPadMin+1)*sizeof(Double_t));
		    for (Int_t k = 0; k < N; k++) {
		      StTpcPixel *S = (StTpcPixel *) pixels->At(k);// cout << *S << endl;
		      Int_t pad = S->pad();
		      Int_t tbin = S->timebin();
		      SumOverPadsD[tbin-kTbMin] += S->adc();
		      SumOverTBinD[pad-kPadMin] += S->adc();
		    }
		    for (Int_t pad = kPadMin; pad <= kPadMax; pad++) {
		      if (SumOverTBinD[pad-kPadMin] > 0) {
			StTpcPixel pixelT(1,sector,row,pad,1024,(UShort_t) SumOverTBinD[pad-kPadMin],Id);
			fCluster->AddPixel(&pixelT);
		      }
		    }
		    delete [] SumOverTBinD;
		    for (Int_t tbin = kTbMin; tbin <= kTbMax; tbin++) {
		      if (SumOverPadsD[tbin-kTbMin] > 0) {
			StTpcPixel pixelT(1,sector,row,255,tbin,(UShort_t) SumOverPadsD[tbin-kTbMin],Id);
			fCluster->AddPixel(&pixelT);
		      }
		    }
		    delete [] SumOverPadsD;
		  }
		}
		if (theHitMap) {
		  if (rHit->TestBit(StMcHit::kMatched)) {
		    pair<rcTpcHitMapIter,rcTpcHitMapIter>
		      recBounds = theHitMap->equal_range(rHit);
		    for (rcTpcHitMapIter it2=recBounds.first; it2!=recBounds.second; ++it2){
		      const StMcTpcHit *mHit = dynamic_cast<const StMcTpcHit *> ((*it2).second);
		      assert ( mHit);
		      if (mHit->isDet()) continue;
		      fCluster->AddMcHit(mHit);
		    }
		  }
		}
		mTpcT->Fill();
	      }
	    }
	  }
	}
      }
    }
  }
  if (mcHits) {
    for (Int_t sector=0;
	 sector<(Int_t) mcHits->numberOfSectors(); sector++) {
      if (Debug()) {cout << sector + 1 << " "; flush(cout);}
      StMcTpcSectorHitCollection* tpcSectHitColl = mcHits->sector(sector);
      for (Int_t row=0;
	   row<(Int_t) tpcSectHitColl->numberOfPadrows();
	   row++) {
	StMcTpcPadrowHitCollection* tpcPadRowHitColl = tpcSectHitColl->padrow(row);
	for (Int_t iHit=0;
	     iHit<(Int_t) tpcPadRowHitColl->hits().size();
	     iHit++){
	  const StMcTpcHit *mHit = dynamic_cast<const StMcTpcHit *> (tpcPadRowHitColl->hits()[iHit]);
	  assert (mHit);
	  //	  cout << "McTpcHit\t" << *mHit << endl;
	  if (mHit->isDet()) continue;
	  if (mHit->TestBit(StMcHit::kMatched)) continue;
	  fCluster->Clear();
	  fCluster->AddMcHit(mHit);
	  mTpcT->Fill();
	}
      }
    }
  }
#endif
  return kStOK;
}
