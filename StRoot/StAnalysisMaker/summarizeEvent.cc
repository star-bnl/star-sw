/***************************************************************************
 *
 * $Id: summarizeEvent.cc,v 2.9 2006/02/14 17:52:32 perev Exp $
 *
 * Author: Torre Wenaus, BNL,
 *         Thomas Ullrich, Nov 1999
 ***************************************************************************
 *
 * Description:  This is an example of a function which performs
 *               some simple analysis using StEvent.
 *               Use this as a template and customize it for your
 *               studies.
 *
 ***************************************************************************
 *
 * $Log: summarizeEvent.cc,v $
 * Revision 2.9  2006/02/14 17:52:32  perev
 * More detailed print. TPC separately
 *
 * Revision 2.8  2005/11/22 23:05:14  fisyak
 * Correct print out for no. of bad Ftpc hits
 *
 * Revision 2.7  2005/10/06 20:01:58  fisyak
 * add information about ftpc tracks, adjust hit.flag cut to >3
 *
 * Revision 2.6  2005/07/19 20:08:17  perev
 * MultiVertex corr
 *
 * Revision 2.5  2005/06/22 22:19:37  fisyak
 * Add protection for absence svtHitCollection
 *
 * Revision 2.4  2005/06/22 16:09:01  fisyak
 * Add summary of quality
 *
 * Revision 2.3  2000/07/12 05:24:39  ullrich
 * Minor updates to cope with revised StAnalysisMaker.
 *
 * Revision 2.2  2000/01/05 16:06:27  ullrich
 * Added SSD hits.
 *
 * Revision 2.1  1999/11/16 12:28:44  ullrich
 * Corrected typo and added print-out of number of primary tracks.
 *
 * Revision 2.0  1999/11/04 16:10:11  ullrich
 * Revision for new StEvent
 *
 **************************************************************************/
#include "StContainers.h"
#include "StEventTypes.h"
#include "StMessMgr.h"

static const char rcsid[] = "$Id: summarizeEvent.cc,v 2.9 2006/02/14 17:52:32 perev Exp $";

void
summarizeEvent(StEvent& event, const int &nevents)
{
  static const UInt_t NoFitPointCutForGoodTrack = 15;
    gMessMgr->QAInfo() << "StAnalysisMaker,  Reading Event: " << nevents
		       << "  Type: " << event.type()
		       << "  Run: " << event.runId() << endm;
    
    StSPtrVecTrackNode& trackNode = event.trackNodes();
    UInt_t nTracks = trackNode.size();
    StTrackNode *node = 0;
    UInt_t nGoodTracks = 0;
    UInt_t nTpcTracks = 0;
    UInt_t nGoodTpcTracks = 0;
    UInt_t nGoodFtpcTracks = 0;
    for (unsigned int i=0; i < nTracks; i++) {
      node = trackNode[i]; if (!node) continue;
      StGlobalTrack* gTrack = static_cast<StGlobalTrack*>(node->track(global));
      if (! gTrack) continue;
      if (gTrack->flag() >= 700) nGoodFtpcTracks++;
      if (gTrack->fitTraits().numberOfFitPoints() <  NoFitPointCutForGoodTrack) continue;
      nGoodTracks++;
    }
    gMessMgr->QAInfo() << "# track nodes:   \t"
		       <<  nTracks << ":\tglobals with NFitP>="<< NoFitPointCutForGoodTrack << ":\t" << nGoodTracks 
		       << ":\tFtpc tracks :\t" << nGoodFtpcTracks << endm;
    StPrimaryVertex *pVertex=0;
    for (int ipr=0;(pVertex=event.primaryVertex(ipr));ipr++) {
      StThreeVectorD primPos = pVertex->position();
      unsigned int nDaughters = pVertex->numberOfDaughters();
      nGoodTracks = 0;nGoodTpcTracks = 0;nTpcTracks = 0;
      for (unsigned int i=0; i < nDaughters; i++) {
        StPrimaryTrack* pTrack = (StPrimaryTrack*)pVertex->daughter(i);
        int good = (pTrack->fitTraits().numberOfFitPoints() >=  NoFitPointCutForGoodTrack);
        nGoodTracks+=good;
	if (pTrack->fitTraits().numberOfFitPoints(kTpcId)) {
          nTpcTracks++; nGoodTpcTracks+=good;
        } 
      }
      gMessMgr->QAInfo() << "# primary vertex("<<ipr<<"): \t"<<primPos<<endm;

      gMessMgr->QAInfo() << "# primary tracks:\t"
		         << nDaughters << ":\tones    with NFitP(>="<< NoFitPointCutForGoodTrack << "):\t" << nGoodTracks << endm;
      gMessMgr->QAInfo() << "# primary TPC tracks:\t"
		         << nTpcTracks << ":\tones    with NFitP(>="<< NoFitPointCutForGoodTrack << "):\t" << nGoodTpcTracks << endm;
    }// end prim vtx    
    
    gMessMgr->QAInfo() << "# V0 vertices:       "
		       << event.v0Vertices().size() << endm;

    gMessMgr->QAInfo() << "# Xi vertices:       "
		       << event.xiVertices().size() << endm;
    
    gMessMgr->QAInfo() << "# Kink vertices:       "
		       << event.kinkVertices().size() << endm;

    UInt_t TotalNoOfTpcHits = 0, noBadTpcHits = 0, noTpcHitsUsedInFit = 0;
    StTpcHitCollection* TpcHitCollection = event.tpcHitCollection();
    if (TpcHitCollection) {
      UInt_t numberOfSectors = TpcHitCollection->numberOfSectors();
      for (UInt_t i = 0; i< numberOfSectors; i++) {
	StTpcSectorHitCollection* sectorCollection = TpcHitCollection->sector(i);
	if (sectorCollection) {
	  Int_t numberOfPadrows = sectorCollection->numberOfPadrows();
	  for (int j = 0; j< numberOfPadrows; j++) {
	    StTpcPadrowHitCollection *rowCollection = sectorCollection->padrow(j);
	    if (rowCollection) {
	      StSPtrVecTpcHit &hits = rowCollection->hits();
	      UInt_t NoHits = hits.size();
	      for (UInt_t k = 0; k < NoHits; k++) {
		StTpcHit *tpcHit = static_cast<StTpcHit *> (hits[k]);
		if (tpcHit) {
		  TotalNoOfTpcHits++;
		  if ( tpcHit->flag()) noBadTpcHits++;
		  if (tpcHit->usedInFit()) noTpcHitsUsedInFit++;
		}
	      }
	    }
	  }
	}
      }
    }
    gMessMgr->QAInfo() << "# TPC hits:          " << TotalNoOfTpcHits 
		       << ":\tdeconvoluted:     " << noBadTpcHits 
		       << ":\tUsed in Fit:      " << noTpcHitsUsedInFit << endm;
    
    UInt_t TotalNoOfSvtHits = 0, noBadSvtHits = 0, noSvtHitsUsedInFit = 0;
    StSvtHitCollection* svthits = event.svtHitCollection();
    if (svthits) {
      StSvtHit* hit;
      for (unsigned int barrel=0; barrel<svthits->numberOfBarrels(); ++barrel) {
	StSvtBarrelHitCollection* barrelhits = svthits->barrel(barrel);
	if (!barrelhits) continue;
	for (unsigned int ladder=0; ladder<barrelhits->numberOfLadders(); ++ladder) {
	  StSvtLadderHitCollection* ladderhits = barrelhits->ladder(ladder);
	  if (!ladderhits) continue;
	  for (unsigned int wafer=0; wafer<ladderhits->numberOfWafers(); ++wafer) {
	    StSvtWaferHitCollection* waferhits = ladderhits->wafer(wafer);
	    if (!waferhits) continue;
	    const StSPtrVecSvtHit& hits = waferhits->hits();
	    for (const_StSvtHitIterator it=hits.begin(); it!=hits.end(); ++it) {
	      hit = static_cast<StSvtHit*>(*it);
	      if (!hit) continue;
	      TotalNoOfSvtHits++;
	      if (hit->flag() >3) noBadSvtHits++;
	      if (hit->usedInFit()) noSvtHitsUsedInFit++;
	    }
	  }
	}
      }
    }
    gMessMgr->QAInfo() << "# SVT hits:          " << TotalNoOfSvtHits 
		       << ":\tBad ones(flag >3): " << noBadSvtHits 
		       << ":\tUsed in Fit:      " << noSvtHitsUsedInFit << endm;
    
    UInt_t TotalNoOfSsdHits = 0, noBadSsdHits = 0, noSsdHitsUsedInFit = 0;
    StSsdHitCollection* ssdhits = event.ssdHitCollection();
    if (ssdhits) {
      StSsdHit* hit;
      for (unsigned int ladder=0; ladder<ssdhits->numberOfLadders(); ++ladder) {
	StSsdLadderHitCollection* ladderhits = ssdhits->ladder(ladder);
	if (!ladderhits) continue;
	for (unsigned int wafer=0; wafer<ladderhits->numberOfWafers(); ++wafer) {
	  StSsdWaferHitCollection* waferhits = ladderhits->wafer(wafer);
	  if (!waferhits) continue;
	  const StSPtrVecSsdHit& hits = waferhits->hits();
	  for (const_StSsdHitIterator it=hits.begin(); it!=hits.end(); ++it) {
	    hit = static_cast<StSsdHit*>(*it);
	    if (!hit) continue;
	    TotalNoOfSsdHits++;
	    if (hit->flag() >3) noBadSsdHits++;
	    if (hit->usedInFit()) noSsdHitsUsedInFit++;
	  }
	}
      }
    }
    gMessMgr->QAInfo() << "# SSD hits:          " << TotalNoOfSsdHits 
		       << ":\tBad ones(flag>3): " << noBadSsdHits 
		       << ":\tUsed in Fit:      " << noSsdHitsUsedInFit << endm;
    
    UInt_t TotalNoOfFtpcHits = 0, noBadFtpcHits = 0, noFtpcHitsUsedInFit = 0;
    StFtpcHitCollection* ftpchits = event.ftpcHitCollection();
    if (ftpchits) {
      StFtpcHit* hit;
      for (unsigned int plane=0; plane<ftpchits->numberOfPlanes(); ++plane) {
	StFtpcPlaneHitCollection* planehits = ftpchits->plane(plane);
	if (!planehits) continue;
	for (unsigned int sector=0; sector<planehits->numberOfSectors(); ++sector) {
	  StFtpcSectorHitCollection* sectorhits = planehits->sector(sector);
	  if (!sectorhits) continue;
	  const StSPtrVecFtpcHit& hits = sectorhits->hits();
	  for (const_StFtpcHitIterator it=hits.begin(); it!=hits.end(); ++it) {
	    hit = static_cast<StFtpcHit*>(*it);
	    if (!hit) continue;
	    TotalNoOfFtpcHits++;
	    if (hit->flag() && (1 << 7)) noBadFtpcHits++;
	    if (hit->usedInFit()) noFtpcHitsUsedInFit++;
	  }
	}
      }
    }
    gMessMgr->QAInfo() << "# FTPC hits:         " << TotalNoOfFtpcHits 
		       << ":\tBad ones(bit7): " << noBadFtpcHits 
		       << ":\tUsed in Fit:      " << noFtpcHitsUsedInFit << endm;
    
}
