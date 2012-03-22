/***************************************************************************
 *
 * $Id: summarizeEvent.cc,v 2.26 2012/03/22 23:45:16 fisyak Exp $
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
 * Revision 2.26  2012/03/22 23:45:16  fisyak
 * Compress output for Event summary
 *
 * Revision 2.25  2010/09/01 14:33:57  fisyak
 * Clean ups
 *
 * Revision 2.24  2009/11/23 15:54:28  fisyak
 * Clean-up est tracks
 *
 * Revision 2.23  2007/10/19 16:18:32  fine
 * new Db schema from TxCorp
 *
 * Revision 2.22  2007/10/18 14:54:29  fine
 * remove the extra coma
 *
 * Revision 2.21  2007/10/18 00:33:05  fine
 * Adjust job tracking messages fro the new tracking schema from TxCorp
 *
 * Revision 2.20  2007/10/12 15:48:52  ullrich
 * Removed use of obsolete detector enums.
 *
 * Revision 2.19  2007/03/28 20:58:02  fisyak
 * Fix print out for RnD hits
 *
 * Revision 2.18  2007/03/27 22:17:58  fisyak
 * Add {} to handle LOG_QA
 *
 * Revision 2.17  2007/03/21 16:49:38  fisyak
 * Add print out for RnD hits
 *
 * Revision 2.16  2006/08/28 16:57:09  fisyak
 * Add print out for Beam Background tracks and short track pointing to EEMC
 *
 * Revision 2.15  2006/08/07 20:49:57  fisyak
 * Add Event Id, correct Ftpc bad hit
 *
 * Revision 2.14  2006/07/03 04:13:37  fine
 * new Job tracking Db activated
 *
 * Revision 2.13  2006/07/01 01:19:15  fine
 * Add new jiob tracking option code
 *
 * Revision 2.12  2006/05/12 18:08:14  fine
 * fix the MySQLAppender problem and re-shape the trakDb messages
 *
 * Revision 2.11  2006/05/10 17:05:35  fine
 * separate the node/track information
 *
 * Revision 2.10  2006/05/09 23:31:20  fine
 * Reshape the job tracking Db tables and add a few LOQ_QA message to record it with the Job tracking Db
 *
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
#include "TMath.h"
#include "TString.h"
static const char rcsid[] = "$Id: summarizeEvent.cc,v 2.26 2012/03/22 23:45:16 fisyak Exp $";

void
summarizeEvent(StEvent& event, const int &nevents)
{
  static const UInt_t NoFitPointCutForGoodTrack = 15;
  LOG_QA << "StAnalysisMaker,  Reading Event: " << nevents
	 << "  Type: " << event.type()
	 << "  Run: " << event.runId() 
	 << "  EventId: " << event.id() <<   endm;
  
  StSPtrVecTrackNode& trackNode = event.trackNodes();
  UInt_t nTracks = trackNode.size();
  StTrackNode *node = 0;
  UInt_t nGoodTracks = 0;
  UInt_t nTpcTracks = 0;
  UInt_t nGoodTpcTracks = 0;
  UInt_t nGoodFtpcTracks = 0;
  UInt_t nBeamBackTracks = 0;
  UInt_t nGoodBeamBackTracks = 0;
  UInt_t nShortTrackForEEmc = 0;
  UInt_t pcTracks = 0; // PostCrossingTrack 
  UInt_t promptTracks = 0; // tracks with prompt hits
  UInt_t crossMembrane = 0;
  for (UInt_t i=0; i < nTracks; i++) {
    node = trackNode[i]; if (!node) continue;
    StGlobalTrack* gTrack = static_cast<StGlobalTrack*>(node->track(global));
    if (! gTrack) continue;
    if (TMath::Abs(gTrack->flag())%100 == 11) nShortTrackForEEmc++;
    if (gTrack->flag()/100 == 9) {
      nBeamBackTracks++;
      if (! gTrack->bad()) nGoodBeamBackTracks++;
    }
    if (gTrack->flag() >= 700 && gTrack->flag() < 900) nGoodFtpcTracks++;
    if ((gTrack->flag()/  1000)%10) pcTracks++;
    if ((gTrack->flag()/ 10000)%10) promptTracks++;
    if ((gTrack->flag()/100000)%10) crossMembrane++;
    if (gTrack->fitTraits().numberOfFitPoints() <  NoFitPointCutForGoodTrack) continue;
    nGoodTracks++;
  }
  LOG_QA << "# track nodes:   \t"
	 <<  nTracks << ":\tgood globals with NFitP>="<< NoFitPointCutForGoodTrack << ":\t" << nGoodTracks 
	 << ":\tFtpc tracks :\t" << nGoodFtpcTracks << endm;
  if (nBeamBackTracks || nShortTrackForEEmc) {
    LOG_QA  << "BeamBack tracks:\t" << nBeamBackTracks << ":\tgood ones:\t" << nGoodBeamBackTracks
	    << ":\tShort tracks pointing to EEMC :\t" << nShortTrackForEEmc 
	    << ":\tPCT :" << pcTracks << ":\tPrompt:" << promptTracks << ":\tCrossMem :" << crossMembrane
	    << endm;
  }
  // Report for jobTracking Db        
#ifdef OLDTRACKING    
  if (nTracks) {
    LOG_QA << "Events=" << nevents 
	   << ",StepEventId='EventFinish'"
	   << ",StepContext=" << "'nodes all',"  << "MessageId='='"
	   << ",ProgrammMessage='" <<  nTracks 
	   << "'" << endm;
  }
  
  if (nGoodTracks) { 
    LOG_QA << "Events=" << nevents 
	   << ",StepEventId='EventFinish'"
	   << ",StepContext=" << "'nodes good',"  << "MessageId='='"
	   << ",ProgrammMessage='" << nGoodTracks 
	   << "'" << endm;
  }
#else
  if (nTracks) {
    //        LOG_QA << "SequenceValue=" << nevents 
    LOG_QA 
      << "StageID='3'"
      << ",MessageKey=" << "'nodes all'" 
      << ",MessageValue='" <<  nTracks 
      << "'" << endm;
  }
  
  if (nGoodTracks) { 
    //        LOG_QA << "SequenceValue=" << nevents 
    LOG_QA 
      << "StageID='3'"
      << ",MessageKey=" << "'nodes good'" 
      << ",MessageValue='" << nGoodTracks 
      << "'" << endm;
  }
#endif    
  
  StPrimaryVertex *pVertex=0;
  for (int ipr=0;(pVertex=event.primaryVertex(ipr));ipr++) {
    StThreeVectorD primPos = pVertex->position();
    UInt_t nDaughters = pVertex->numberOfDaughters();
    nGoodTracks = 0;nGoodTpcTracks = 0;nTpcTracks = 0;pcTracks = 0;promptTracks = 0;crossMembrane = 0;
    for (UInt_t i=0; i < nDaughters; i++) {
      StPrimaryTrack* pTrack = (StPrimaryTrack*)pVertex->daughter(i);
      int good = (pTrack->fitTraits().numberOfFitPoints() >=  NoFitPointCutForGoodTrack);
      nGoodTracks+=good;
      if ((pTrack->flag()/  1000)%10) pcTracks++;
      if ((pTrack->flag()/ 10000)%10) promptTracks++;
      if ((pTrack->flag()/100000)%10) crossMembrane++;
      
      if (pTrack->fitTraits().numberOfFitPoints(kTpcId)) {
	nTpcTracks++; nGoodTpcTracks+=good;
      } 
    }
    const Char_t *beam = " ";
    LOG_QA << Form("# primary vertex(%3i) %1s: pct/prt/xm %2i/%2i/%2i: %8.3f,%8.3f,%8.3f",
		   ipr,beam,pcTracks,promptTracks,crossMembrane,primPos.x(),primPos.y(),primPos.z())
	   << " Prob/Chi2: "     << Form("%6.4f/%8.3f",pVertex->probChiSquared(),pVertex->chiSquared())
	   << " Rank: "       << Form("%8.3f",pVertex->ranking())
	   << Form(" tracks: %4i, %4i", nDaughters,nGoodTracks);
    if (nTpcTracks != nDaughters || nGoodTpcTracks != nGoodTracks) {
      LOG_QA << Form(" TPC:%4i,%4i",nTpcTracks,nGoodTpcTracks);
    }
    LOG_QA << endm;
    // Report for jobTracking Db   (non-zero entry only)    
#ifdef OLDTRACKING       
    if (nDaughters) {
      LOG_QA << "Events=" << nevents
	     << ",StepEventId='EventFinish'"
	     << ",StepContext=" << "'primary all',"  << "MessageId='='"
	     << ",ProgrammMessage='" <<  nDaughters
	     << "'" << endm;
    }
    if (nGoodTracks) {
      LOG_QA << "Events=" << nevents
	     << ",StepEventId='EventFinish'"
	     << ",StepContext=" << "'primary good',"  << "MessageId='='"
	     << ",ProgrammMessage='" << nGoodTracks
	     << "'" << endm;
    }
#else
    if (nDaughters) {
      //            LOG_QA << "SequenceValue=" << nevents
      LOG_QA 
	<< "StageID='3'"
	<< ",MessageKey=" << "'primary all'"  
	<< ",MessageValue='" <<  nDaughters
	<< "'" << endm;
    }
    if (nGoodTracks) {
      //            LOG_QA << "SequenceValue=" << nevents
      LOG_QA 
	<< "StageID='3'"
	<< ",MessageKey=" << "'primary good'" 
	<< ",MessageValue='" << nGoodTracks
	<< "'" << endm;
    }
#endif     
  }// end prim vtx    
  if (event.v0Vertices()  .size()) {
    LOG_QA << "# V0 vertices:       "
	 << event.v0Vertices().size() << endm;
  }
  if (event.xiVertices()  .size()) {
    LOG_QA << "# Xi vertices:       "
	   << event.xiVertices().size() << endm;
  }
  if (event.kinkVertices().size()) {
    LOG_QA << "# Kink vertices:       "
	 << event.kinkVertices().size() << endm;
  }
#ifdef OLDTRACKING             
  // Report for jobTracking Db   (non-zero entry only)      
  if (event.v0Vertices()  .size()) {
    LOG_QA << "Events=" << nevents 
	   << ",StepEventId='EventFinish'"
	   << ",StepContext=" << "'V0Vertices', " << "MessageId='='," << "ProgrammMessage=" << event.v0Vertices()  .size() << endm;
  }
  if (event.xiVertices()  .size()) {
    LOG_QA << "Events=" << nevents 
	   << ",StepEventId='EventFinish'"
	   << ",StepContext=" << "'XiVertices', " << "MessageId='='," << "ProgrammMessage="<< event.xiVertices()  .size()  << endm;
  }
  
  if (event.kinkVertices().size()) {
    LOG_QA << "Events=" << nevents 
	   << ",StepEventId='EventFinish'"
	   << ",StepContext=" << "'KinkVertices'," << "MessageId='='," << "ProgrammMessage="<< event.kinkVertices().size() << endm;
  }
#else
  // Report for jobTracking Db   (non-zero entry only)      
  if (event.v0Vertices()  .size()) {
    //        LOG_QA << "SequenceValue=" << nevents 
    LOG_QA 
      << "StageID='3'"
      << ",MessageKey=" << "'V0Vertices', " << "MessageValue=" << event.v0Vertices()  .size() << endm;
  }
  if (event.xiVertices()  .size()) {
    //        LOG_QA << "SequenceValue=" << nevents 
    LOG_QA 
      << "StageID='3'"
      << ",MessageKey=" << "'XiVertices', " << "MessageValue="<< event.xiVertices()  .size()  << endm;
  }
  
  if (event.kinkVertices().size()) {
    //        LOG_QA << "SequenceValue=" << nevents 
    LOG_QA 
      << "StageID='3'"
      << ",MessageKey=" << "'KinkVertices'," << "MessageValue="<< event.kinkVertices().size() << endm;
  }
#endif    
  
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
  if (TotalNoOfTpcHits) {
    LOG_QA   << "# TPC hits:          " << TotalNoOfTpcHits 
	     << ":\tBad ones (! flag):     " << noBadTpcHits 
	     << ":\tUsed in Fit:      " << noTpcHitsUsedInFit << endm;
  }
  UInt_t TotalNoOfSvtHits = 0, noBadSvtHits = 0, noSvtHitsUsedInFit = 0;
  StSvtHitCollection* svthits = event.svtHitCollection();
  if (svthits) {
    StSvtHit* hit;
    for (UInt_t barrel=0; barrel<svthits->numberOfBarrels(); ++barrel) {
      StSvtBarrelHitCollection* barrelhits = svthits->barrel(barrel);
      if (!barrelhits) continue;
      for (UInt_t ladder=0; ladder<barrelhits->numberOfLadders(); ++ladder) {
	StSvtLadderHitCollection* ladderhits = barrelhits->ladder(ladder);
	if (!ladderhits) continue;
	for (UInt_t wafer=0; wafer<ladderhits->numberOfWafers(); ++wafer) {
	  StSvtWaferHitCollection* waferhits = ladderhits->wafer(wafer);
	  if (!waferhits) continue;
	  const StSPtrVecSvtHit& hits = waferhits->hits();
	  for (const_StSvtHitIterator it=hits.begin(); it!=hits.end(); ++it) {
	    hit = static_cast<StSvtHit*>(*it);
	    if (!hit) continue;
	    TotalNoOfSvtHits++;
	    if (hit->flag() >3)   noBadSvtHits++;
	    if (hit->usedInFit()) noSvtHitsUsedInFit++;
	  }
	}
      }
    }
  }
  if (TotalNoOfSvtHits) {
    LOG_QA << "# SVT hits:          " << TotalNoOfSvtHits 
	   << ":\tBad ones(flag >3): " << noBadSvtHits 
	   << ":\tUsed in Fit:      " << noSvtHitsUsedInFit << endm;
  }
  UInt_t TotalNoOfSsdHits = 0, noBadSsdHits = 0, noSsdHitsUsedInFit = 0;
  StSsdHitCollection* ssdhits = event.ssdHitCollection();
  if (ssdhits) {
    StSsdHit* hit;
    for (UInt_t ladder=0; ladder<ssdhits->numberOfLadders(); ++ladder) {
      StSsdLadderHitCollection* ladderhits = ssdhits->ladder(ladder);
      if (!ladderhits) continue;
      for (UInt_t wafer=0; wafer<ladderhits->numberOfWafers(); ++wafer) {
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
  if (TotalNoOfSsdHits) {
    LOG_QA << "# SSD hits:          " << TotalNoOfSsdHits 
	   << ":\tBad ones(flag>3): " << noBadSsdHits 
	   << ":\tUsed in Fit:      " << noSsdHitsUsedInFit << endm;
  }
  UInt_t TotalNoOfFtpcHits = 0, noBadFtpcHits = 0, noFtpcHitsUsedInFit = 0;
  StFtpcHitCollection* ftpchits = event.ftpcHitCollection();
  if (ftpchits) {
    StFtpcHit* hit;
    for (UInt_t plane=0; plane<ftpchits->numberOfPlanes(); ++plane) {
      StFtpcPlaneHitCollection* planehits = ftpchits->plane(plane);
      if (!planehits) continue;
      for (UInt_t sector=0; sector<planehits->numberOfSectors(); ++sector) {
	StFtpcSectorHitCollection* sectorhits = planehits->sector(sector);
	if (!sectorhits) continue;
	const StSPtrVecFtpcHit& hits = sectorhits->hits();
	for (const_StFtpcHitIterator it=hits.begin(); it!=hits.end(); ++it) {
	  hit = static_cast<StFtpcHit*>(*it);
	  if (!hit) continue;
	  TotalNoOfFtpcHits++;
	  /*
	    bit0:unfolded
	    bit1:unfold failed
	    bit2:saturated
	    bit3:bad shape
	    bit4:cut off
	    bit5:tracked
	    bit6:global coords
	    bit7:don't use for tracking
	    
	    I assume good hits have bit 0 and 5 (if included on a track) on
	    
	    Joern and Marcus - is this correct?
	    
	    Janet
	  */
	  if (! ( hit->flag() & 1 || hit->flag() & (1 << 5))) noBadFtpcHits++;
	  else if (hit->flag() & (1 << 5))  noFtpcHitsUsedInFit++;
	}
      }
    }
  }
  if (TotalNoOfFtpcHits) {
    LOG_QA << "# FTPC hits:         " << TotalNoOfFtpcHits 
	   << ":\tBad ones(!bit0): " << noBadFtpcHits 
	   << ":\tUsed in Fit:      " << noFtpcHitsUsedInFit << endm;
  }
  StRnDHitCollection* rndhits = event.rndHitCollection();
  if (rndhits) {
    StSPtrVecRnDHit&  hits = rndhits->hits();
    Int_t NoHits =  rndhits->numberOfHits();
    if (NoHits) {
      struct NoHits_t {
	StDetectorId  kId;
	const Char_t *Name;
	Int_t         TotalNoOfHits;
	Int_t         noBadHits;
	Int_t         noHitsUsedInFit;
      };
      const Int_t NHtypes = 4;
      NoHits_t Hits[7] = {
	{kPxlId, "Hft", 0, 0, 0},
	{kIstId, "Ist", 0, 0, 0},           
	{kFgtId, "Fgt", 0, 0, 0},           
	{kUnknownId,"UnKnown", 0, 0, 0}
      };           
      StRnDHit* hit;
      for (Int_t i = 0; i < NoHits; i++) {
	hit = hits[i];
	Int_t j = 0;
	for (j = 0; j < NHtypes-1; j++) if ( Hits[j].kId == hit->detector()) break;
	Hits[j].TotalNoOfHits++;
	if (hit->flag())  Hits[j].noBadHits++;
	if (hit->usedInFit()) Hits[j].noHitsUsedInFit++;
      }
      for (Int_t j = 0; j < NHtypes; j++) {
	if (Hits[j].TotalNoOfHits) {
	  LOG_QA << "# " << Hits[j].Name << " hits:         " << Hits[j].TotalNoOfHits
		 << ":\tBad ones: " << Hits[j].noBadHits
		 << ":\tUsed in Fit:      " << Hits[j].noHitsUsedInFit << endm;
	}
      }
    }
  }
}
