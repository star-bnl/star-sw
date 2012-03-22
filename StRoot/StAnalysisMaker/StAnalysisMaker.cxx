//
//  This is a STAR typical comment header. You should modify
//  it to reflect your changes.
//  As a minimum it should contain the name of the author, the
//  date it was written/modified, and a short description of what
//  the class is meant to do. The cvs strings $X$ (where X=Id, Log)
//  are not needed when you do not intend to put the file under
//  cvs control. Remove them.
//  
/*!
 * \class  StAnalysisMaker
 * \brief  A typical Analysis Class
 * \author Torre Wenaus, BNL, Thomas Ullrich
 * \date   Nov 1999
 *
 *
 * This is an example of a maker to perform analysis using StEvent.
 * Use this as a template and customize it for your studies.
 *
 * $Id: StAnalysisMaker.cxx,v 2.16 2012/03/22 23:45:16 fisyak Exp $
 *
 */


//
//  Include header files. What has to be included strongly depends
//  on your implementation. StEventTypes.h contains all includes
//  you need to use StEvent.
//
#include "StAnalysisMaker.h"
#include "StEventTypes.h"
#include "StMessMgr.h"
#include "StDcaGeometry.h"
#if ROOT_VERSION_CODE < 334081
#include "TArrayL.h"
#else
#include "TArrayL64.h"
#endif
#include "TClassTable.h"
#include "TNtuple.h"
//
//  The following line defines a static string. Currently it contains
//  the cvs Id. The compiler will put the string (literally) in the
//  object file. It thus ends up in the shared library.
//  The UNIX command 'strings' allowsto print all printable character in
//  a non-text file. This way one can check the version of the file
//  contained in a given shared library. If you do not intend to put
//  the file under cvs control (likely) you can remove the line.

//
//  Proptotypes of little functions which perform
//  specific analysis tasks. You'll find them
//  in the same directory as StAnalysisMaker.cxx.
//  You most likely will not need them but they can serve
//  as an example for your own functions.
//
void summarizeEvent(StEvent&, const int&);
long countPrimaryTracks(StEvent&);
long countPrimaryPions(StEvent&);


//
//  This is needed to make your maker work in root4star.
//  It can be place anywhere in the file. Note that this
//  is a macro, that's why the ';' is missing.
//
ClassImp(StAnalysisMaker)


/// The constructor. Initialize you data members here.
StAnalysisMaker::StAnalysisMaker(const Char_t *name) : StMaker(name)
{
    mEventCounter = 0;
}
Int_t StAnalysisMaker::Finish() {
    //
    //  A good place for printout and to summarize
    //  the run.
    //
    gMessMgr->Info() << "StAnalysisMaker::Finish() "
		     << "Processed " << mEventCounter << " events." << endm;
    
    return kStOK;
}

/*!
 *  This method is called every event. That's the
 *  right place to plug in your analysis. 
 */
Int_t StAnalysisMaker::Make() {
    mEventCounter++;  // increase counter
	
    //
    //	Get pointer to StEvent
    //
    StEvent* event;
    event = (StEvent *) GetInputDS("StEvent");
    if (!event){
      gMessMgr->Warning() << "StAnalysisMaker::Make : No StEvent" << endm;
      return kStOK;        // if no event, we're done
    }

    //
    //  The following is only needed since the
    //  QA folks use this maker for their QA runs.
    //  You do not need this.
    //  
    summarizeEvent(*event, mEventCounter); 

    //
    //  See if this event survives the event filter.
    //  If not we stop here right away.
    //
    if (!accept(event)){
      gMessMgr->Warning() << "StAnalysisMaker::Make : Event was not accepted" << endm;
      return kStOK;
    }
    return kStOK;
}

bool StAnalysisMaker::accept(StEvent* event)
{
    //
    //  This is a kind of very simple event filter.
    //  We select only events with a valid event vertex,
    //  i.e. event->primaryVertex() returns a non-zero pointer.
    // 
    return event->primaryVertex();
}

bool StAnalysisMaker::accept(StTrack* track)
{
    //
    //  This is a kind of very simple track filter.
    //  We only check for positive flags.
    //  Note that this method works for global and
    //  primary tracks since we deal with the base
    //  class only (StTrack).
    //
    return track && track->flag() >= 0;
}
//________________________________________________________________________________
void StAnalysisMaker::PrintStEvent(Int_t k, Int_t minFitPts) {
  static const Char_t *trackType[] = {"global", "primary", "tpt", "secondary", "estGlobal", "estPrimary"};
  StEvent* pEvent = (StEvent*) StMaker::GetChain()->GetInputDS("StEvent");
  if (!pEvent) return;
  cout << "Event: Run "<< pEvent->runId() << " Event No: " << pEvent->id() << endl;
  cout << "Vertex Positions" << endl;
  UInt_t NpVX = pEvent->numberOfPrimaryVertices();
  if (NpVX ) {
    for (UInt_t i = 0; i < NpVX; i++) {
      const StPrimaryVertex *vx = pEvent->primaryVertex(i);
      const StThreeVectorF &position = vx->position();
      cout << Form("Vertex: %i Position: %8.3f %8.3f %8.3f type: %3i Fl: %3i IdT: %4i Q: %4i",
		   i,position.x(),position.y(),position.z(),
		   vx->type(),vx->flag(), vx->idTruth(), vx->qaTruth()) << endl;
    }
  } else {
    cout << "Event: Vertex Not Found" << endl;
  }
  
  StSPtrVecTrackNode& trackNode = pEvent->trackNodes();
  UInt_t nTracks = trackNode.size();
  StTrackNode *node = 0;
  Int_t line = 0;
  for (UInt_t  i=0; i < nTracks; i++) {
    node = trackNode[i]; if (!node) continue;
    StGlobalTrack* gTrack = static_cast<StGlobalTrack*>(node->track(global));
    if (! gTrack->detectorInfo()) {cout << "Missing detectorInfo for track " << i << " ==========" << endl;}
    const StDcaGeometry* dca    = gTrack->dcaGeometry();
    StPrimaryTrack *pTrack = 	static_cast<StPrimaryTrack*>(node->track(primary));
    for (int l = 0; l < 2; l++) {
      StTrack        *track = 0;
      if (k%10 > 0 && k%10 != l+1) continue;
      if (l == global)  track = gTrack;
      if (l == primary) track = pTrack;
      if (track) {
	if (minFitPts > 0 && track->fitTraits().numberOfFitPoints() < minFitPts) continue;
	if (dca && l == global) {
	  if (! line) {
	    cout << "track# type   flag" << endl;
	    line++;
	  }
	  cout << *dca;
	  Double_t length = track->length();
	  if (length > 9999.) length = 9999.;
	  cout << Form(" L %8.3f", length);
	  cout << Form(" NF %4d chi2 %8.3f NP %4d Id: %4i Q: %4i",
		       track->fitTraits().numberOfFitPoints(), track->fitTraits().chi2(), track->numberOfPossiblePoints(),
		       track->idTruth(), track->qaTruth());
	} else 	{
	  //                      1234567890123456781234567812345678 12345678 12345678 12345678   123 12345678 12345678 123 12345678 
	  if (! line) {
	    cout << "track# type   flag       z     mom     pT     eta     phi  c      pX      pY      pZ  "
		 << "Max  length     dca  NFP    chi2 NP BEMC" << endl; //FhitXYZ" << endl;
	    line++;
	  }
	  Short_t charge = track->geometry()->charge();
	  StThreeVectorD g3 = track->geometry()->momentum(); // p of global track
	  cout << Form("%4d%10s%4d%8.3f%8.3f%8.3f%8.3f%8.2f",
		       i,trackType[l],track->flag(),track->geometry()->origin().z(),
		       g3.mag(),g3.perp(),g3.pseudoRapidity(),180/TMath::Pi()*g3.phi());
	  cout << Form(" %2d%8.3f%8.3f%8.3f",charge,g3.x(),g3.y(),g3.z());
	  Double_t length = track->length();
	  if (length > 9999.) length = 9999.;
	  cout << Form(" %4d%8.3f%8.3f", track->numberOfPossiblePoints(),length,track->impactParameter());
	  cout << Form(" %4d%8.3f%4d",track->fitTraits().numberOfFitPoints(), track->fitTraits().chi2(), track->detectorInfo()->numberOfPoints());
	  if (track->vertex())
	    cout << Form(" B%3i",((StPrimaryVertex *)track->vertex())->numMatchesWithBEMC());
	}
#if 0
      cout << " Svt p/h/f" << track->numberOfPossiblePoints(kSvtId) 
	   << "/" << track->detectorInfo()->hits(kSvtId).size()
	   << "/" << track->fitTraits().numberOfFitPoints(kSvtId);
#endif
      cout << endl;
      }
    } // l
    //    if (i > 5) break;
  }  
}
//________________________________________________________________________________
void StAnalysisMaker::PrintTpcHits(Int_t sector, Int_t row, Bool_t plot, Int_t IdTruth) {
  struct BPoint_t {
    Float_t sector, row, x, y, z, q;
  };
    
  StEvent* pEvent = (StEvent*) StMaker::GetChain()->GetInputDS("StEvent");
  if (!pEvent) { cout << "Can't find StEvent" << endl; return;}
  //  StSPtrVecTrackNode& trackNode = pEvent->trackNodes();
  Int_t TotalNoOfTpcHits = 0;
  StTpcHitCollection* TpcHitCollection = pEvent->tpcHitCollection();
  if (! TpcHitCollection) { cout << "No TPC Hit Collection" << endl; return;}
  UInt_t numberOfSectors = TpcHitCollection->numberOfSectors();
  for (UInt_t i = 0; i< numberOfSectors; i++) {
    if (sector == 0 || (Int_t) i+1 == sector) {
      StTpcSectorHitCollection* sectorCollection = TpcHitCollection->sector(i);
      if (sectorCollection) {
	Int_t numberOfPadrows = sectorCollection->numberOfPadrows();
	//	Int_t noHits = 0;
	for (int j = 0; j< numberOfPadrows; j++) {
	  if (row == 0 || j+1 == row) {
	    StTpcPadrowHitCollection *rowCollection = sectorCollection->padrow(j);
	    if (rowCollection) {
	      StSPtrVecTpcHit &hits = rowCollection->hits();
#if ROOT_VERSION_CODE < 334081
	      Long_t NoHits = hits.size();
	      TArrayL idxT(NoHits); Long_t *idx = idxT.GetArray();
#else
	      Long64_t NoHits = hits.size();
	      TArrayL64 idxT(NoHits); Long64_t *idx = idxT.GetArray();
#endif
	      TotalNoOfTpcHits += NoHits;
	      TArrayD dT(NoHits);   Double_t *d = dT.GetArray();
	      for (Long64_t k = 0; k < NoHits; k++) {
		StTpcHit *tpcHit = static_cast<StTpcHit *> (hits[k]);
		const StThreeVectorF& xyz = tpcHit->position();
		d[k] = xyz.z();
	      }
	      TMath::Sort(NoHits,d,idx,kFALSE);
	      for (Long64_t k = 0; k < NoHits; k++) {
		Int_t l = idx[k];
		StTpcHit *tpcHit = static_cast<StTpcHit *> (hits[l]);
		if (! tpcHit) continue;
		if (IdTruth >= 0 && tpcHit->idTruth() != IdTruth) continue;
		tpcHit->Print();
	      }
	    }
	  }
	}
      }
    }
    //    break;
  }
  cout << "TotalNoOfTpcHits = " << TotalNoOfTpcHits << endl;
}
//________________________________________________________________________________
void StAnalysisMaker::PrintSvtHits() {
  UInt_t i,j,k,l;
  //  Double_t zPrim = 0;
  StEvent* pEvent = (StEvent*) StMaker::GetChain()->GetInputDS("StEvent");
  if (!pEvent) return;
  //  if (pEvent->numberOfPrimaryVertices() != 1) return;
  StPrimaryVertex *primaryVertex = pEvent->primaryVertex();
  if ( primaryVertex) {
    const StThreeVectorF &primXYZ = primaryVertex->position();
    //  cout << "primaryVertex " << primXYZ << endl;
    cout << "primaryVertex \t" << primXYZ.x() << "\t" << primXYZ.y() << "\t" << primXYZ.z() << endl;
  }
  Int_t TotalNoOfSvtHits = 0;
  StSvtHitCollection* SvtHitCollection = pEvent->svtHitCollection();
  if (! SvtHitCollection) { cout << "No SVT Hit Collection" << endl; return;}
  UInt_t numberOfBarrels = SvtHitCollection->numberOfBarrels();
  //  Int_t vers = gClassTable->GetID("StSvtHit");
  for ( i = 0; i< numberOfBarrels; i++) {
    StSvtBarrelHitCollection* barrelCollection = SvtHitCollection->barrel(i);
    if (barrelCollection) {
      UInt_t numberOfLadders = barrelCollection->numberOfLadders();
      //      UInt_t noHits = 0;
      for (j = 0; j< numberOfLadders; j++) {
	StSvtLadderHitCollection *ladderCollection = barrelCollection->ladder(j);
	if (ladderCollection) {
	  UInt_t numberOfWafers = ladderCollection->numberOfWafers();
	  for (k = 0; k < numberOfWafers; k++) {
	    StSvtWaferHitCollection* waferCollection = ladderCollection->wafer(k);
	    StSPtrVecSvtHit &hits = waferCollection->hits();
	    UInt_t NoHits = hits.size();
	    for (l = 0; l < NoHits; l++) {
	      StSvtHit *hit = hits[l];
	      if (hit) {
		//		cout << *((StHit *) hit) << endl;
		TotalNoOfSvtHits++;
		hit->Print();
	      }
	    }
	  }
	}
      }
    }
  }
  cout << "Total no. of Svt Hits " <<   TotalNoOfSvtHits << endl;
}
//________________________________________________________________________________
void StAnalysisMaker::PrintSsdHits() {
  UInt_t i,k,l;
  //  Double_t zPrim = 0;
  StEvent* pEvent = (StEvent*) StMaker::GetChain()->GetInputDS("StEvent");
  if (!pEvent) return;
  //  if (pEvent->numberOfPrimaryVertices() != 1) return;
  StPrimaryVertex *primaryVertex = pEvent->primaryVertex();
  if ( primaryVertex) {
    const StThreeVectorF &primXYZ = primaryVertex->position();
    //  cout << "primaryVertex " << primXYZ << endl;
    cout << "primaryVertex \t" << primXYZ.x() << "\t" << primXYZ.y() << "\t" << primXYZ.z() << endl;
  }
  //  Int_t TotalNoOfSsdHits = 0;
  StSsdHitCollection* SsdHitCollection = pEvent->ssdHitCollection();
  if (! SsdHitCollection) { cout << "No SSD Hit Collection" << endl; return;}
  UInt_t numberOfLadders = SsdHitCollection->numberOfLadders();
  //  Int_t vers = gClassTable->GetID("StSsdHit");
  for ( i = 0; i< numberOfLadders; i++) {
    StSsdLadderHitCollection* ladderCollection = SsdHitCollection->ladder(i);
    if (ladderCollection) {
      UInt_t numberOfWafers = ladderCollection->numberOfWafers();
      for (k = 0; k < numberOfWafers; k++) {
	StSsdWaferHitCollection* waferCollection = ladderCollection->wafer(k);
	StSPtrVecSsdHit &hits = waferCollection->hits();
	UInt_t NoHits = hits.size();
	for (l = 0; l < NoHits; l++) {
	  StSsdHit *hit = hits[l];
	  if (hit) {
	    hit->Print("");
	  }
	}
      }
    }
  }
}
//________________________________________________________________________________
void StAnalysisMaker::PrintRnDHits() {
  UInt_t i=0,k=0,l;
  //  Double_t zPrim = 0;
  StEvent* pEvent = (StEvent*) StMaker::GetChain()->GetInputDS("StEvent");
  if (!pEvent) return;
  //  if (pEvent->numberOfPrimaryVertices() != 1) return;
  StPrimaryVertex *primaryVertex = pEvent->primaryVertex();
  if ( primaryVertex) {
    const StThreeVectorF &primXYZ = primaryVertex->position();
    //  cout << "primaryVertex " << primXYZ << endl;
    cout << "primaryVertex \t" << primXYZ.x() << "\t" << primXYZ.y() << "\t" << primXYZ.z() << endl;
  }
  //  Int_t TotalNoOfRnDHits = 0;
  StRnDHitCollection* RnDHitCollection = pEvent->rndHitCollection();
  if (! RnDHitCollection) { cout << "No RND Hit Collection" << endl; return;}
  StSPtrVecRnDHit &hits = RnDHitCollection->hits();
  UInt_t NoHits = hits.size();
  for (l = 0; l < NoHits; l++) {
    StRnDHit *hit = hits[l];
    if (hit) {
      //		cout << *((StHit *) hit) << endl;
      const StThreeVectorF &P = hit->position();
      printf("l:%2i w:%2i",i+1,k+1);
      printf(" x: %8.3f y: %8.3f z: %8.3f ", P.x(), P.y(), P.z());
      printf("l:%2i w:%2i",
	     hit->ladder(), hit->wafer());
      printf(" Id: %4i Q: %4i",hit->idTruth(), hit->qaTruth());
      printf(" Flag: %4i Fit: %3i",hit->flag(), hit->usedInFit());
      printf("\n");
    }
  }
}
/* -------------------------------------------------------------------------
 * $Log: StAnalysisMaker.cxx,v $
 * Revision 2.16  2012/03/22 23:45:16  fisyak
 * Compress output for Event summary
 *
 * Revision 2.15  2010/09/01 14:33:57  fisyak
 * Clean ups
 *
 * Revision 2.14  2010/01/26 20:35:51  fisyak
 * use dca print out
 *
 * Revision 2.13  2009/11/23 15:54:28  fisyak
 * Clean-up est tracks
 *
 * Revision 2.12  2009/11/10 20:17:59  fisyak
 * Add print out for StEvent track and hits
 *
 * Revision 2.11  2009/11/03 15:13:22  fisyak
 * Comment print out, wait till StEvent will be mofidied
 *
 * Revision 2.10  2009/11/03 15:03:56  fisyak
 * Add static method to print StEvent
 *
 * Revision 2.9  2008/04/02 23:15:35  fisyak
 * Add protection against allGlobals == 0
 *
 * Revision 2.8  2004/02/04 01:36:40  jeromel
 * Minor change for user's education. Use of gMessMgr
 *
 * Revision 2.7  2004/02/01 18:01:53  jeromel
 * A few message addition
 *
 * Revision 2.6  2003/03/20 00:29:19  jeromel
 * Calling Wite() on 0x0 pointer
 *
 * Revision 2.5  2003/02/27 15:25:36  jeromel
 * Missing check on triggerIdCollection() now added
 *
 * Revision 2.4  2003/02/18 22:19:09  jeromel
 * Added dump of Y3 triggers
 *
 * Revision 2.3  2002/04/28 00:10:27  jeromel
 * doxygen basic dox added. GetCVS() had wrong signature : corrected to avoid
 * propagation of this typo in new makers.
 *
 * Revision 2.2  2000/07/12 05:23:28  ullrich
 * Updated for better use as template for actual analysis.
 *
 * Revision 2.1  1999/12/30 01:54:57  ogilvie
 * added countPrimaryPions as example how to use PID
 *
 * Revision 2.0  1999/11/04 16:10:03  ullrich
 * Revision for new StEvent
 *
 * -------------------------------------------------------------------------
 */

