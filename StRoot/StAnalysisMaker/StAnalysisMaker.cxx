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
 * $Id: StAnalysisMaker.cxx,v 2.26 2020/01/28 15:05:10 genevb Exp $
 *
 */


//
//  Include header files. What has to be included strongly depends
//  on your implementation. StEventTypes.h contains all includes
//  you need to use StEvent.
//
//#define __TPC_LOCAL_COORDINATES__
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
#include "StThreeVectorF.hh"
#include "StDetectorName.h"
#ifdef __TPC_LOCAL_COORDINATES__
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StCoordinates.hh" 
#endif /* __TPC_LOCAL_COORDINATES__ */
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
//
//  This is needed to make your maker work in root4star.
//  It can be place anywhere in the file. Note that this
//  is a macro, that's why the ';' is missing.
//
ClassImp(StAnalysisMaker);

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
    summarizeEvent(event, mEventCounter); 

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
void StAnalysisMaker::PrintStEvent(TString opt) {
  // opt = vpgl3 => "v" print vertex, "p" and primary tracks, "g" print global tracks, "l3" global tracks
  StEvent* pEvent = (StEvent*) StMaker::GetChain()->GetInputDS("StEvent");
  if (!pEvent) return;
  cout << "Event: Run "<< pEvent->runId() << " Event No: " << pEvent->id() << endl;
  UInt_t NpVX = pEvent->numberOfPrimaryVertices();
  if (NpVX) {
    if (opt.Contains("v",TString::kIgnoreCase)) {
      for (UInt_t i = 0; i < NpVX; i++) {
	const StPrimaryVertex *vx = pEvent->primaryVertex(i);
	vx->Print(Form("Vertex: %3i ",i));
#ifdef StTrackMassFit_hh
	const StTrackMassFit *pf = vx->parent();
	if (pf) cout << *pf << endl;
#endif
	if (opt.Contains("p",TString::kIgnoreCase)) {
	  UInt_t nDaughters = vx->numberOfDaughters();
	  for (UInt_t j = 0; j < nDaughters; j++) {
	    const StTrack* track = vx->daughter(j);
	    if (! track) continue;
	    cout << *((const StPrimaryTrack *)track) << endl;
#ifdef StTrackMassFit_hh
	    const StVertex*  vxEnd =  track->endVertex();
	    if (vxEnd) cout << *vxEnd << endl;
#endif
	  }
#ifdef StTrackMassFit_hh
	  UInt_t nMassFits = vx->numberOfMassFits();
	  for (UInt_t j = 0; j < nMassFits; j++) {
	    const StTrackMassFit * track = vx->massFit(j);
	    if (! track) continue;
	    cout << *track << endl;
	  }
#endif
	}
      }
    }
  } else {
    cout << "Event: Vertex Not Found" << endl;
  }
  if (opt.Contains("g",TString::kIgnoreCase)) {
    StSPtrVecTrackNode& trackNode = pEvent->trackNodes();
    UInt_t nTracks = trackNode.size();
    StTrackNode *node = 0;
#ifndef StTrackMassFit_hh
    cout << " Global tracks " << nTracks << endl;
#else
    cout << nTracks << " Track nodes" << endl;
#endif
    for (UInt_t  i=0; i < nTracks; i++) {
      node = trackNode[i]; if (!node) continue;
#ifdef StTrackMassFit_hh
      cout << *node << endl;
#else
      UInt_t nentries = node->entries();
      for (UInt_t j = 0; j < nentries; j++) {
	StTrack *track = node->track(j);
	if (! track) continue;
	if (track->type() == global) {
	  StGlobalTrack* gTrack = (StGlobalTrack* ) track;
	  cout << *gTrack << endl;
	} else if (track->type() == primary) {
	  StPrimaryTrack* pTrack = (StPrimaryTrack* ) track;
	  cout << *pTrack << endl;
	}
      } 
#endif
    }
  }
  if (opt.Contains("l3",TString::kIgnoreCase)) {
    if (pEvent->l3Trigger()) {
      StSPtrVecTrackNode& trackNode = pEvent->l3Trigger()->trackNodes();
      UInt_t nTracks = trackNode.size();
      StTrackNode *node = 0;
      cout << " L3 global tracks " << nTracks << endl;
      for (UInt_t  i=0; i < nTracks; i++) {
	node = trackNode[i]; if (!node) continue;
	StGlobalTrack* gTrack = dynamic_cast<StGlobalTrack*>(node->track(global));
	if (gTrack) cout << *gTrack << endl;
      } 
    }
  }
}
//________________________________________________________________________________
void StAnalysisMaker::PrintGlobalTrack(Int_t itk) {
  StEvent* pEvent = (StEvent*) StMaker::GetChain()->GetInputDS("StEvent");
  if (!pEvent) return;
  cout << "Event: Run "<< pEvent->runId() << " Event No: " << pEvent->id() << endl;
  StSPtrVecTrackNode& trackNode = pEvent->trackNodes();
  UInt_t nTracks = trackNode.size();
  StTrackNode *node = 0;
  cout << " Global tracks " << endl;
  for (UInt_t i = 0; i < nTracks; i++) {
    node = trackNode[i]; if (!node) continue;
    StGlobalTrack* gTrack = static_cast<StGlobalTrack*>(node->track(global));
    if (itk != 0 && gTrack->key() != itk) continue;
    cout << *gTrack << endl;
    if (! gTrack->detectorInfo()) {cout << "=============== detectorInfo is missing" << endl; continue;}
    StPtrVecHit hvec = gTrack->detectorInfo()->hits();
    for (UInt_t j=0; j<hvec.size(); j++) {// hit loop
      if (hvec[j]->detector() == kTpcId) {
	StTpcHit *tpcHit = static_cast<StTpcHit *> (hvec[j]);
	if (! tpcHit) continue;
	cout << *tpcHit << endl;
      } else {
	cout << *hvec[j] << endl;
      }
    }
  } 
}
//________________________________________________________________________________
void StAnalysisMaker::PrintVertex(Int_t ivx) {
  // opt = vpg => "v" print vertex, "p" and primary tracks, "g" print global tracks 
  StEvent* pEvent = (StEvent*) StMaker::GetChain()->GetInputDS("StEvent");
  if (!pEvent) return;
  cout << "Event: Run "<< pEvent->runId() << " Event No: " << pEvent->id() << endl;
  UInt_t NpVX = pEvent->numberOfPrimaryVertices();
  if (NpVX) {
    for (Int_t i = 0; i < NpVX; i++) {
      if (ivx >= 0 && i != ivx) continue;
      const StPrimaryVertex *vx = pEvent->primaryVertex(i);
      vx->Print(Form("Vertex: %3i ",i));
      UInt_t nDaughters = vx->numberOfDaughters();
      for (UInt_t j = 0; j < nDaughters; j++) {
	StPrimaryTrack* pTrack = (StPrimaryTrack*) vx->daughter(j);
	if (! pTrack) continue;
	cout << *pTrack << endl;
	if (! pTrack->detectorInfo()) {cout << "=============== detectorInfo is missing" << endl; continue;}
	StPtrVecHit hvec = pTrack->detectorInfo()->hits();
	for (UInt_t j=0; j<hvec.size(); j++) {// hit loop
	  if (hvec[j]->detector() == kTpcId) {
	    StTpcHit *tpcHit = static_cast<StTpcHit *> (hvec[j]);
	    if (! tpcHit) continue;
	    cout << *tpcHit << endl;
	  } else {
	    cout << *hvec[j] << endl;
	  }
	}
      }
    }
  } else {
    cout << "Event: Vertex Not Found" << endl;
  }
}
//________________________________________________________________________________
void StAnalysisMaker::PrintTpcHits(Int_t sector, Int_t row, Int_t plot, Int_t IdTruth) {
  // plot = 1 => All hits;
  // plot = 2 => prompt hits only |z| > 190
  struct BPoint_t {
    Float_t                     sector,row,x,y,z,q,adc,pad,timebucket,IdTruth,xL,yL,zL;
  };
  static const Char_t *vname = "sector:row:x:y:z:q:adc:pad:timebucket:IdTruth"
#ifdef __TPC_LOCAL_COORDINATES__
    ":xL:yL:zL"
#endif /* __TPC_LOCAL_COORDINATES__ */
    ;
  BPoint_t BPoint;
  static TNtuple *Nt = 0;
  if (plot && Nt == 0) {
    TFile *tf =  StMaker::GetTopChain()->GetTFile();
    if (tf) {tf->cd(); Nt = new TNtuple("TpcHit","TpcHit",vname);}
  }
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
	      if (! NoHits) continue;
	      TotalNoOfTpcHits += NoHits;
#if 1
	      TArrayD dT(NoHits);   Double_t *d = dT.GetArray();
	      for (Long64_t k = 0; k < NoHits; k++) {
		const StTpcHit *tpcHit = static_cast<const StTpcHit *> (hits[k]);
#if 0
		const StThreeVectorF& xyz = tpcHit->position();
		d[k] = xyz.z();
#else
		d[k] = tpcHit->id();
#endif
	      }
	      idx[0] = 0;
	      if (NoHits > 1) TMath::Sort(NoHits,d,idx,kFALSE);
#endif
	      for (Long64_t k = 0; k < NoHits; k++) {
#if 1
		Int_t l = idx[k];
#else
		Int_t l = k;
#endif
		StTpcHit *tpcHit = static_cast<StTpcHit *> (hits[l]);
		if (! tpcHit) continue;
		if (IdTruth >= 0 && tpcHit->idTruth() != IdTruth) continue;
		if (! plot) 		tpcHit->Print();
		else {
		  if (Nt) {
		    const StThreeVectorF& xyz = tpcHit->position();
		    if (plot == 2 && TMath::Abs(xyz.z()) < 195.0) continue;
#ifdef __TPC_LOCAL_COORDINATES__
		    StTpcCoordinateTransform tran;
		    StGlobalCoordinate glob(xyz);
		    StTpcLocalCoordinate lTpc;
		    tran(glob,lTpc,i+1,j+1);
		    BPoint.xL = lTpc.position().x();
		    BPoint.yL = lTpc.position().y();
		    BPoint.zL = lTpc.position().z();
#endif /* __TPC_LOCAL_COORDINATES__ */
		    BPoint.sector = i+1;
		    BPoint.row = j+1;
		    BPoint.x = xyz.x();
		    BPoint.y = xyz.y();
		    BPoint.z = xyz.z();
		    BPoint.q = 1.e6*tpcHit->charge();
		    BPoint.adc = tpcHit->adc();
		    BPoint.pad = tpcHit->pad();
		    BPoint.timebucket = tpcHit->timeBucket();
		    BPoint.IdTruth =  tpcHit->idTruth();
		    Nt->Fill(&BPoint.sector);
		  }
		}
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
void StAnalysisMaker::PrintEmcHits(Int_t det, Int_t mod, const Option_t *opt) {
  TString Opt(opt);
  StEvent* pEvent = (StEvent*) StMaker::GetChain()->GetInputDS("StEvent");
  if (!pEvent) { cout << "Can't find StEvent" << endl; return;}
  StEmcCollection* emccol=(StEmcCollection*) pEvent->emcCollection();
  if (! emccol) { cout << "No Emc Hit Collection" << endl; return;}
  //cout <<"Filling hits and clusters \n";
  Int_t d1 = 0, d2 = 7;
  if (det >= 0 && det <= 7) {d1 = d2 = det;}
  for(Int_t d = d1; d <= d2; d++)  {  
    StDetectorId id = static_cast<StDetectorId>(d+kBarrelEmcTowerId);
    if (id != kBarrelEmcTowerId && id != kEndcapEmcTowerId &&
	! Opt.Contains("Pre",TString::kIgnoreCase)) continue;
    const StEmcDetector* detector=emccol->detector(id);
    if(detector) {                          
      Int_t maxMod = 121;
      if (d > 3) maxMod = 14;
      Int_t j1 = 1;
      if (mod > 0 and mod < maxMod) {j1 = maxMod = mod;}
      //cout <<"Filling hits for detetor "<<EmcDet<<endl;
      if (Opt.Contains("Adc",TString::kIgnoreCase)) {
	for(Int_t j = j1; j < maxMod; j++) {
	  const StEmcModule* module = detector->module(j);
	  if(module) {
	    const StSPtrVecEmcRawHit& rawHit=module->hits();
	    Int_t nhits = (Int_t) rawHit.size();
	    for(Int_t k = 0; k < nhits; k++) if (rawHit[k]->energy() > 0) cout << DetectorName(id) << "\t" << *rawHit[k] << endl;
	  }
	}
      }
      if (Opt.Contains("Clu",TString::kIgnoreCase)) {
	const StEmcClusterCollection *cl = detector->cluster();
	if (cl) {
	  Int_t NoCls = cl->numberOfClusters();
	  if (NoCls) {
	    const StSPtrVecEmcCluster&       clusters = cl->clusters();
	    for (Int_t i = 0; i < NoCls; i++) {
	      if (clusters[i]->energy() > 0) cout << DetectorName(id) << "\t" << *clusters[i] << endl;
	    }
	  }
	}
      }
    }
  }
  if (Opt.Contains("Point",TString::kIgnoreCase)) {
    const StSPtrVecEmcPoint& bp = emccol->barrelPoints();
    const StSPtrVecEmcPoint& ep = emccol->endcapPoints();
    for (Int_t i = 0; i < 2; i++) {// barrel & endcap
      const StSPtrVecEmcPoint& p = (i == 0) ? bp : ep;
      Int_t np = (Int_t) p.size();
      if (np) {
	cout << "Found " << np << " Points in ";
	if (! i) cout << "Barrel";
	else     cout << "Encap";
	cout << endl;
	for (Int_t j = 0; j < np; j++) {
	  cout << *p[j] << endl;
	}
      }
    } 
  } 
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
void StAnalysisMaker::PrintSstHits() {
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
  //  Int_t TotalNoOfSstHits = 0;
  StSstHitCollection* SstHitCollection = pEvent->sstHitCollection();
  if (! SstHitCollection) { cout << "No SST Hit Collection" << endl; return;}
  UInt_t numberOfLadders = SstHitCollection->numberOfLadders();
  //  Int_t vers = gClassTable->GetID("StSstHit");
  for ( i = 0; i< numberOfLadders; i++) {
    StSstLadderHitCollection* ladderCollection = SstHitCollection->ladder(i);
    if (ladderCollection) {
      UInt_t numberOfWafers = ladderCollection->numberOfWafers();
      for (k = 0; k < numberOfWafers; k++) {
	StSstWaferHitCollection* waferCollection = ladderCollection->wafer(k);
	StSPtrVecSstHit &hits = waferCollection->hits();
	UInt_t NoHits = hits.size();
	for (l = 0; l < NoHits; l++) {
	  StSstHit *hit = hits[l];
	  if (hit) {
	    hit->Print("");
	  }
	}
      }
    }
  }
}
//________________________________________________________________________________
void StAnalysisMaker::PrintToFHits() {
  //  Double_t zPrim = 0;
  StEvent* pEvent = (StEvent*) StMaker::GetChain()->GetInputDS("StEvent");
  if (!pEvent) return;
  const StBTofCollection* tof = pEvent->btofCollection();
  if (! tof) {LOG_QA  << "No BToF collection" << endm; return;}
  else       {LOG_QA  << "BToF collection";}
  if (tof->tofHeader() && tof->tofHeader()->vpdVz() > -250) {
    LOG_QA  << " VpdZ:" << Form("%7.2f",tof->tofHeader()->vpdVz());
  }
  LOG_QA << endm;
  const StSPtrVecBTofHit& tofHits = tof->tofHits();
  for(size_t i=0;i<tofHits.size();i++) { //loop on hits in modules
    StBTofHit *aHit = tofHits[i];
    if(!aHit) continue;
    LOG_QA  << *aHit << endm;
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
//________________________________________________________________________________
void StAnalysisMaker::summarizeEvent(StEvent *event, Int_t mEventCounter) {
  if (! event) event = (StEvent*) StMaker::GetChain()->GetInputDS("StEvent");
  static const UInt_t NoFitPointCutForGoodTrack = StVertex::NoFitPointCutForGoodTrack();
  LOG_QA << "+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-" << endm
	 << "StAnalysisMaker,  Reading Event: " << mEventCounter
	 << "  Type: " << event->type()
	 << "  Run: " << event->runId() 
	 << "  EventId: " << event->id();
  const StTriggerIdCollection* triggerCol = event->triggerIdCollection();
  if (triggerCol) {
    const StTriggerId* nominal = triggerCol->nominal();
    if (nominal) {
      UInt_t maxTriggers = nominal->maxTriggerIds();
      LOG_QA << " TriggerIds: ";
      for (UInt_t i = 0; i < maxTriggers; i++) {
	if (nominal->triggerId(i)) {LOG_QA << nominal->triggerId(i) << "|";}
      }
    }
  }
      LOG_QA << endm;
  StSPtrVecTrackNode& trackNode = event->trackNodes();
  UInt_t nTracks = trackNode.size();
  StTrackNode *node = 0;
  UInt_t nGoodTracks = 0;
  UInt_t nGoodFtpcTracks = 0;
  UInt_t nBeamBackTracks = 0;
  UInt_t nGoodBeamBackTracks = 0;
  UInt_t nShortTrackForEEmc = 0;
  UInt_t nShortTrackForETOF = 0;
  UInt_t pcTracks = 0; // PostCrossingTrack 
  UInt_t promptTracks = 0; // tracks with prompt hits
  UInt_t crossMembrane = 0;
  UInt_t nToFMatched   = 0;
  UInt_t nBEmcMatched   = 0;
  UInt_t nEEmcMatched   = 0;
  UInt_t nWestTpcOnly = 0;
  UInt_t nEastTpcOnly = 0;
  StGlobalTrack* gTrack = 0;
  for (UInt_t i=0; i < nTracks; i++) {
    node = trackNode[i]; if (!node) continue;
    gTrack = static_cast<StGlobalTrack*>(node->track(global));
    if (! gTrack) continue;
    if (gTrack->flag() < 0) continue;
    if (TMath::Abs(gTrack->flag())%100 == 11) nShortTrackForEEmc++;
    if (TMath::Abs(gTrack->flag())%100 == 12) nShortTrackForETOF++;
    if (gTrack->flag()/100 == 9) {
      nBeamBackTracks++;
      if (! gTrack->bad()) nGoodBeamBackTracks++;
    }
    if (gTrack->flag() >= 700 && gTrack->flag() < 900) nGoodFtpcTracks++;
    if (gTrack->isPostXTrack())                 pcTracks++;
    if (gTrack->isPromptTrack())                       promptTracks++;
    if (gTrack->isMembraneCrossingTrack())             crossMembrane++;
    if (gTrack->isToFMatched())                        nToFMatched++;
    if (gTrack->isBemcMatched())                       nBEmcMatched++;
    if (gTrack->isEemcMatched() )                      nEEmcMatched++;
    if (gTrack->fitTraits().numberOfFitPoints() <  NoFitPointCutForGoodTrack) continue;
    if (gTrack->isWestTpcOnly())                        nWestTpcOnly++;
    if (gTrack->isEastTpcOnly())                        nEastTpcOnly++;
    nGoodTracks++;
  }
  LOG_QA << "# track nodes:   \t"
	 <<  nTracks << ": good globals with NFitP>="<< NoFitPointCutForGoodTrack << ": " << nGoodTracks;
  if (nGoodFtpcTracks) {LOG_QA << ": Ftpc tracks : " << nGoodFtpcTracks;}
  LOG_QA << endm;
  if (nBeamBackTracks)    {LOG_QA  << "BeamBack tracks: " << nBeamBackTracks << ": good ones: " << nGoodBeamBackTracks;}
  if (nShortTrackForEEmc) {LOG_QA << ": Short tracks pointing to EEMC : " << nShortTrackForEEmc;}
  if (nShortTrackForETOF) {LOG_QA << ": Short tracks pointing to ETOF : " << nShortTrackForETOF;}
  if (nBeamBackTracks || nShortTrackForEEmc || nShortTrackForETOF) {LOG_QA << endm;}
  LOG_QA  << "post (C)rossing tracks :" << pcTracks << ": (P)rompt:" << promptTracks 
	  << ": (X) membrane :" << crossMembrane
	  << "(T)of/ctb matches:" << nToFMatched << " :Emc matches(B/E): " << nBEmcMatched << "/" << nEEmcMatched
	  << " :Only W:" << nWestTpcOnly << " E:" << nEastTpcOnly;
  if (event->btofCollection()) {
    if (event->btofCollection()->tofHeader() && event->btofCollection()->tofHeader()->vpdVz() > -250){
      LOG_QA  << " VpdZ:" << Form("%7.2f",event->btofCollection()->tofHeader()->vpdVz());
    }
  }
  if (event->triggerData()) {
    LOG_QA  << ": ZdcZ:" << Form("%7.2f",event->triggerData()->zdcVertexZ());
  }
  LOG_QA  << endm;
  // Report for jobTracking Db        
  if (nTracks) {
    //        LOG_QA << "SequenceValue=" << mEventCounter 
    LOG_QA 
      << "StageID='3'"
      << ",MessageKey=" << "'nodes all'" 
      << ",MessageValue='" <<  nTracks 
      << "'" << endm;
  }
  
  if (nGoodTracks) { 
    //        LOG_QA << "SequenceValue=" << mEventCounter 
    LOG_QA 
      << "StageID='3'"
      << ",MessageKey=" << "'nodes good'" 
      << ",MessageValue='" << nGoodTracks 
      << "'" << endm;
  }
  
  StPrimaryVertex *pVertex=0;
  Int_t NoVertexPos = 0; // no. of vertices with positive rank
  for (Int_t ipr=0;(pVertex=event->primaryVertex(ipr));ipr++) {
    if (pVertex->ranking() > 0) NoVertexPos++;
  }
  LOG_QA << "StageID='3'" << ",MessageKey=" << "'No. of Vertices with positive rank'" << ",MessageValue='" << NoVertexPos << "'" << endm;
  for (Int_t ipr=0;(pVertex=event->primaryVertex(ipr));ipr++) {
#ifdef StTrackMassFit_hh
    Int_t key = pVertex->key();
    if (key <= 0)  pVertex->setKey(ipr);
    LOG_QA << *pVertex << endm;
#else
    LOG_QA << Form("#V[%3i]",ipr) << *pVertex << endm;
#endif
    // Report for jobTracking Db   (non-zero entry only)    
    if (pVertex->numberOfDaughters()) {
      //            LOG_QA << "SequenceValue=" << mEventCounter
      LOG_QA 
	<< "StageID='3'"
	<< ",MessageKey=" << "'primary all'"  
	<< ",MessageValue='" <<  pVertex->numberOfDaughters()
	<< "'" << endm;
    }
    if (pVertex->numberOfGoodTracks()) {
      //            LOG_QA << "SequenceValue=" << mEventCounter
      LOG_QA 
	<< "StageID='3'"
	<< ",MessageKey=" << "'primary good'" 
	<< ",MessageValue='" << pVertex->numberOfGoodTracks()
	<< "'" << endm;
    }
  }// end prim vtx    
  if (event->v0Vertices()  .size()) {
    LOG_QA << "# V0 vertices:       "	 << event->v0Vertices().size() << endm;
    StSPtrVecV0Vertex& v0Vertices = event->v0Vertices();
    Int_t nv0 = v0Vertices.size();
    for (Int_t iv0=0;iv0<nv0;iv0++) {
      StV0Vertex *v0Vertex = v0Vertices[iv0];
      if (! v0Vertex) continue;
#ifdef  StTrackMassFit_hh
      Int_t key = v0Vertex->key();
      if (key <= 0) key = iv0;
      LOG_QA << Form("#V[%3i]",key) << *v0Vertex << endm;
#else
      LOG_QA << *v0Vertex << endm;
#endif
    }
  }// end prim vtx    

  if (event->xiVertices()  .size()) {
    LOG_QA << "# Xi vertices:       "	   << event->xiVertices().size() << endm;
  }
  if (event->kinkVertices().size()) {    LOG_QA << "# Kink vertices:       "
	 << event->kinkVertices().size() << endm;
  }
  // Report for jobTracking Db   (non-zero entry only)      
  if (event->v0Vertices()  .size()) {
    //        LOG_QA << "SequenceValue=" << mEventCounter 
    LOG_QA 
      << "StageID='3'"
      << ",MessageKey=" << "'V0Vertices', " << "MessageValue=" << event->v0Vertices()  .size() << endm;
  }
  if (event->xiVertices()  .size()) {
    //        LOG_QA << "SequenceValue=" << mEventCounter 
    LOG_QA 
      << "StageID='3'"
      << ",MessageKey=" << "'XiVertices', " << "MessageValue="<< event->xiVertices()  .size()  << endm;
  }
  
  if (event->kinkVertices().size()) {
    //        LOG_QA << "SequenceValue=" << mEventCounter 
    LOG_QA 
      << "StageID='3'"
      << ",MessageKey=" << "'KinkVertices'," << "MessageValue="<< event->kinkVertices().size() << endm;
  }
  
  UInt_t TotalNoOfTpcHits = 0, noBadTpcHits = 0, noTpcHitsUsedInFit = 0;
  StTpcHitCollection* TpcHitCollection = event->tpcHitCollection();
  if (TpcHitCollection) {
    UInt_t numberOfSectors = TpcHitCollection->numberOfSectors();
    for (UInt_t i = 0; i< numberOfSectors; i++) {
      StTpcSectorHitCollection* sectorCollection = TpcHitCollection->sector(i);
      if (sectorCollection) {
	Int_t numberOfPadrows = sectorCollection->numberOfPadrows();
	for (Int_t j = 0; j< numberOfPadrows; j++) {
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
  StSvtHitCollection* svthits = event->svtHitCollection();
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
  UInt_t TotalNoOfPxlHits = 0, noBadPxlHits = 0, noPxlHitsUsedInFit = 0;
  StPxlHitCollection* pxlhits = event->pxlHitCollection();
  if (pxlhits) {
    StPxlHit* hit;
    for (UInt_t sector=0; sector<pxlhits->numberOfSectors(); ++sector) {
      StPxlSectorHitCollection* sectorhits = pxlhits->sector(sector);
      if (!sectorhits) continue;
      for (UInt_t ladder=0; ladder<sectorhits->numberOfLadders(); ++ladder) {
	StPxlLadderHitCollection* ladderhits = sectorhits->ladder(ladder);
	if (!ladderhits) continue;
	for (UInt_t sensor=0; sensor<ladderhits->numberOfSensors(); ++sensor) {
	  StPxlSensorHitCollection* sensorhits = ladderhits->sensor(sensor);
	  if (!sensorhits) continue;
	  const StSPtrVecPxlHit& hits = sensorhits->hits();
	  for (const_StPxlHitIterator it=hits.begin(); it!=hits.end(); ++it) {
	    hit = static_cast<StPxlHit*>(*it);
	    if (!hit) continue;
	    TotalNoOfPxlHits++;
	    if (hit->flag() >3)   noBadPxlHits++;
	    if (hit->usedInFit()) noPxlHitsUsedInFit++;
	  }
	}
      }
    }
  }
  if (TotalNoOfPxlHits) {
    LOG_QA << "# PXL hits:          " << TotalNoOfPxlHits 
	   << ":\tBad ones(flag >3): " << noBadPxlHits 
	   << ":\tUsed in Fit:      " << noPxlHitsUsedInFit << endm;
  }
  UInt_t TotalNoOfIstHits = 0, noBadIstHits = 0, noIstHitsUsedInFit = 0;
  StIstHitCollection* isthits = event->istHitCollection();
  if (isthits) {
    StIstHit* hit;
    for (Int_t ladder=0; ladder<kIstNumLadders; ++ladder) {
      StIstLadderHitCollection* ladderhits = isthits->ladder(ladder);
      if (!ladderhits) continue;
      for (Int_t sensor=0; sensor<kIstNumSensorsPerLadder; ++sensor) {
	StIstSensorHitCollection* sensorhits = ladderhits->sensor(sensor);
	if (!sensorhits) continue;
	const StSPtrVecIstHit& hits = sensorhits->hits();
	for (const_StIstHitIterator it=hits.begin(); it!=hits.end(); ++it) {
	  hit = static_cast<StIstHit*>(*it);
	  if (!hit) continue;
	  TotalNoOfIstHits++;
	  if (hit->flag() >3)   noBadIstHits++;
	  if (hit->usedInFit()) noIstHitsUsedInFit++;
	}
      }
    }
  }
  if (TotalNoOfIstHits) {
    LOG_QA << "# IST hits:          " << TotalNoOfIstHits 
	   << ":\tBad ones(flag >3): " << noBadIstHits 
	   << ":\tUsed in Fit:      " << noIstHitsUsedInFit << endm;
  }

  UInt_t TotalNoOfSsdHits = 0, noBadSsdHits = 0, noSsdHitsUsedInFit = 0;
  StSsdHitCollection* ssdhits = event->ssdHitCollection();
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
  UInt_t TotalNoOfSstHits = 0, noBadSstHits = 0, noSstHitsUsedInFit = 0;
  StSstHitCollection* ssthits = event->sstHitCollection();
  if (ssthits) {
    StSstHit* hit;
    for (UInt_t ladder=0; ladder<ssthits->numberOfLadders(); ++ladder) {
      StSstLadderHitCollection* ladderhits = ssthits->ladder(ladder);
      if (!ladderhits) continue;
      for (UInt_t wafer=0; wafer<ladderhits->numberOfWafers(); ++wafer) {
	StSstWaferHitCollection* waferhits = ladderhits->wafer(wafer);
	if (!waferhits) continue;
	const StSPtrVecSstHit& hits = waferhits->hits();
	for (const_StSstHitIterator it=hits.begin(); it!=hits.end(); ++it) {
	  hit = static_cast<StSstHit*>(*it);
	  if (!hit) continue;
	  TotalNoOfSstHits++;
	  if (hit->flag() >3) noBadSstHits++;
	  if (hit->usedInFit()) noSstHitsUsedInFit++;
	}
      }
    }
  }
  if (TotalNoOfSstHits) {
    LOG_QA << "# SST hits:          " << TotalNoOfSstHits 
	   << ":\tBad ones(flag>3): " << noBadSstHits 
	   << ":\tUsed in Fit:      " << noSstHitsUsedInFit << endm;
  }
  UInt_t TotalNoOfFtpcHits = 0, noBadFtpcHits = 0, noFtpcHitsUsedInFit = 0;
  StFtpcHitCollection* ftpchits = event->ftpcHitCollection();
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
#ifdef  StRnDHit_hh
  StRnDHitCollection* rndhits = event->rndHitCollection();
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
#endif /*  StRnDHit_hh */
  StEmcCollection* emccol = event->emcCollection();
  if (emccol) {
    const Char_t *Names[2] = {"EMC ","EEMC"};
    for (Int_t be = 0; be < 2; be++) {// Barrel and Endcap
      Int_t d1 = 0; 
      Int_t d2 = 3;
      if (be) {d1 = 4; d2 =7;}
      Int_t Adcs[4] = {0, 0, 0, 0};
      Int_t Cls[4]  = {0, 0, 0, 0};
      for(Int_t d = d1; d <= d2; d++)  {  
	StDetectorId id = static_cast<StDetectorId>(d+kBarrelEmcTowerId);
	const StEmcDetector* detector=emccol->detector(id);
	if (detector) {
	  Int_t maxMod = 121;
	  if (d > 3) maxMod = 14;
	  for(Int_t j = 1; j < maxMod; j++) {
	    const StEmcModule* module = detector->module(j);
	    if(module) {
	      const StSPtrVecEmcRawHit& rawHit=module->hits();
	      for(UInt_t k=0;k<rawHit.size();k++) { //loop on hits in modules
		if (rawHit[k]->energy() <= 0.1) continue;
		Adcs[d-d1]++;
	      }
	    }
	  }
	  const StEmcClusterCollection *cl = detector->cluster();
	  if (cl) {
	    Cls[d-d1] = cl->numberOfClusters();
	  }
	}
      }
      Int_t np = 0;
      if (! be) np = emccol->barrelPoints().size();
      else      np = emccol->endcapPoints().size();
      if (np || 
	  Adcs[0] || Adcs[1] || Adcs[2] || Adcs[3] ||
	  Cls[0]  || Cls[1]  || Cls[2]  || Cls[3] ) {
	LOG_QA << Form("# %s points:%5i",Names[be],np);
	LOG_QA << Form(": Adc(T/p/E/P) %4i/%4i/%5i/%5i",Adcs[0],Adcs[1],Adcs[2],Adcs[3]);
	LOG_QA << Form(": Cls(T/p/E/P) %4i/%4i/%5i/%5i",Cls[0],Cls[1],Cls[2],Cls[3]);
	LOG_QA << endm;
      }
    }
  }
  const StBTofCollection* tof = event->btofCollection();
  if (tof) {
    const StSPtrVecBTofHit& tofHits = tof->tofHits();
    if (tofHits.size()) {
      Int_t n = tofHits.size();
      Int_t m = 0;
      for(Int_t i=0;i<n;i++) { //loop on hits in modules
	StBTofHit *aHit = tofHits[i];
	if(!aHit) continue;
	if (aHit->associatedTrack()) m++;
      }
      LOG_QA << Form("# BTof   hits:%5i: Matched with tracks:%5i",n,m) << endm; 
    }
  }
  const StPhmdCollection* pmdcol = event->phmdCollection();
  if (pmdcol) {
    const StPhmdDetector* pmd_det = pmdcol->detector(StDetectorId(kPhmdId));
    const StPhmdDetector* cpv_det = pmdcol->detector(StDetectorId(kPhmdCpvId));
    Int_t n = 0;
    Int_t m = 0;
    if (pmd_det) {
      const StPhmdClusterCollection* pmd_clusters = pmd_det->cluster();
      if (pmd_clusters) n = pmd_clusters->numberOfclusters();
    }
    if (cpv_det) {
      const StPhmdClusterCollection* cpv_clusters = cpv_det->cluster();
      if (cpv_clusters) m = cpv_clusters->numberOfclusters();
    }
    LOG_QA << Form("# Pmd  clusters:%5i: Cpv  clusters:%5i",n,m) << endm; 
  }
  if (event->fpdCollection() && event->fpdCollection()->numberOfADC()) {
    UShort_t sum = 0;
    for (UInt_t i = 0; i < event->fpdCollection()->numberOfADC(); i++) sum +=  event->fpdCollection()->adc()[i];
    if (sum) 
      LOG_QA << "# FPD ADC sum:            " << sum << endm;
  }
  if (event->fgtCollection()) {
    LOG_QA << "# FGT hits:            " << event->fgtCollection()->getNumHits() << endm;
  }
  if (event->richCollection() && event->richCollection()->getRichHits().size()) {
    LOG_QA << "# RICH hits:           " << event->richCollection()->getRichHits().size() << endm;
  }
  if (event->numberOfPsds()) {
    LOG_QA << "# PSDs:                " << event->numberOfPsds() << endm;
  }
#ifdef _ST_GMT_HIT_H_
  if (event->gmtCollection() && event->gmtCollection()->getNumHits()) {
    LOG_QA << "# GMT hits:           " << event->gmtCollection()->getNumHits() 
	   << " points: " << event->gmtCollection()->getNumPoints()
	   << endm;
  }
#endif /* _ST_GMT_HIT_H_ */
  if (event->rpsCollection()) {
    LOG_QA << "# RPS hits:            " << event->rpsCollection()->clusters().size() << endm;
  }
  
  LOG_QA << "*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-" << endm;
}
//________________________________________________________________________________
/* -------------------------------------------------------------------------
 * $Log: StAnalysisMaker.cxx,v $
 * Revision 2.26  2020/01/28 15:05:10  genevb
 * end-of-line needed for nShortTrackForETOF
 *
 * Revision 2.25  2020/01/27 21:28:30  genevb
 * Add short tracks toward ETOF
 *
 * Revision 2.24  2015/07/19 23:02:44  fisyak
 * Add print out for Sst, Gmt, pp2pp
 *
 * Revision 1.2  2014/01/15 23:12:46  fisyak
 * Freeze
 *
 * Revision 1.1.1.1  2013/08/29 14:21:56  fisyak
 * Freeze
 *
 * Revision 2.23  2012/12/18 17:16:26  fisyak
 * Add PrintVertex
 *
 * Revision 2.22  2012/11/25 22:22:45  fisyak
 * Add separators for summary
 *
 * Revision 2.21  2012/11/08 16:57:53  fisyak
 * Add pmd to summary
 *
 * Revision 2.20  2012/11/07 21:35:26  fisyak
 * Add to summary print out for EMC and ToF
 *
 * Revision 2.19  2012/10/23 19:44:18  fisyak
 * Add print out for ToF and Emc hits
 *
 * Revision 2.18  2012/09/16 21:59:14  fisyak
 * Compress print out, add PrintEmcHits
 *
 * Revision 2.17  2012/05/07 13:59:44  fisyak
 * enhance print out for primary vertixes
 *
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

