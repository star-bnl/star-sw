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
 * $Id: StAnalysisMaker.cxx,v 2.13 2009/11/23 15:54:28 fisyak Exp $
 *
 */


//
//  Include header files. What has to be included strongly depends
//  on your implementation. StEventTypes.h contains all includes
//  you need to use StEvent.
//
#include "StAnalysisMaker.h"
#include "StEventTypes.h"
#include "TNtuple.h"
#include "TFile.h"
#include "StMessMgr.h"
#include "StDcaGeometry.h"
#if ROOT_VERSION_CODE < 334081
#include "TArrayL.h"
#else
#include "TArrayL64.h"
#endif
#include "TClassTable.h"
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
    mFile = 0;
    mTuple = 0;
}



/*!
 *
 *  Usually ok to leave this as it is, the destructor should
 *  however free/delete private data allocated in other part
 *  of the code.
 *
 */
StAnalysisMaker::~StAnalysisMaker() { /* noop */ }



/*!
 *
 * Called once at the beginning.
 * This is a good place to book histos and tuples.
 *
 */
Int_t
StAnalysisMaker::Init()
{
    //
    //  Output file.
    //  mFileName should contain a valid filename. Here
    //  we dump the file into the null device.
    //
    mFileName = "/dev/null";    
    mFile =  new TFile(mFileName.c_str(), "RECREATE");
    gMessMgr->Info() << "StAnalysisMaker::Init(): "
		     << "Histograms will be stored in file '"
		     <<  mFileName.c_str() << "'" << endm;
    
    //
    //  Define Ntuple.
    //  Keep the order TFile -> TNtuple
    //  
    string varList = "evt:xvt:yvtx:zvtx:ctbsum:zdcsum:nprimary:npions:goodfrac";
    mTuple = new TNtuple("example","example",varList.c_str());
    
    //
    //  Call Init() of the base class.
    //  Always leave this in.
    //
    return StMaker::Init();
}

/*!
 *
 *  Called every event after Make(). Usually you do not
 *  need to do anything here. Leave it as it is.
 *
 */
void
StAnalysisMaker::Clear(Option_t *opt)
{
    StMaker::Clear();
}

/*!
 *
 * Called once at the end.
 *
 */
Int_t
StAnalysisMaker::Finish()
{
    //
    //  A good place for printout and to summarize
    //  the run.
    //
    gMessMgr->Info() << "StAnalysisMaker::Finish() "
		     << "Processed " << mEventCounter << " events." << endm;
    
    //
    //  Write Ntuple/histos to file and close it.
    //  
    if( mFile){
      mFile->Write();  
      mFile->Close();
    }
    
    return kStOK;
}

/*!
 *  This method is called every event. That's the
 *  right place to plug in your analysis. 
 */
Int_t
StAnalysisMaker::Make()
{
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

    //
    //  Ok we survived the filter. Now it is time
    //  to do something with the event.
    //  In the following we simply fill the Ntuple.
    //
    int k = 0;
    float tuple[10];
    tuple[k++] = event->id();                               // the event number
    
    tuple[k++] = event->primaryVertex()->position().x();    // x-vertex
    tuple[k++] = event->primaryVertex()->position().y();    // y-vertex
    tuple[k++] = event->primaryVertex()->position().z();    // z-vertex


    // Y3 trigger Id dump
    StTriggerIdCollection *trgcol = event->triggerIdCollection();
    if ( ! trgcol ){
      gMessMgr->Warning() << "StAnalysisMaker::Make : No triggerIdCollection" << endm;
    } else {
      const StTriggerId *l1 = trgcol->l1();
      const StTriggerId *l2 = trgcol->l2();
      const StTriggerId *l3 = trgcol->l3();
      const StTriggerId *nominal = trgcol->nominal();

      if(l1) {
	vector<unsigned int> l1Vec = l1->triggerIds();
	cout << "L1: Mask " <<l1->mask() << " " ;
	for (vector<unsigned int>::iterator viter = l1Vec.begin();
	     viter != l1Vec.end(); ++viter) {
	  cout << (*viter) << "," ;
	}
	cout << endl;
      }
      if(l2) {
	vector<unsigned int> l2Vec = l2->triggerIds();
	cout << "L2: Mask " <<l2->mask() << " " ;
	for (vector<unsigned int>::iterator viter = l2Vec.begin();
	     viter != l2Vec.end(); ++viter) {
	  cout << (*viter) << "," ;
	}
	cout << endl;
      }
      if(l3) {
	vector<unsigned int> l3Vec = l3->triggerIds();
	cout << "L3: Mask " <<l3->mask() << " " ;
	for (vector<unsigned int>::iterator viter = l3Vec.begin();
	     viter != l3Vec.end(); ++viter) {
	  cout << (*viter) << "," ;
	}
	cout << endl;
      }
      
      if(nominal) {
	vector<unsigned int> nominalVec = nominal->triggerIds();
	cout << "NOMINAL: Mask " <<nominal->mask() << " " ;
	for (vector<unsigned int>::iterator viter = nominalVec.begin();
	     viter != nominalVec.end(); ++viter) {
	  cout << (*viter) << "," ;
	}
	cout << endl;
      }
    }


    //
    //  Get the ZDC and CTB data.
    //  
    StTriggerDetectorCollection *theTriggers = event->triggerDetectorCollection();
    if (!theTriggers){
      // good idea to check if the data is available at all
      gMessMgr->Warning() << "StAnalysisMaker::Make : no triggerDetectorCollection" << endm;
      return kStOK;           
    }
    StCtbTriggerDetector &theCtb = theTriggers->ctb();
    StZdcTriggerDetector &theZdc = theTriggers->zdc();

    //
    //  Sum all CTB counter
    float ctbsum = 0;
    for (UInt_t  islat=0; islat<theCtb.numberOfSlats(); islat++) 
	for (UInt_t  itray=0; itray<theCtb.numberOfTrays(); itray++)
	    ctbsum += theCtb.mips(itray, islat, 0);

    tuple[k++] = ctbsum;            // CTB
    tuple[k++] = theZdc.adcSum();   // ZDC
    
    //
    //  Count tracks
    //  This is just an example on how to use little
    //  helper functions which makes the code more readable.
    //
    tuple[k++] = countPrimaryTracks(*event);
    tuple[k++] = countPrimaryPions(*event);

    //
    //  Last but not least we count the number of good global
    //  tracks. Good or bad is determined by the track filter.
    //  The fraction of good globals gets stored in the tuple. 
    //
    int allGlobals = 0;
    int goodGlobals = 0;

    StTrack *track;
    StSPtrVecTrackNode& nodes = event->trackNodes();
    for (UInt_t  j=0; j<nodes.size(); j++) {
	track = nodes[j]->track(global);
	if (track) allGlobals++;
	if (accept(track)) goodGlobals++;
    }
    if (allGlobals) tuple[k++] = static_cast<float>(goodGlobals)/allGlobals;
    else            tuple[k++] = -1;
    //
    //  That's it.
    //  Store the current tuple. See you next event.
    //
    mTuple->Fill(tuple);

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
void StAnalysisMaker::PrintStEvent(Int_t k) {
  static const Char_t *trackType[] = {"global", "primary", "tpt", "secondary", "estGlobal", "estPrimary"};
  StEvent* pEvent = (StEvent*) StMaker::GetChain()->GetInputDS("StEvent");
  if (!pEvent) return;
  cout << "Event: Run "<< pEvent->runId() << " Event No: " << pEvent->id() << endl;
  cout << "Vertex Positions" << endl;
  const StPrimaryVertex *pvertex = pEvent->primaryVertex();
  if (pvertex ) {
    const StThreeVectorF &position = pvertex->position();
    cout << "Event: Vertex Position " 
	 << "\t" << position.x()
	 << "\t" << position.y()
	 << "\t" << position.z()
	 << endl;
  }
  else {
    cout << "Event: Vertex Not Found" << endl;
  }
  
  StSPtrVecTrackNode& trackNode = pEvent->trackNodes();
  UInt_t nTracks = trackNode.size();
  StTrackNode *node = 0;
  Int_t line = 0;
  for (UInt_t  i=0; i < nTracks; i++) {
    node = trackNode[i]; if (!node) continue;
    StGlobalTrack* gTrack = static_cast<StGlobalTrack*>(node->track(global));
    StDcaGeometry* dca    = gTrack->dcaGeometry();
    StPrimaryTrack *pTrack = 	static_cast<StPrimaryTrack*>(node->track(primary));
    for (int l = 0; l < 2; l++) {
      StTrack        *track = 0;
      if (k%10 > 0 && k%10 != l+1) continue;
      if (l == global)  track = gTrack;
      if (l == primary) track = pTrack;
      if (track) {
	if (dca && l == global) {
	  if (! line) {
	    cout << "track# type   flag       z     mom      pT     eta     phi  c   imp  +/-            z    +/-           rho    "
		 << "NPP length    DCA    NFP   chi2   NP  hit  X        Y        Z" << endl;
	    line++;
	  }
	  Double_t eta = - TMath::Log(TMath::Tan((TMath::Pi()/2-dca->dipAngle())/2));
	  cout << Form("%4d%10s%4d%8.3f%8.3f%8.3f%8.3f%8.2f",
		       i,trackType[l],track->flag(),dca->z(),dca->momentum().mag(),dca->pt(),eta,180./TMath::Pi()*dca->psi());
	  Short_t charge = track->geometry()->charge();
	  const Float_t *errMx =  dca->errMatrix();
	  Double_t sigmaX = TMath::Sqrt(errMx[0]);
	  Double_t sigmaZ = TMath::Sqrt(errMx[2]);
	  Double_t rho = errMx[1]/(sigmaX*sigmaZ);
	  cout << Form(" %2d %8.3f+-%8.3f",charge,dca->impact(), sigmaX);
	  cout << Form(" %8.3f+-%8.3f %8.3f",dca->z(), sigmaZ, rho);
	  Double_t length = track->length();
	  if (length > 9999.) length = 9999.;
	  cout << Form(" %4d%8.3f%8.3f", track->numberOfPossiblePoints(),length,track->impactParameter());
	  cout << Form(" %4d%8.3f%4d",track->fitTraits().numberOfFitPoints(), track->fitTraits().chi2(), track->detectorInfo()->numberOfPoints());
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
      cout << " Svt p/h/f" << track->numberOfPossiblePoints(kSvtId) 
	   << "/" << track->detectorInfo()->hits(kSvtId).size()
	   << "/" << track->fitTraits().numberOfFitPoints(kSvtId);
      cout << endl;
      }
    } // l
    //    if (i > 5) break;
  }  
}
//________________________________________________________________________________
void StAnalysisMaker::PrintTpcHits(Int_t sector, Int_t row, Bool_t plot) {
  struct BPoint_t {
    Float_t sector, row, x, y, z, q;
  };
  static const Char_t *vname = "sector:row:x:y:z:q";
  static TNtuple *Nt = 0;
  if (plot && Nt == 0) Nt = new TNtuple("TpcHit","TpcHit",vname);
    
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

