//
// $Id: StEventCompendiumMaker.cxx,v 1.3 2012/05/07 14:43:47 fisyak Exp $
//
//#include <iostream>
#include "Stiostream.h"
#include <cmath>
#include "StEventCompendiumMaker.h"
#include "TDataSet.h"
#include "StDetectorDbMaker/St_MagFactorC.h"
#include "StMessMgr.h"
#include "StEvent.h"
#include "StEventSummary.h"
#include "StContainers.h"
#include "StTrackNode.h"
#include "StTrack.h"
#include "StTrackGeometry.h"
#include "StPrimaryVertex.h" 

static const char rcsid[] = "$Id: StEventCompendiumMaker.cxx,v 1.3 2012/05/07 14:43:47 fisyak Exp $";
ClassImp(StEventCompendiumMaker)
//________________________________________________________________________________
Int_t StEventCompendiumMaker::Make(){
    StEvent* rEvent = 0;
    rEvent = (StEvent*) GetInputDS("StEvent");
    if (!rEvent) {
	gMessMgr->Warning() << "StEventCompendiumMaker::Make: No StEvent found, bail out!"  << endm;
	return kStWarn;
    }
    fillEventSummary(rEvent);
    // the magnetic field needs to be obtained from the database.
    // this are the magic words...
    St_MagFactorC* mMagTable = St_MagFactorC::instance();
    double scalef = mMagTable->ScaleFactor();
    double bfieldz = scalef * 4.97952;  // (value returned from gufld at 0,0,0 for FullField)
    rEvent->summary()->setMagneticField(bfieldz);
    if (Debug() > 1) {
	rEvent->summary()->Dump();
    }
    return kStOK;
}
//________________________________________________________________________________
void StEventCompendiumMaker::fillEventSummary(StEvent* e) {
    StEventSummary* summary = e->summary();
    if (!e->trackNodes().size()){
	cout << "fillEventSummary: Zero tracks nodes in StEvent...\n" << endl;
    }
    // Initialize global track counters & sum variables
    int     glb_trk_good(0), glb_trk_plus(0), glb_trk_minus(0);
    int     prim_trk_good(0);
    float   mean_pt(0), mean_pt2(0), mean_eta(0), rms_eta(0);
    // Fill pt, eta & phi histograms  */
    const StSPtrVecTrackNode& nodes = e->trackNodes();
    for (size_t itrk=0; itrk < nodes.size(); itrk++) {/* begin global track loop */
	StTrack* gtrk = nodes[itrk]->track(global);
	// Calculate track multiplicities 
	
	//  good global tracks 
	if ( gtrk->flag() <= 0 ) continue;
	glb_trk_good++;
	
	// good primary tracks
	StTrack* ptrk = nodes[itrk]->track(primary);
	if(ptrk && ptrk->flag()>0) prim_trk_good++;
	
	// count plus & minus
	if ( gtrk->geometry()->charge() > 0 ) glb_trk_plus++;     //  charge = 1 
	if ( gtrk->geometry()->charge() < 0 ) glb_trk_minus++;    //  charge = -1
	
	const StThreeVectorF& mom = gtrk->geometry()->momentum();
	//  Calculate kinematic varialbles for good tracks only
	float eta   = mom.pseudoRapidity();
	float pt    = mom.perp();
	
	// Sum pt, pt^2, eta, eta^2  for all good global charged tracks*/ 
	mean_pt  += pt;
	mean_pt2 += pt*pt;
	mean_eta += eta;
	rms_eta  += eta*eta;
    }// end of global track loop
    
    //  Fill track multiplicities into StEventSummary
    summary->setNumberOfTracks(nodes.size());
    summary->setNumberOfGoodTracks(glb_trk_good);
    summary->setNumberOfGoodPrimaryTracks(prim_trk_good);
    summary->setNumberOfGoodTracks(positive,glb_trk_plus);
    summary->setNumberOfGoodTracks(negative,glb_trk_minus);
    // summary->setNumberOfExoticTracks(); //No current definition of "exotic" tracks.
    summary->setNumberOfVertices(e->numberOfPrimaryVertices());

    // Count v0, xi, and kink candidates
    summary->setNumberOfVerticesForType(kV0VtxId,e->v0Vertices().size());
    summary->setNumberOfVerticesForType(kXiVtxId,e->xiVertices().size());
    summary->setNumberOfVerticesForType(kKinkVtxId,e->kinkVertices().size());
    

    // Pileup Vertices
    // This assumes that the pileup vertices are inserted into the primary vertex container
    // and are flagged with kOtherVtxId as their StVertexId.
    // If this is not true, then this number is incorrect.
    size_t nPileupVert = 0;
    for (size_t i=0; i<e->numberOfPrimaryVertices();++i) {
	StVertexId vtx_id = e->primaryVertex(i)->type();
	if(vtx_id == kOtherVtxId)  nPileupVert++ ;
    }
    summary->setNumberOfPileupVertices(nPileupVert);   
    
    // Fill mean eta, pt, pt^2 and rms_eta
    if (glb_trk_good > 0) {
	summary->setMeanPt(mean_pt/(float)glb_trk_good);
	summary->setMeanPt2(mean_pt2/(float)glb_trk_good);
	summary->setMeanEta(mean_eta/(float)glb_trk_good);
	summary->setRmsEta(sqrt(rms_eta/(float)glb_trk_good));
    }
    
    // Fill Primary vertex information
    if (e->primaryVertex()) {
	summary->setPrimaryVertexPosition(e->primaryVertex()->position());
    }
    

    // The eta, pt, phi, emc energy vs eta and emc energy vs phi histograms
    // are _not_ filled by this routine.
    // Also, StEventSummary doesn't have the mutator methods for them.
  
  return;
}  //  End of fillEventSummary


