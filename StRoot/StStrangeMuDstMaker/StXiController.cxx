// $Id: StXiController.cxx,v 2.1 2000/06/09 22:17:11 genevb Exp $
// $Log: StXiController.cxx,v $
// Revision 2.1  2000/06/09 22:17:11  genevb
// Allow MC data to be copied between DSTs, other small improvements
//
// Revision 2.0  2000/06/05 05:19:46  genevb
// New version of Strangeness micro DST package
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StXiController strangeness micro DST controller for Xis              //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "TTree.h"
#include "StEvent/StEvent.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StAssociationMaker/StAssociationMaker.h"
#include "StAssociationMaker/StTrackPairInfo.hh"
#include "StTrack.h"
#include "StGlobalTrack.h"
#include "StXiVertex.h"
#include "StXiMuDst.hh"
#include "StXiMc.hh"
#include "StMcEventTypes.hh"
#include "StParticleDefinition.hh"

#include "StStrangeControllerInclude.h"  // Location of header for this class

class StStrangeEvMuDst;

//_____________________________________________________________________________
StXiController::StXiController() : StStrangeControllerBase("Xi") {
}
//_____________________________________________________________________________
StXiController::~StXiController() {
}
//_____________________________________________________________________________
Int_t StXiController::MakeReadDst() {

  StStrangeEvMuDst* ev = masterMaker->GetEvent();      // Tell the vertices
  entries = GetN();                                    // about the event
  for (Int_t j = 0; j<entries; j++) {
    StXiMuDst* xi = (StXiMuDst*) (*dataArray)[j];
    xi->SetEvent(ev);
  }
  PrintNumCand("read",entries);
  nEntries += entries;

  return kStOK;
}
//_____________________________________________________________________________
Int_t StXiController::MakeCreateDst(StEvent& event) {

  // Loop over vertices to build array of candidates
  Int_t nBad = 0;
  StSPtrVecXiVertex& xiVertices = event.xiVertices();
  entries = xiVertices.size();
  Int_t asize = dataArray->GetSize();
  if (entries > asize) dataArray->Expand(entries+increment);
  StStrangeEvMuDst* ev = masterMaker->GetEvent();
  for (Int_t i=0; i<entries; i++) {
    StXiVertex* xiVertex = xiVertices[i];
    StV0Vertex* v0Vertex = xiVertex->v0Vertex();
    if (v0Vertex) {
      new((*dataArray)[i]) StXiMuDst(xiVertex,v0Vertex,ev);
    } else nBad++;
  }
  PrintNumCand("found",entries);
  if (nBad) gMessMgr->Warning() << "StXiController: " << nBad
                                << "with missing V0 vertices" << endm;
  nEntries += entries;

  return kStOK;
}
//_____________________________________________________________________________
Int_t StXiController::MakeCreateMcDst(StMcVertex* mcVert) {  

  mcXiMapType* theMcXiMap = 0;
  mcTrackMapType* theMcTrackMap = 0;
  if (assocMaker) {
    theMcXiMap = assocMaker->mcXiMap();
    theMcTrackMap = assocMaker->mcTrackMap();
  }
  StXiVertex* rcXiPartner = 0;
  Int_t indexRecoArray = -1;
  Int_t count = theMcXiMap->count(mcVert);
  StSPtrVecMcTrack& Daughters = mcVert->daughters();
  
  for (StMcTrackIterator DTrackIt = Daughters.begin();
                         DTrackIt != Daughters.end(); DTrackIt++) {
    if (!(Int_t)(*DTrackIt)->particleDefinition()->charge()) continue;
    new((*mcArray)[mcEntries]) StXiMc(mcVert,(*DTrackIt));
    if ((assocMaker)&&(count>0)) {
      pair<mcXiMapIter,mcXiMapIter> mcXiBounds =
            theMcXiMap->equal_range(mcVert);
      indexRecoArray = -1;
      for(mcXiMapIter mcXiMapIt = mcXiBounds.first;
		      mcXiMapIt != mcXiBounds.second; ++mcXiMapIt) {
        rcXiPartner = (*mcXiMapIt).second;
        // stupid way
        for(Int_t i = 0; i <= GetN(); i++) {
          StXiMuDst* tmpXi = (StXiMuDst*) dataArray->At(i);
          if( fabs(rcXiPartner->position().x()-tmpXi->decayVertexXiX()) < 0.00001 &&
              fabs(rcXiPartner->position().y()-tmpXi->decayVertexXiY()) < 0.00001 &&
              fabs(rcXiPartner->position().z()-tmpXi->decayVertexXiZ()) < 0.00001 )
          { indexRecoArray = i; break; }
        }
      }
      new((*assocArray)[assocEntries++]) 
		    StStrangeAssoc(indexRecoArray,mcEntries++);
      if(indexRecoArray!=-1) {
        StGlobalTrack *globalMatch;
        pair<mcTrackMapIter,mcTrackMapIter> mcTrackBounds = 
              theMcTrackMap->equal_range(*DTrackIt);
        StTrackPairInfo*   bestPairInfo = (*mcTrackBounds.first).second;
        for(mcTrackMapIter mcMapIt = mcTrackBounds.first;
                           mcMapIt != mcTrackBounds.second; ++mcMapIt) {
          if ((*mcMapIt).second->commonTpcHits() > bestPairInfo->commonTpcHits())
	         bestPairInfo = (*mcMapIt).second;
        } 
        if (mcTrackBounds.first != mcTrackBounds.second) {
          Int_t hits = 0, commonHits = 0;
          globalMatch = bestPairInfo->partnerTrack();
          commonHits = bestPairInfo->commonTpcHits();  //Common hits
          StPtrVecHit recTpcHits =globalMatch->detectorInfo()->hits(kTpcId);
          hits = recTpcHits.size();                    //Reconstructed hits
			   
          ((StXiMc *)mcArray->At(mcEntries-1))->SetHitInfo(hits,commonHits);
        }
      }
    }
    break;
  }
  
  return kStOK;
}
//_____________________________________________________________________________
ClassImp(StXiController)
