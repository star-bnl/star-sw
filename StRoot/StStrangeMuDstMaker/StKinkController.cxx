// $Id: StKinkController.cxx,v 2.0 2000/06/05 05:19:39 genevb Exp $
// $Log: StKinkController.cxx,v $
// Revision 2.0  2000/06/05 05:19:39  genevb
// New version of Strangeness micro DST package
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StKinkController strangeness micro DST controller for Kinks              //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "TTree.h"
#include "StEvent/StEvent.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StAssociationMaker/StAssociationMaker.h"
#include "StAssociationMaker/StTrackPairInfo.hh"
#include "StTrack.h"
#include "StGlobalTrack.h"
#include "StKinkVertex.h"
#include "StKinkMuDst.hh"
#include "StKinkMc.hh"
#include "StMcEventTypes.hh"
#include "StParticleDefinition.hh"

#include "StStrangeControllerInclude.h"  // Location of header for this class

class StStrangeEvMuDst;

//_____________________________________________________________________________
StKinkController::StKinkController() : StStrangeControllerBase("Kink") {
  increment = 100;
  max = 500;
}
//_____________________________________________________________________________
StKinkController::~StKinkController() {
}
//_____________________________________________________________________________
Int_t StKinkController::MakeReadDst() {

  entries = GetN();
  PrintNumCand("read",entries);
  nEntries += entries;

  return kStOK;
}
//_____________________________________________________________________________
Int_t StKinkController::MakeCreateDst(StEvent& event) {

  // Loop over vertices to build array of candidates
  StSPtrVecKinkVertex& kinkVertices = event.kinkVertices();
  entries = kinkVertices.size();
  Int_t asize = dataArray->GetSize();
  if (entries > asize) dataArray->Expand(entries+increment);
  for (Int_t i=0; i<entries; i++) {
    StKinkVertex* kinkVertex = kinkVertices[i];
    new((*dataArray)[i]) StKinkMuDst(kinkVertex);
  }
  PrintNumCand("found",entries);
  nEntries += entries;

  return kStOK;
}
//_____________________________________________________________________________
Int_t StKinkController::MakeCreateMcDst(StMcVertex* mcVert) {  

  mcKinkMapType* theMcKinkMap = 0;
  mcTrackMapType* theMcTrackMap = 0;
  if (assocMaker) {
    theMcKinkMap = assocMaker->mcKinkMap();
    theMcTrackMap = assocMaker->mcTrackMap();
  }
  StKinkVertex* rcKinkPartner = 0;
  Int_t indexRecoArray = -1;
  Int_t count = theMcKinkMap->count(mcVert);
  StSPtrVecMcTrack& Daughters = mcVert->daughters();	
  
  for (StMcTrackIterator DTrackIt = Daughters.begin();
                         DTrackIt != Daughters.end(); DTrackIt++) {
    if (!(Int_t)(*DTrackIt)->particleDefinition()->charge()) continue;
    new((*mcArray)[mcEntries++]) StKinkMc(mcVert,(*DTrackIt));
    if ((assocMaker)&&(count>0)) {
      pair<mcKinkMapIter,mcKinkMapIter> mcKinkBounds =
            theMcKinkMap->equal_range(mcVert);
      indexRecoArray = -1;

      rcKinkPartner = (*mcKinkBounds.first).second;
      float x, y, z, delta;
      x = (mcVert)->position().x();
      y = (mcVert)->position().y();
      z = (mcVert)->position().z();
      delta = (x - rcKinkPartner->position().x())*(x - rcKinkPartner->position().x())+
        (y - rcKinkPartner->position().y())*(y - rcKinkPartner->position().y())+
        (z - rcKinkPartner->position().z())*(z - rcKinkPartner->position().z());

      //Now loop over the bounds      
      for(mcKinkMapIter mcKinkMapIt = mcKinkBounds.first;
                      mcKinkMapIt != mcKinkBounds.second; ++mcKinkMapIt) {
        StKinkVertex *temp = (*mcKinkMapIt).second;
        if ((x - temp->position().x())*(x - temp->position().x())+
            (y - temp->position().y())*(y - temp->position().y())+
            (z - temp->position().z())*(z - temp->position().z()) < delta)
                rcKinkPartner = (*mcKinkMapIt).second;
      }
      // stupid way
      for(Int_t i = 0; i <= dataArray->GetLast(); i++) {
        StKinkMuDst* tmpKink = (StKinkMuDst*) dataArray->At(i);
        if( fabs(rcKinkPartner->position().x()-tmpKink->positionX()) < 0.00001 &&
            fabs(rcKinkPartner->position().y()-tmpKink->positionY()) < 0.00001 &&
            fabs(rcKinkPartner->position().z()-tmpKink->positionZ()) < 0.00001 )
        { indexRecoArray = i; break; }
      }
      new((*assocArray)[assocEntries++]) 
		    StStrangeAssoc(indexRecoArray,mcEntries-1);
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
			   
          ((StKinkMc*) mcArray->At(mcEntries-1))->SetHitInfo(hits,commonHits);
        }
      }
    }
    break;
  }
  
  return kStOK;
}
//_____________________________________________________________________________
Int_t StKinkController::MakeCreateSubDst() {

  // If no entries to copy, skip event
  if (!entries) return kStOK;

  // Event info copied directly from dstMaker.
  if ((*selections)[0] < 0) {  // Copy all from the event
    tree->SetBranchAddress(GetName(),&tempArray);
  } else if (entries) {        // Copy selected from the event
    Int_t asize = dataArray->GetSize();
    if (entries > asize) dataArray->Expand(entries+increment);
    for (Int_t k=0; k<entries; k++) {
      new((*dataArray)[k]) StKinkMuDst(*((StKinkMuDst*)
            (GetDstController()->Get((*selections)[k]))));
    }
  }
  PrintNumCand("copying",entries);
  nEntries += entries;
  
  return kStOK;
}
//_____________________________________________________________________________
ClassImp(StKinkController)
