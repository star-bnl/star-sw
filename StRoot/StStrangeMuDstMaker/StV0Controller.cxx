// $Id: StV0Controller.cxx,v 2.1 2000/06/09 22:17:11 genevb Exp $
// $Log: StV0Controller.cxx,v $
// Revision 2.1  2000/06/09 22:17:11  genevb
// Allow MC data to be copied between DSTs, other small improvements
//
// Revision 2.0  2000/06/05 05:19:44  genevb
// New version of Strangeness micro DST package
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StV0Controller strangeness micro DST controller for V0s              //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "TTree.h"
#include "StEvent/StEvent.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StAssociationMaker/StAssociationMaker.h"
#include "StAssociationMaker/StTrackPairInfo.hh"
#include "StTrack.h"
#include "StGlobalTrack.h"
#include "StV0Vertex.h"
#include "StV0MuDst.hh"
#include "StV0Mc.hh"
#include "StMcEventTypes.hh"
#include "StParticleDefinition.hh"

#include "StStrangeControllerInclude.h"  // Location of header for this class

class StStrangeEvMuDst;

//_____________________________________________________________________________
StV0Controller::StV0Controller() : StStrangeControllerBase("V0") {
}
//_____________________________________________________________________________
StV0Controller::~StV0Controller() {
}
//_____________________________________________________________________________
Int_t StV0Controller::MakeReadDst() {

  StStrangeEvMuDst* ev = masterMaker->GetEvent();      // Tell the vertices
  entries = GetN();                                    // about the event
  for (Int_t j = 0; j<entries; j++) {
    StV0MuDst* v0 = (StV0MuDst*) (*dataArray)[j];
    v0->SetEvent(ev);
  }
  PrintNumCand("read",entries);
  nEntries += entries;

  return kStOK;
}
//_____________________________________________________________________________
Int_t StV0Controller::MakeCreateDst(StEvent& event) {

  // Loop over vertices to build array of candidates
  StSPtrVecV0Vertex& v0Vertices = event.v0Vertices();
  entries = v0Vertices.size();
  Int_t asize = dataArray->GetSize();
  if (entries > asize) dataArray->Expand(entries+increment);
  StStrangeEvMuDst* ev = masterMaker->GetEvent();
  for (Int_t i=0; i<entries; i++) {
    StV0Vertex* v0Vertex = v0Vertices[i];
    new((*dataArray)[i]) StV0MuDst(v0Vertex,ev);
  }
  PrintNumCand("found",entries);
  nEntries += entries;

  return kStOK;
}
//_____________________________________________________________________________
Int_t StV0Controller::MakeCreateMcDst(StMcVertex* mcVert) {  

  mcV0MapType* theMcV0Map = 0;
  mcTrackMapType* theMcTrackMap = 0;
  if (assocMaker) {
    theMcV0Map = assocMaker->mcV0Map();
    theMcTrackMap = assocMaker->mcTrackMap();
  }
  StMcTrack *Pos = 0; 
  StMcTrack *Neg = 0;
  StV0Vertex* rcV0Partner = 0;
  Int_t indexRecoArray = -1;
  Int_t count = theMcV0Map->count(mcVert);
  
  if ((assocMaker)&&(count>0)) {
    pair<mcV0MapIter,mcV0MapIter> mcV0Bounds = theMcV0Map->equal_range(mcVert);
    rcV0Partner = (*mcV0Bounds.first).second;
    float x, y, z, delta;
    x = (mcVert)->position().x();
    y = (mcVert)->position().y();
    z = (mcVert)->position().z();
    delta = (x - rcV0Partner->position().x())*(x - rcV0Partner->position().x())+
        (y - rcV0Partner->position().y())*(y - rcV0Partner->position().y())+
        (z - rcV0Partner->position().z())*(z - rcV0Partner->position().z());

    //Now loop over the bounds      
    for(mcV0MapIter mcV0MapIt = mcV0Bounds.first;
                    mcV0MapIt != mcV0Bounds.second; ++mcV0MapIt) {
      StV0Vertex *temp = (*mcV0MapIt).second;
      if ((x - temp->position().x())*(x - temp->position().x())+
          (y - temp->position().y())*(y - temp->position().y())+
          (z - temp->position().z())*(z - temp->position().z()) < delta)
		      rcV0Partner = (*mcV0MapIt).second;
    }
    // stupid way
    for(Int_t i = 0; i < GetN(); i++) {
      StV0MuDst* tmpV0 = (StV0MuDst*) dataArray->At(i);
      if( fabs(rcV0Partner->position().x()-tmpV0->decayVertexV0X()) < 0.00001 &&
          fabs(rcV0Partner->position().y()-tmpV0->decayVertexV0Y()) < 0.00001 &&
          fabs(rcV0Partner->position().z()-tmpV0->decayVertexV0Z()) < 0.00001 )
      { indexRecoArray = i; break; }
    }
  }

  StSPtrVecMcTrack& Daughters = mcVert->daughters();
  for (StMcTrackIterator DTrackIt = Daughters.begin();
                         DTrackIt != Daughters.end(); DTrackIt++) {
    if (((*DTrackIt)->geantId()==8)||((*DTrackIt)->geantId()==14)) 
      Pos = (*DTrackIt);
    if (((*DTrackIt)->geantId()==9)||((*DTrackIt)->geantId()==15))
      Neg = (*DTrackIt);
  }
  if ((Pos)&&(Neg)) {
    new((*mcArray)[mcEntries++]) 
          StV0Mc(mcVert,Pos,Neg);
    if((assocMaker)&&(count>0)) {
      new((*assocArray)[assocEntries++]) 
            StStrangeAssoc(indexRecoArray,mcEntries-1);
      StGlobalTrack *globalMatch;

      pair<mcTrackMapIter,mcTrackMapIter> mcTrackBounds = 
            theMcTrackMap->equal_range(Pos);
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
			   
        ((StV0Mc *)mcArray->At(mcEntries-1))->SetHitInfoPositive(hits,commonHits);
      }

      //		   pair<mcTrackMapIter,mcTrackMapIter> 
      mcTrackBounds = theMcTrackMap->equal_range(Neg);
      //		   StTrackPairInfo*   
      bestPairInfo = (*mcTrackBounds.first).second;
			
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
		       
        ((StV0Mc *)mcArray->At(mcEntries-1))->SetHitInfoNegative(hits,commonHits);
      }
    }
  }
  
  return kStOK;
}
//_____________________________________________________________________________
ClassImp(StV0Controller)
