// $Id: StV0Controller.cxx,v 3.11 2011/04/03 15:51:58 fisyak Exp $
// $Log: StV0Controller.cxx,v $
// Revision 3.11  2011/04/03 15:51:58  fisyak
// Fix effect of constness in StAssociationMaker
//
// Revision 3.10  2002/08/20 12:45:35  jones
// Fix to MakeCreateMcDst in StV0Controller; better file handling in StStrangeMuDstPlayer
//
// Revision 3.9  2002/06/13 16:06:01  genevb
// Additional security against zombies in StEvent vectors
//
// Revision 3.8  2002/05/29 19:09:51  genevb
// Removed some mistakes left in last time
//
// Revision 3.7  2002/04/30 16:02:48  genevb
// Common muDst, improved MC code, better kinks, StrangeCuts now a branch
//
// Revision 3.6  2001/05/04 20:15:14  genevb
// Common interfaces and reorganization of components, add MC event info
//
// Revision 3.5  2001/04/25 18:22:15  perev
// HPcorrs
//
// Revision 3.4  2000/09/18 19:25:19  genevb
// Additional protection for missing MC info
//
// Revision 3.3  2000/08/31 21:25:34  genevb
// Adjustment for V0s used in Xis only
//
// Revision 3.2  2000/07/17 20:28:40  genevb
// File size limitation workaround, some under the hood improvements
//
// Revision 3.1  2000/07/14 21:28:34  genevb
// Added V0Mc index for XiMc, fixed bug with entries for XiMc, cleaned up controllers
//
// Revision 3.0  2000/07/14 12:56:50  genevb
// Revision 3 has event multiplicities and dedx information for vertex tracks
//
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
#include "StTrackDetectorInfo.h"

#include "StStrangeControllerInclude.h"  // Location of header for this class

class StStrangeEvMuDst;

//_____________________________________________________________________________
StV0Controller::StV0Controller() : StStrangeControllerBase(v0T) {
}
//_____________________________________________________________________________
StV0Controller::~StV0Controller() {
}
//_____________________________________________________________________________
Int_t StV0Controller::MakeReadDst() {

  Int_t j;
  StStrangeEvMuDst* ev = masterMaker->GetEvent();      // Tell the vertices
  entries = GetN();                                    // about the event
  for (j = 0; j<entries; j++) {
    StV0MuDst* v0 = (StV0MuDst*) (*dataArray)[j];
    v0->SetEvent(ev);
  }
  PrintNumCand("read",entries);
  nEntries += entries;

  if (doMc) {
    ev = masterMaker->GetMcEvent();
    Int_t mc_entries = GetNMc();
    for (j = 0; j<mc_entries; j++) {
      StV0Mc* mc_v0 = (StV0Mc*) (*mcArray)[j];
      mc_v0->SetEvent(ev);
    }
  }

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
  Int_t j=0;
  for (Int_t i=0; i<entries; i++) {
    StV0Vertex* v0Vertex = v0Vertices[i];
    if ((v0Vertex) && (v0Vertex->dcaParentToPrimaryVertex() >= 0))
      new((*dataArray)[j++]) StV0MuDst(v0Vertex,ev);
  }
  entries = j;
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
  if (!((assocMaker)&&(theMcV0Map)&&(theMcTrackMap))) return kStOk;
  StStrangeEvMuDst* ev = masterMaker->GetMcEvent();
  StMcTrack *Pos = 0; 
  StMcTrack *Neg = 0;
  const StV0Vertex* rcV0Partner = 0;
  Int_t indexRecoArray = -1;
  Int_t count = theMcV0Map->count(mcVert);
  
  if (count>0) {
    pair<mcV0MapIter,mcV0MapIter> mcV0Bounds = theMcV0Map->equal_range(mcVert);
    rcV0Partner = (*mcV0Bounds.first).second;
    float x, y, z, delta, xd, yd, zd;
    x = mcVert->position().x();
    y = mcVert->position().y();
    z = mcVert->position().z();
    xd = x - rcV0Partner->position().x();
    yd = y - rcV0Partner->position().y();
    zd = z - rcV0Partner->position().z();
    delta = xd*xd + yd*yd + zd*zd;

    //Now loop over the bounds      
    for(mcV0MapIter mcV0MapIt = mcV0Bounds.first;
                    mcV0MapIt != mcV0Bounds.second; ++mcV0MapIt) {
      const StV0Vertex *temp = (*mcV0MapIt).second;
      if (temp != rcV0Partner) {
        xd = x - temp->position().x();
        yd = y - temp->position().y();
        zd = z - temp->position().z();
        float delta2 = xd*xd + yd*yd + zd*zd;
        if (delta2 < delta) { rcV0Partner = temp; delta = delta2; }
      }
    }
    x = rcV0Partner->position().x();
    y = rcV0Partner->position().y();
    z = rcV0Partner->position().z();
    // stupid way
    for(Int_t i = 0; i < GetN(); i++) {
      StV0MuDst* tmpV0 = (StV0MuDst*) dataArray->At(i);
      if( fabs(x - tmpV0->decayVertexV0X()) < 0.00001 &&
          fabs(y - tmpV0->decayVertexV0Y()) < 0.00001 &&
          fabs(z - tmpV0->decayVertexV0Z()) < 0.00001 )
      { indexRecoArray = i; break; }
    }
  }

  StSPtrVecMcTrack& Daughters = mcVert->daughters();
  for (StMcTrackIterator DTrackIt = Daughters.begin();
       DTrackIt != Daughters.end(); DTrackIt++) {
    switch ((Int_t)(*DTrackIt)->particleDefinition()->charge()) {
    case ( 1) : // Positive
      Pos = (*DTrackIt); break;
    case (-1) : // Negative
      Neg = (*DTrackIt); break;
    }
  }

  if ((Pos)&&(Neg)) {
    StV0Mc* v0Mc = new((*mcArray)[mcEntries++]) StV0Mc(mcVert,Pos,Neg,ev);
    if((assocMaker)&&(indexRecoArray!=-1)) {
      new((*assocArray)[assocEntries++]) 
            StStrangeAssoc(indexRecoArray,mcEntries-1);

      pair<mcTrackMapIter,mcTrackMapIter> mcTrackBounds = 
            theMcTrackMap->equal_range(Pos);
      StTrackPairInfo*   bestPairInfo = (*mcTrackBounds.first).second;
      {for(mcTrackMapIter mcMapIt = mcTrackBounds.first;
		         mcMapIt != mcTrackBounds.second; ++mcMapIt) {
        if ((*mcMapIt).second->commonTpcHits() > bestPairInfo->commonTpcHits())
	       bestPairInfo = (*mcMapIt).second;
      }}
      if (mcTrackBounds.first != mcTrackBounds.second) {
        v0Mc->SetHitInfoPositive(bestPairInfo->commonTpcHits());
      }

      //		   pair<mcTrackMapIter,mcTrackMapIter> 
      mcTrackBounds = theMcTrackMap->equal_range(Neg);
      //		   StTrackPairInfo*   
      bestPairInfo = (*mcTrackBounds.first).second;
			
      {for(mcTrackMapIter mcMapIt = mcTrackBounds.first;
		         mcMapIt != mcTrackBounds.second; ++mcMapIt) {
        if ((*mcMapIt).second->commonTpcHits() > bestPairInfo->commonTpcHits())
              bestPairInfo = (*mcMapIt).second;
      }}
      if (mcTrackBounds.first != mcTrackBounds.second) {
        v0Mc->SetHitInfoNegative(bestPairInfo->commonTpcHits());
      }
    }
  }
  
  return kStOK;
}
//_____________________________________________________________________________
ClassImp(StV0Controller)
