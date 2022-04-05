// $Id: StKinkController.cxx,v 3.7 2011/04/03 15:51:58 fisyak Exp $
// $Log: StKinkController.cxx,v $
// Revision 3.7  2011/04/03 15:51:58  fisyak
// Fix effect of constness in StAssociationMaker
//
// Revision 3.6  2002/06/13 16:06:01  genevb
// Additional security against zombies in StEvent vectors
//
// Revision 3.5  2002/04/30 16:02:47  genevb
// Common muDst, improved MC code, better kinks, StrangeCuts now a branch
//
// Revision 3.4  2000/12/18 21:35:17  genevb
// Introduced variable buffer-sizing
//
// Revision 3.3  2000/09/18 19:25:19  genevb
// Additional protection for missing MC info
//
// Revision 3.2  2000/07/17 20:28:40  genevb
// File size limitation workaround, some under the hood improvements
//
// Revision 3.1  2000/07/14 21:28:34  genevb
// Added V0Mc index for XiMc, fixed bug with entries for XiMc, cleaned up controllers
//
// Revision 3.0  2000/07/14 12:56:47  genevb
// Revision 3 has event multiplicities and dedx information for vertex tracks
//
// Revision 2.1  2000/06/09 22:17:09  genevb
// Allow MC data to be copied between DSTs, other small improvements
//
// Revision 2.0  2000/06/05 05:19:39  genevb
// New version of Strangeness micro DST package
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StKinkController strangeness micro DST controller for Kinks          //
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
#include "StTrackDetectorInfo.h"

#include "StStrangeControllerInclude.h"  // Location of header for this class

//_____________________________________________________________________________
StKinkController::StKinkController() : StStrangeControllerBase(kinkT) {
  increment = 100;
  max = 500;
  bsize = 128000;
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
  Int_t j=0;
  for (Int_t i=0; i<entries; i++) {
    StKinkVertex* kinkVertex = kinkVertices[i];
    if (kinkVertex)
      new((*dataArray)[j++]) StKinkMuDst(kinkVertex);
  }
  entries = j;
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
  if (!((assocMaker)&&(theMcKinkMap)&&(theMcTrackMap))) return kStOk;
  const StKinkVertex* rcKinkPartner = 0;
  StMcTrack* Daughter = 0;
  Int_t indexRecoArray = -1;
  Int_t count = theMcKinkMap->count(mcVert);
  StSPtrVecMcTrack& Daughters = mcVert->daughters();	
  
  for (StMcTrackIterator DTrackIt = Daughters.begin();
                         DTrackIt != Daughters.end(); DTrackIt++) {
    if ((Int_t)(*DTrackIt)->particleDefinition()->charge()) {
      Daughter = (*DTrackIt);
      break;
    }
  }

  if (Daughter) {
    StKinkMc* kinkMc = new((*mcArray)[mcEntries++]) StKinkMc(mcVert,Daughter);
    if (count>0) {
      pair<mcKinkMapIter,mcKinkMapIter> mcKinkBounds =
            theMcKinkMap->equal_range(mcVert);
      indexRecoArray = -1;

      rcKinkPartner = (*mcKinkBounds.first).second;
      float x, y, z, delta, xd, yd, zd;
      x = mcVert->position().x();
      y = mcVert->position().y();
      z = mcVert->position().z();
      xd = x - rcKinkPartner->position().x();
      yd = y - rcKinkPartner->position().y();
      zd = z - rcKinkPartner->position().z();
      delta = xd*xd + yd*yd + zd*zd;

      //Now loop over the bounds      
      for(mcKinkMapIter mcKinkMapIt = mcKinkBounds.first;
                      mcKinkMapIt != mcKinkBounds.second; ++mcKinkMapIt) {
        const StKinkVertex *temp = (*mcKinkMapIt).second;
        if (temp != rcKinkPartner) {
          xd = x - temp->position().x();
          yd = y - temp->position().y();
          zd = z - temp->position().z();
          float delta2 = xd*xd + yd*yd + zd*zd;
          if (delta2 < delta) { rcKinkPartner = temp; delta = delta2; }
        }
      }
      x = rcKinkPartner->position().x();
      y = rcKinkPartner->position().y();
      z = rcKinkPartner->position().z();
      // stupid way
      for(Int_t i = 0; i < GetN(); i++) {
        StKinkMuDst* tmpKink = (StKinkMuDst*) dataArray->At(i);
        if( fabs(x - tmpKink->positionX()) < 0.00001 &&
            fabs(y - tmpKink->positionY()) < 0.00001 &&
            fabs(z - tmpKink->positionZ()) < 0.00001 )
        { indexRecoArray = i; break; }
      }
      new((*assocArray)[assocEntries++]) 
		    StStrangeAssoc(indexRecoArray,mcEntries-1);
      if(indexRecoArray!=-1) {
        pair<mcTrackMapIter,mcTrackMapIter> mcTrackBounds = 
              theMcTrackMap->equal_range(Daughter);
        StTrackPairInfo*   bestPairInfo = (*mcTrackBounds.first).second;
        for(mcTrackMapIter mcMapIt = mcTrackBounds.first;
                           mcMapIt != mcTrackBounds.second; ++mcMapIt) {
          if ((*mcMapIt).second->commonTpcHits() > bestPairInfo->commonTpcHits())
	         bestPairInfo = (*mcMapIt).second;
        } 
        if (mcTrackBounds.first != mcTrackBounds.second) {
          kinkMc->SetHitInfo(bestPairInfo->commonTpcHits());
        }
      }
    }
  }
  
  return kStOK;
}
//_____________________________________________________________________________
ClassImp(StKinkController)
