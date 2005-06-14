//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StEvtVtxSeedMaker class                                              //
// Author: G. Van Buren, BNL                                            //
// Description: calculates mean primary vertex positions from           //
//              suitable events to use as seeds in finding better       //
//              primary vertex positions (helpful for low               //
//              multiplicity events like pp collisions)                 //
//              using StEvent                                           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StEvtVtxSeedMaker.h"
#include "StEventTypes.h"
#include "StMessMgr.h"


ClassImp(StEvtVtxSeedMaker)
//_____________________________________________________________________________
StEvtVtxSeedMaker::StEvtVtxSeedMaker(const char *name):
  StVertexSeedMaker(name), event(0) {
}
//_____________________________________________________________________________
Int_t StEvtVtxSeedMaker::Make() {
  event = (StEvent*) GetInputDS("StEvent");
  if (!event) {
    gMessMgr->Error("StEvtVtxSeedMaker: No StEvent found!");
    return kStErr;
  }
 
  return StVertexSeedMaker::Make();
}
//_____________________________________________________________________________
Bool_t StEvtVtxSeedMaker::CheckTriggers() {
  Bool_t notTrig = kTRUE;

  // Check trigger ids
  StTriggerIdCollection* trigIdColl = event->triggerIdCollection();
  if (trigIdColl) {
    const StTriggerId* tr = trigIdColl->nominal();
    if (tr) {
      vector<unsigned int> idVec = tr->triggerIds();
      for (unsigned int iTrg = 0;
           (notTrig) && (iTrg < idVec.size()) ; iTrg++) {
        if (ValidTrigger(idVec[iTrg])) notTrig = kFALSE;
      }
    }
  }
  return notTrig;
}
//_____________________________________________________________________________
Int_t StEvtVtxSeedMaker::GetEventData() {
  // Get primary vertex from StEvent
  StPrimaryVertex* primVtx = event->primaryVertex();
  if (!primVtx) {
    gMessMgr->Error("StEvtVtxSeedMaker: No primary vertex from StEvent!");
    return kStErr;
  }

  StThreeVectorF pvert = primVtx->position();
  zvertex = pvert.z();
  yvertex = pvert.y();
  xvertex = pvert.x();
  mult = (float)(primVtx->numberOfDaughters());

  return kStOk;
}
//_____________________________________________________________________________
void StEvtVtxSeedMaker::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StEvtVtxSeedMaker.cxx,v 1.1 2005/06/14 18:52:04 genevb Exp $\n");
  printf("**************************************************************\n");

  if (Debug()) StVertexSeedMaker::PrintInfo();
}
//_____________________________________________________________________________
// $Id: StEvtVtxSeedMaker.cxx,v 1.1 2005/06/14 18:52:04 genevb Exp $
// $Log: StEvtVtxSeedMaker.cxx,v $
// Revision 1.1  2005/06/14 18:52:04  genevb
// Introduction of code to use StEvent for beamline constraint
//
//
