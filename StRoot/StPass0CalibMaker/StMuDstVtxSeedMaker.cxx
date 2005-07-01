//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMuDstVtxSeedMaker class                                            //
// Author: G. Van Buren, BNL                                            //
// Description: calculates mean primary vertex positions from           //
//              suitable events to use as seeds in finding better       //
//              primary vertex positions (helpful for low               //
//              multiplicity events like pp collisions)                 //
//              using MuDst                                             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StMuDstVtxSeedMaker.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMessMgr.h"


ClassImp(StMuDstVtxSeedMaker)
//_____________________________________________________________________________
StMuDstVtxSeedMaker::StMuDstVtxSeedMaker(const char *name):
  StVertexSeedMaker(name), mudst(0), event(0) {
}
//_____________________________________________________________________________
Int_t StMuDstVtxSeedMaker::Make() {
  StMuDstMaker* mudstMaker = (StMuDstMaker*) GetMaker("MuDst");
  if (!mudstMaker) {
    gMessMgr->Error("StMuDstVtxSeedMaker: No StMuDstMaker(\"MuDst\") found!");
    return kStErr;
  }
  mudst = mudstMaker->muDst();
  if (!mudst) {
    gMessMgr->Error("StMuDstVtxSeedMaker: No MuDst found!");
    return kStErr;
  }
  event = mudst->event();
  if (!event) {
    gMessMgr->Error("StMuDstVtxSeedMaker: No MuEvent found!");
    return kStErr;
  }
 
  return StVertexSeedMaker::Make();
}
//_____________________________________________________________________________
Bool_t StMuDstVtxSeedMaker::CheckTriggers() {
  Bool_t notTrig = kTRUE;

  // Check trigger ids
  StMuTriggerIdCollection& trigIdColl = event->triggerIdCollection();
  const StTriggerId& tr = trigIdColl.nominal();
  vector<unsigned int> idVec = tr.triggerIds();
  for (unsigned int iTrg = 0;
       (notTrig) && (iTrg < idVec.size()) ; iTrg++) {
    if (ValidTrigger(idVec[iTrg])) notTrig = kFALSE;
  }
  return notTrig;
}
//_____________________________________________________________________________
Int_t StMuDstVtxSeedMaker::GetEventData() {
  // Get primary vertex from MuEvent
  StThreeVectorF pvert = event->primaryVertexPosition();
  zvertex = pvert.z();
  yvertex = pvert.y();
  xvertex = pvert.x();

/*
  Ideally, we would get this from "numberOfGoodPrimaryTracks", but
  this has been dysfunctional since somewhere between P03ih and P04if

  StEventSummary& summ = event->eventSummary();
  mult = (float)(summ.numberOfGoodPrimaryTracks());
*/
  mult = (float) (mudst->primaryTracks()->GetEntries());
  printf("HHHH %f %d\n",mult,(int) (mudst->globalTracks()->GetEntries()));

  return kStOk;
}
//_____________________________________________________________________________
void StMuDstVtxSeedMaker::PrintInfo() {
  printf("**************************************************************\n");
  printf("* $Id: StMuDstVtxSeedMaker.cxx,v 1.2 2005/07/01 21:46:01 genevb Exp $\n");
  printf("**************************************************************\n");

  if (Debug()) StVertexSeedMaker::PrintInfo();
}
//_____________________________________________________________________________
// $Id: StMuDstVtxSeedMaker.cxx,v 1.2 2005/07/01 21:46:01 genevb Exp $
// $Log: StMuDstVtxSeedMaker.cxx,v $
// Revision 1.2  2005/07/01 21:46:01  genevb
// Specify output directory
//
// Revision 1.1  2005/06/14 18:52:20  genevb
// Introduction of code to use MuDst for beamline constraint
//
//
