//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StPicoDstVtxSeedMaker class                                            //
// Author: G. Van Buren, BNL                                            //
// Description: calculates mean primary vertex positions from           //
//              suitable events to use as seeds in finding better       //
//              primary vertex positions (helpful for low               //
//              multiplicity events like pp collisions)                 //
//              using PicoDst                                             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StPicoDstVtxSeedMaker.h"
#include "StPicoDstMaker/StPicoDstMaker.h"
#include "StPicoEvent/StPicoDst.h"
#include "StPicoEvent/StPicoEvent.h"
#include "StPicoEvent/StPicoTrack.h"
#include "StMessMgr.h"


ClassImp(StPicoDstVtxSeedMaker)
//_____________________________________________________________________________
StPicoDstVtxSeedMaker::StPicoDstVtxSeedMaker(const char *name):
  StVertexSeedMaker(name), picodst(0), event(0) {
}
//_____________________________________________________________________________
Int_t StPicoDstVtxSeedMaker::Make() {
  StPicoDstMaker* picodstMaker = (StPicoDstMaker*) GetMaker("PicoDst");
  if (!picodstMaker) {
    gMessMgr->Error("StPicoDstVtxSeedMaker: No StPicoDstMaker(\"PicoDst\") found!");
    return kStErr;
  }
  picodst = picodstMaker->picoDst();
  if (!picodst) {
    gMessMgr->Error("StPicoDstVtxSeedMaker: No PicoDst found!");
    return kStErr;
  }
  event = picodst->event();
  if (!event) {
    gMessMgr->Error("StPicoDstVtxSeedMaker: No PicoEvent found!");
    return kStErr;
  }
 
  return StVertexSeedMaker::Make();
}
//_____________________________________________________________________________
bool StPicoDstVtxSeedMaker::CheckTriggers() {
  bool notTrig = kTRUE;

  // Check trigger ids
  vector<unsigned int> idVec = event->triggerIds();
  for (unsigned int iTrg = 0;
       (notTrig) && (iTrg < idVec.size()) ; iTrg++) {
    if (ValidTrigger(idVec[iTrg])) notTrig = kFALSE;
  }
  return notTrig;
}
//_____________________________________________________________________________
int StPicoDstVtxSeedMaker::GetEventData() {
  // Get primary vertex from MuEvent
  zdc = event->ZDCx();
  fill = event->fillId();
  run = event->runId();
  // timeEvent not saved in PicoDst

  TVector3 pvert = event->primaryVertex();
  TVector3 epvert = event->primaryVertexError();
  zvertex = pvert.z();
  yvertex = pvert.y();
  xvertex = pvert.x();
  eyvertex = epvert.y();
  exvertex = epvert.x();

  vpd_zvertex = event->vzVpd();

  rank = event->ranking();

  // Number of good primary tracks for this vertex
  mult = 0;

  // picodst doesn't store track count with HFT hits...
  // ...find it ourselves
  hmatch = 0;
  for (int trkn=0; trkn<(int) picodst->numberOfTracks(); trkn++) {
    StPicoTrack* primTrk = picodst->track(trkn);
    if (! primTrk->isPrimary()) continue;
    mult++;
    if (primTrk->isHft()) hmatch++;
  }

  // hits not saved in PicoDst
  itpc = 0; otpc = 0;

  //detmap will store number of matches in other detectors
  detmap = 0;

  // cap at 7 in detmap (bits 0,1,2)
  Packer( 0,3,bmatch,event->nBEMCMatch());

  // cap at 7 in detmap (bits 3,4,5)
  Packer( 3,3,ematch,0);

  // cap at 7 in detmap (bits 6,7,8)
  Packer( 6,3,tmatch,event->nBTOFMatch());

  // cap at 3 in detmap (bits 9,10)
  Packer( 9,2,cmatch,0);

  // cap at 7 in detmap (bits 11,12,13)
  Packer(11,3,hmatch,hmatch);

  // cap at 3 in detmap (bits 14,15)
  Packer(14,2,pmatch,0);

  // cap at 3 in detmap (bits 16,17,18)
  Packer(16,3,pct   ,0);

  return kStOk;
}
//_____________________________________________________________________________
void StPicoDstVtxSeedMaker::PrintInfo() {
  LOG_INFO << "\n**************************************************************"
           << "\n* $Id: StPicoDstVtxSeedMaker.cxx,v 1.3 2018/08/02 04:08:09 genevb Exp $"
           << "\n**************************************************************" << endm;

  if (Debug()) StVertexSeedMaker::PrintInfo();
}
//_____________________________________________________________________________
// $Id: StPicoDstVtxSeedMaker.cxx,v 1.3 2018/08/02 04:08:09 genevb Exp $
// $Log: StPicoDstVtxSeedMaker.cxx,v $
// Revision 1.3  2018/08/02 04:08:09  genevb
// Changed return type in StPicoEvent
//
// Revision 1.2  2018/04/04 11:01:39  jeromel
// Changed location pf include
//
// Revision 1.1  2017/08/08 03:58:20  genevb
// Add vertex-seed-finding with picoDsts
//
//
//
