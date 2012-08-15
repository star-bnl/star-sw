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
 
  Int_t result = kStOk;
  for (pvn=0; pvn<event->numberOfPrimaryVertices(); pvn++) {
    result = StVertexSeedMaker::Make();
    if (result != kStOk) break;
  }

  return result;
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
  StPrimaryVertex* primVtx = event->primaryVertex(pvn);
  if (!primVtx) {
    gMessMgr->Error("StEvtVtxSeedMaker: No primary vertex from StEvent!");
    return kStErr;
  }
  StRunInfo* runInfo = event->runInfo();
  if (runInfo) {
    zdc = (float) (runInfo->zdcCoincidenceRate());
    fill = (int) (runInfo->beamFillNumber(blue));
    run = runInfo->runId();
  }

  StThreeVectorF pvert = primVtx->position();
  StThreeVectorF epvert = primVtx->positionError();
  zvertex = pvert.z();
  yvertex = pvert.y();
  xvertex = pvert.x();
  eyvertex = epvert.y();
  exvertex = epvert.x();
  // Number of good primary tracks for this vertex
  mult = 0;
  for (unsigned int trkn=0; trkn<primVtx->numberOfDaughters(); trkn++)
    if (!(primVtx->daughter(trkn)->bad())) mult++;
  rank = primVtx->ranking();

  // Determine sub-sectors of tracks associated with this vertex
  itpc = 0; otpc = 0; detmap = 0;
  bool ibits[24];
  bool obits[24];
  unsigned int hitn,trkn;
  for (trkn=0; trkn<24; trkn++) { ibits[trkn] = false; obits[trkn] = false; }
  for (trkn=0; trkn<primVtx->numberOfDaughters(); trkn++) {
    StTrack* trk = primVtx->daughter(trkn);
    StPtrVecHit hits = trk->detectorInfo()->hits(kTpcId);
    for (hitn=0; hitn<hits.size(); hitn++) {
      StTpcHit* hit = (StTpcHit*) (hits[hitn]);
      if (hit->padrow() < 14) ibits[hit->sector()-1] = true;
      else obits[hit->sector()-1] = true;
    }
  }
  // pack into bits 0..23
  for (trkn=0; trkn<24; trkn++) {
    if (ibits[trkn]) itpc += (int) (::pow(2,trkn));
    if (obits[trkn]) otpc += (int) (::pow(2,trkn));
  }

  //detmap will store number of matches in other detectors

  unsigned short nBEMC = primVtx->numMatchesWithBEMC();
  if (nBEMC>7) nBEMC=7; // 7 should be enough to convince
  // pack into bits 0,1,2
  detmap += nBEMC;

  unsigned short nEEMC = primVtx->numMatchesWithEEMC();
  if (nEEMC>7) nEEMC=7; // 7 should be enough to convince
  // pack into bits 3,4,5
  detmap += 8*nEEMC;

  unsigned short nBTOF = primVtx->numMatchesWithBTOF();
  if (nBTOF>7) nBTOF=7; // 7 should be enough to convince
  // pack into bits 6,7,8
  detmap += 64*nBTOF;

  unsigned short nCRCM = primVtx->numTracksCrossingCentralMembrane();
  if (nCRCM>3) nCRCM=3; // 3 should be enough to convince
  // pack into bits 9,10
  detmap += 512*nCRCM;

  return kStOk;
}
//_____________________________________________________________________________
void StEvtVtxSeedMaker::PrintInfo() {
  LOG_INFO << "\n**************************************************************"
           << "\n* $Id: StEvtVtxSeedMaker.cxx,v 1.8 2012/08/15 00:08:09 genevb Exp $"
           << "\n**************************************************************" << endm;

  if (Debug()) StVertexSeedMaker::PrintInfo();
}
//_____________________________________________________________________________
// $Id: StEvtVtxSeedMaker.cxx,v 1.8 2012/08/15 00:08:09 genevb Exp $
// $Log: StEvtVtxSeedMaker.cxx,v $
// Revision 1.8  2012/08/15 00:08:09  genevb
// ZDC sum rate -> ZDC coincidence rate
//
// Revision 1.7  2012/08/14 23:56:06  genevb
// detmap now includes BEMC+EEMC+BTOF+CM, added mean zdc to log output
//
// Revision 1.6  2009/06/12 17:09:17  genevb
// Match mult for MuDst and StEvent
//
// Revision 1.5  2009/05/22 23:50:50  genevb
// Code mods for BEMC matches, BeamWidth
//
// Revision 1.4  2008/05/21 17:48:38  genevb
// Use vertex errors for weighting
//
// Revision 1.3  2007/05/16 02:59:25  genevb
// printf => LOG_INFO
//
// Revision 1.2  2006/09/01 22:27:16  genevb
// More detailed info in ntuple
//
// Revision 1.1  2005/06/14 18:52:04  genevb
// Introduction of code to use StEvent for beamline constraint
//
//
