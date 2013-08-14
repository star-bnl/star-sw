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
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
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
 
  Int_t result = kStOk;
  for (pvn=0; pvn<mudst->numberOfPrimaryVertices(); pvn++) {
    result = StVertexSeedMaker::Make();
    if (result != kStOk) break;
  }

  return result;
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
  StMuPrimaryVertex* primVtx = mudst->primaryVertex(pvn);
  if (!primVtx) {
    gMessMgr->Error("StMuDstVtxSeedMaker: No primary vertex from StMuDst!");
    return kStErr;
  }
  StRunInfo& runInfo = event->runInfo();
  zdc = (float) (runInfo.zdcCoincidenceRate());
  fill = (int) (runInfo.beamFillNumber(blue));
  run = runInfo.runId();

  StThreeVectorF pvert = primVtx->position();
  StThreeVectorF epvert = primVtx->posError();
  zvertex = pvert.z();
  yvertex = pvert.y();
  xvertex = pvert.x();
  eyvertex = epvert.y();
  exvertex = epvert.x();

  // Number of good primary tracks for this vertex
  mudst->setVertexIndex(pvn);
  mult = mudst->numberOfPrimaryTracks();
  rank = primVtx->ranking();

  // hits not saved in MuDst
  itpc = 0; otpc = 0; detmap = 0;

  //detmap will store number of matches in other detectors

  unsigned short nBEMC = primVtx->nBEMCMatch();
  bmatch = nBEMC;
  if (nBEMC>7) nBEMC=7; // 7 should be enough to convince
  // pack into bits 0,1,2
  detmap += nBEMC;

  unsigned short nEEMC = primVtx->nEEMCMatch();
  ematch = nEEMC;
  if (nEEMC>7) nEEMC=7; // 7 should be enough to convince
  // pack into bits 3,4,5
  detmap += 8*nEEMC;

  unsigned short nBTOF = primVtx->nBTOFMatch();
  tmatch = nBTOF;
  if (nBTOF>7) nBTOF=7; // 7 should be enough to convince
  // pack into bits 6,7,8
  detmap += 64*nBTOF;

  unsigned short nCRCM = primVtx->nCrossCentralMembrane();
  cmatch = nCRCM;
  if (nCRCM>3) nCRCM=3; // 3 should be enough to convince
  // pack into bits 9,10
  detmap += 512*nCRCM;

  hmatch = 0; // reserved for HFT matches

  return kStOk;
}
//_____________________________________________________________________________
void StMuDstVtxSeedMaker::PrintInfo() {
  LOG_INFO << "\n**************************************************************"
           << "\n* $Id: StMuDstVtxSeedMaker.cxx,v 1.11 2013/08/14 21:42:48 genevb Exp $"
           << "\n**************************************************************" << endm;

  if (Debug()) StVertexSeedMaker::PrintInfo();
}
//_____________________________________________________________________________
// $Id: StMuDstVtxSeedMaker.cxx,v 1.11 2013/08/14 21:42:48 genevb Exp $
// $Log: StMuDstVtxSeedMaker.cxx,v $
// Revision 1.11  2013/08/14 21:42:48  genevb
// Introduce time offsets, noclobber toggle, more matched-tracks controls
//
// Revision 1.10  2012/08/15 00:08:09  genevb
// ZDC sum rate -> ZDC coincidence rate
//
// Revision 1.9  2012/08/14 23:56:06  genevb
// detmap now includes BEMC+EEMC+BTOF+CM, added mean zdc to log output
//
// Revision 1.8  2009/06/12 17:09:17  genevb
// Match mult for MuDst and StEvent
//
// Revision 1.7  2009/05/22 23:50:50  genevb
// Code mods for BEMC matches, BeamWidth
//
// Revision 1.6  2008/05/21 17:48:39  genevb
// Use vertex errors for weighting
//
// Revision 1.5  2007/05/16 02:59:25  genevb
// printf => LOG_INFO
//
// Revision 1.4  2006/09/01 22:27:16  genevb
// More detailed info in ntuple
//
// Revision 1.3  2005/07/01 21:46:59  genevb
// Remove extraneous print statement
//
// Revision 1.2  2005/07/01 21:46:01  genevb
// Specify output directory
//
// Revision 1.1  2005/06/14 18:52:20  genevb
// Introduction of code to use MuDst for beamline constraint
//
//
