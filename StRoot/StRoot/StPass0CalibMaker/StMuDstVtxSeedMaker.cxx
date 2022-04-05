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
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StBTofHeader.h"
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
 
  int result = kStOk;
  for (pvn=0; pvn<mudst->numberOfPrimaryVertices(); pvn++) {
    result = StVertexSeedMaker::Make();
    if (result != kStOk) break;
  }

  return result;
}
//_____________________________________________________________________________
bool StMuDstVtxSeedMaker::CheckTriggers() {
  bool notTrig = kTRUE;

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
int StMuDstVtxSeedMaker::GetEventData() {
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
  timeEvent = event->eventInfo().time();

  StThreeVectorF pvert = primVtx->position();
  StThreeVectorF epvert = primVtx->posError();
  zvertex = pvert.z();
  yvertex = pvert.y();
  xvertex = pvert.x();
  eyvertex = epvert.y();
  exvertex = epvert.x();

  StBTofHeader* btofHeader = mudst->btofHeader();
  vpd_zvertex = (btofHeader ? btofHeader->vpdVz() : -999);

  mudst->setVertexIndex(pvn);
  rank = primVtx->ranking();

  // Number of good primary tracks for this vertex
  mult = mudst->numberOfPrimaryTracks();

  // primary vertex class doesn't store track count with HFT hits...
  // ...find it ourselves
  hmatch = 0;
  for (int trkn=0; trkn<mult; trkn++) {
    StMuTrack* primTrk = mudst->primaryTracks(trkn);
    if (primTrk->nHitsFit(kPxlId) +
        primTrk->nHitsFit(kIstId) +
        primTrk->nHitsFit(kSsdId))
      hmatch++;
  }

  // hits not saved in MuDst
  itpc = 0; otpc = 0;

  //detmap will store number of matches in other detectors
  detmap = 0;

  // cap at 7 in detmap (bits 0,1,2)
  Packer( 0,3,bmatch,primVtx->nBEMCMatch());

  // cap at 7 in detmap (bits 3,4,5)
  Packer( 3,3,ematch,primVtx->nEEMCMatch());

  // cap at 7 in detmap (bits 6,7,8)
  Packer( 6,3,tmatch,primVtx->nBTOFMatch());

  // cap at 3 in detmap (bits 9,10)
  Packer( 9,2,cmatch,primVtx->nCrossCentralMembrane());

  // cap at 7 in detmap (bits 11,12,13)
  Packer(11,3,hmatch,hmatch);

  // cap at 3 in detmap (bits 14,15)
  Packer(14,2,pmatch,primVtx->nPromptTracks());

  // cap at 3 in detmap (bits 16,17,18)
  Packer(16,3,pct   ,primVtx->nPostXtracks());

  return kStOk;
}
//_____________________________________________________________________________
void StMuDstVtxSeedMaker::PrintInfo() {
  LOG_INFO << "\n**************************************************************"
           << "\n* $Id: StMuDstVtxSeedMaker.cxx,v 1.15 2016/08/02 21:17:17 genevb Exp $"
           << "\n**************************************************************" << endm;

  if (Debug()) StVertexSeedMaker::PrintInfo();
}
//_____________________________________________________________________________
// $Id: StMuDstVtxSeedMaker.cxx,v 1.15 2016/08/02 21:17:17 genevb Exp $
// $Log: StMuDstVtxSeedMaker.cxx,v $
// Revision 1.15  2016/08/02 21:17:17  genevb
// Added tDay,tFill to resNtuple, and improved C++11 compliance
//
// Revision 1.14  2015/05/18 21:25:47  genevb
// Use HFT hits
//
// Revision 1.13  2015/05/15 05:38:21  genevb
// Include prompt hits and post-crossing tracks, simplify detmap packing, update doxygen documentation
//
// Revision 1.12  2015/05/14 20:29:25  genevb
// Add z of VPD vertex
//
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
