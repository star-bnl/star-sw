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
#include "StDetectorDbMaker/St_tpcPadConfigC.h"


ClassImp(StEvtVtxSeedMaker)
//_____________________________________________________________________________
StEvtVtxSeedMaker::StEvtVtxSeedMaker(const char *name):
  StVertexSeedMaker(name), event(0) {
}
//_____________________________________________________________________________
Int_t StEvtVtxSeedMaker::Make() {
  event = static_cast<StEvent*>(GetInputDS("StEvent"));
  if (!event) {
    gMessMgr->Error("StEvtVtxSeedMaker: No StEvent found!");
    return kStErr;
  }
 
  int result = kStOk;
  for (pvn=0; pvn<event->numberOfPrimaryVertices(); pvn++) {
    result = StVertexSeedMaker::Make();
    if (result != kStOk) break;
  }

  return result;
}
//_____________________________________________________________________________
bool StEvtVtxSeedMaker::CheckTriggers() {
  bool notTrig = kTRUE;

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
int StEvtVtxSeedMaker::GetEventData() {
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
  timeEvent = event->time();

  StThreeVectorF pvert = primVtx->position();
  StThreeVectorF epvert = primVtx->positionError();
  zvertex = pvert.z();
  yvertex = pvert.y();
  xvertex = pvert.x();
  eyvertex = epvert.y();
  exvertex = epvert.x();

  rank = primVtx->ranking();
  mult = 0;
  hmatch = 0;
  itpc = 0; otpc = 0;
  for (unsigned int trkn=0; trkn<primVtx->numberOfDaughters(); trkn++) {
    StTrack* primTrk = primVtx->daughter(trkn);
    if (!(primTrk->bad())) {
      // Number of good primary tracks for this vertex
      mult++;
      // primary vertex class doesn't store track count with HFT hits...
      // ...find it ourselves
      const StTrackFitTraits& fitTraits = primTrk->fitTraits();
      if (fitTraits.numberOfFitPoints(kPxlId) +
          fitTraits.numberOfFitPoints(kIstId) +
          fitTraits.numberOfFitPoints(kSsdId))
        hmatch++;
    }
    // Determine TPC sub-sectors of tracks associated with this vertex
    // pack into bits 0..23
    StPtrVecHit hits = primTrk->detectorInfo()->hits(kTpcId);
    for (unsigned int hitn=0; hitn<hits.size(); hitn++) {
      StTpcHit* hit = (StTpcHit*) (hits[hitn]);
      // TPC padrow and sector indices use 1..n
      int mask = 1<<(hit->sector()-1);
      if (hit->padrow() <= St_tpcPadConfigC::instance()->innerPadRows(hit->sector())) itpc |= mask;
      else otpc |= mask;
    }
  }

  const StBTofCollection* btofColl = event->btofCollection();
  const StBTofHeader* btofHeader = (btofColl ? btofColl->tofHeader() : 0);
  vpd_zvertex = (btofHeader ? btofHeader->vpdVz() : -999);

  //detmap will store number of matches in other detectors
  detmap = 0;

  // cap at 7 in detmap (bits 0,1,2)
  Packer( 0,3,bmatch,primVtx->numMatchesWithBEMC());

  // cap at 7 in detmap (bits 3,4,5)
  Packer( 3,3,ematch,primVtx->numMatchesWithEEMC());

  // cap at 7 in detmap (bits 6,7,8)
  Packer( 6,3,tmatch,primVtx->numMatchesWithBTOF());

  // cap at 3 in detmap (bits 9,10)
  Packer( 9,2,cmatch,primVtx->numTracksCrossingCentralMembrane());

  // cap at 7 in detmap (bits 11,12,13)
  Packer(11,3,hmatch,hmatch);

  // cap at 3 in detmap (bits 14,15)
  Packer(14,2,pmatch,primVtx->numTracksWithPromptHit());

  // cap at 3 in detmap (bits 16,17,18)
  Packer(16,3,pct   ,primVtx->numPostXTracks());

  return kStOk;
}
//_____________________________________________________________________________
void StEvtVtxSeedMaker::PrintInfo() {
  LOG_INFO << "\n**************************************************************"
           << "\n* $Id: StEvtVtxSeedMaker.cxx,v 1.15 2018/04/11 02:43:21 smirnovd Exp $"
           << "\n**************************************************************" << endm;

  if (Debug()) StVertexSeedMaker::PrintInfo();
}
//_____________________________________________________________________________
// $Id: StEvtVtxSeedMaker.cxx,v 1.15 2018/04/11 02:43:21 smirnovd Exp $
// $Log: StEvtVtxSeedMaker.cxx,v $
// Revision 1.15  2018/04/11 02:43:21  smirnovd
// Enable TPC/iTPC switch via St_tpcPadConfig
//
// This is accomplished by substituting St_tpcPadPlanes with St_tpcPadConfig.
// A sector ID is passed to St_tpcPadConfig in order to extract parameters for
// either TPC or iTPC
//
// Revision 1.14  2016/08/02 21:17:16  genevb
// Added tDay,tFill to resNtuple, and improved C++11 compliance
//
// Revision 1.13  2015/05/19 19:36:09  genevb
// Code cleanup in preparation for C++11
//
// Revision 1.12  2015/05/18 21:25:31  genevb
// Use HFT hits, some streamlining of for-loops
//
// Revision 1.11  2015/05/15 05:38:21  genevb
// Include prompt hits and post-crossing tracks, simplify detmap packing, update doxygen documentation
//
// Revision 1.10  2015/05/14 20:29:25  genevb
// Add z of VPD vertex
//
// Revision 1.9  2013/08/14 21:42:48  genevb
// Introduce time offsets, noclobber toggle, more matched-tracks controls
//
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
