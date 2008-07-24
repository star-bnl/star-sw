/***********************************************************************
 *
 * $Id: StV0MuDst.cc,v 3.6 2008/07/10 16:16:55 genevb Exp $
 *
 * Authors: Gene Van Buren, UCLA, 24-Mar-2000
 *          Peter G. Jones, University of Birmingham, 04-Jun-1999
 *
 ***********************************************************************
 *
 * Description: V0 micro dst class
 *
 ***********************************************************************
 *
 * $Log: StV0MuDst.cc,v $
 * Revision 3.6  2008/07/10 16:16:55  genevb
 * Allow for marking of bad tracks -> bad secondary vertices
 *
 * Revision 3.5  2001/11/05 23:41:06  genevb
 * Add more dEdx, B field info, careful of changes to TTree unrolling
 *
 * Revision 3.4  2001/05/04 20:15:15  genevb
 * Common interfaces and reorganization of components, add MC event info
 *
 * Revision 3.3  2000/04/25 18:22:49  perev
 * HPcorrs
 *
 * Revision 3.2  2000/08/31 21:25:34  genevb
 * Adjustment for V0s used in Xis only
 *
 * Revision 3.1  2000/08/10 01:16:24  genevb
 * Added number of dedx points
 *
 * Revision 3.0  2000/07/14 12:56:50  genevb
 * Revision 3 has event multiplicities and dedx information for vertex tracks
 *
 * Revision 2.0  2000/06/02 22:11:54  genevb
 * New version of Strangeness micro DST package
 *
 * Revision 1.3  2000/03/31 03:20:24  jones
 * Added topology map to V0/Xi; access funcs for each data member
 *
 * Revision 1.2  2000/03/29 20:52:13  genevb
 * Added StKinkMuDst, replaced arrays
 *
 * Revision 1.1  2000/03/29 03:10:08  genevb
 * Introduction of Strangeness Micro DST package
 *
 *
 ***********************************************************************/
#include "phys_constants.h"
#include "StV0MuDst.hh"
#include "StTrack.h"
#include "StTrackFitTraits.h"
#include "StV0Vertex.h"
#include "StStrangeEvMuDst.hh"
#include "StDedxPidTraits.h"
#include "TMath.h"
ClassImp(StV0MuDst)

StV0MuDst::StV0MuDst() : StV0I() { 
}

void StV0MuDst::Fill(StV0Vertex* v0Vertex,
                       StStrangeEvMuDst* event) {
  mEvent = event;
  mDecayVertexV0X = v0Vertex->position().x();
  mDecayVertexV0Y = v0Vertex->position().y();
  mDecayVertexV0Z = v0Vertex->position().z();
  mDcaV0Daughters = v0Vertex->dcaDaughters();
  mDcaV0ToPrimVertex = TMath::Abs(v0Vertex->dcaParentToPrimaryVertex());
  mDcaPosToPrimVertex = v0Vertex->dcaDaughterToPrimaryVertex(positive);
  mDcaNegToPrimVertex = v0Vertex->dcaDaughterToPrimaryVertex(negative);
  mMomNegX = v0Vertex->momentumOfDaughter(negative).x();
  mMomNegY = v0Vertex->momentumOfDaughter(negative).y();
  mMomNegZ = v0Vertex->momentumOfDaughter(negative).z();
  mMomPosX = v0Vertex->momentumOfDaughter(positive).x();
  mMomPosY = v0Vertex->momentumOfDaughter(positive).y();
  mMomPosZ = v0Vertex->momentumOfDaughter(positive).z();
  mChi2V0 = v0Vertex->chiSquared();
  mClV0 = v0Vertex->probChiSquared();

  StTrack* trk = v0Vertex->daughter(positive);
  mKeyPos = trk->key();
  mTopologyMapPos = trk->topologyMap();
  mChi2Pos = trk->fitTraits().chi2(0);
  mClPos = trk->fitTraits().chi2(1);
  if (trk->bad()) setPosBad();
  mDedxPos = 0.;
  mNumDedxPos = 0;
  // For now, get the truncated mean dE/dX from the TPC
  StPtrVecTrackPidTraits pidPos = trk->pidTraits(kTpcId);
  UInt_t i;
  for (i=0; i<pidPos.size(); i++) {
    StDedxPidTraits* pid = (StDedxPidTraits*) pidPos[i];
    if (pid->method() == kTruncatedMeanId) {
      mDedxPos = pid->mean();
      mErrDedxPos = pid->errorOnMean();
      mNumDedxPos = pid->numberOfPoints() + (100*((int) (pid->length())));
      break;
    }
  }

  trk = v0Vertex->daughter(negative);
  mKeyNeg = trk->key();
  mTopologyMapNeg = trk->topologyMap();
  mChi2Neg = trk->fitTraits().chi2(0);
  mClNeg = trk->fitTraits().chi2(1);
  if (trk->bad()) setNegBad();
  mDedxNeg = 0.;
  mNumDedxNeg = 0;
  // For now, get the truncated mean dE/dX from the TPC
  StPtrVecTrackPidTraits pidNeg = trk->pidTraits(kTpcId);
  for (i=0; i<pidNeg.size(); i++) {
    StDedxPidTraits* pid = (StDedxPidTraits*) pidNeg[i];
    if (pid->method() == kTruncatedMeanId) {
      mDedxNeg = pid->mean();
      mErrDedxNeg = pid->errorOnMean();
      mNumDedxNeg = pid->numberOfPoints() + (100*((int) (pid->length())));
      break;
    }
  }
}

StV0MuDst::~StV0MuDst() {
}

Long_t StV0MuDst::detectorIdTrack(StTrackTopologyMap& map) {
  UInt_t tpcHits = map.numberOfHits(kTpcId);
  UInt_t svtHits = map.numberOfHits(kSvtId);
  UInt_t ssdHits = map.numberOfHits(kSvtId);
  if (( tpcHits)&&(!svtHits)&&(!ssdHits)) return kTpcId;
  if ((!tpcHits)&&( svtHits)&&(!ssdHits)) return kSvtId;
  if ((!tpcHits)&&(!svtHits)&&( ssdHits)) return kSsdId;
  if (( tpcHits)&&( svtHits)&&(!ssdHits)) return kTpcSvtId;
  if (( tpcHits)&&(!svtHits)&&( ssdHits)) return kTpcSsdId;
  if ((!tpcHits)&&( svtHits)&&( ssdHits)) return kSsdSvtId;
  if (( tpcHits)&&( svtHits)&&( ssdHits)) return kTpcSsdSvtId;
  return kUnknownId;
}

Long_t StV0MuDst::detectorIdV0() {
  return ((100*detectorIdTrack(mTopologyMapPos))+
               detectorIdTrack(mTopologyMapNeg));
}

Long_t StV0MuDst::detectorIdPars() {
  Long_t det_id_pos = detectorIdTrack(mTopologyMapPos);
  Long_t det_id_neg = detectorIdTrack(mTopologyMapNeg);
  if ((det_id_pos == kTpcId)   ||(det_id_neg == kTpcId)) return 1;
  if ((det_id_pos == kSvtId)   ||(det_id_neg == kSvtId)||
      (det_id_pos == kSsdId)   ||(det_id_neg == kSsdId)||
      (det_id_pos == kSsdSvtId)||(det_id_neg == kSsdSvtId)) return 2;
  return 3;
}

StTrackTopologyMap* gFakeTopoPtr = new StTrackTopologyMap();
